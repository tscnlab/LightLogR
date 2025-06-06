#' Create a gapless sequence of Datetimes
#'
#' Create a gapless sequence of Datetimes. The Datetimes are determined by the
#' minimum and maximum Datetime in the dataset and an epoch. The epoch can
#' either be guessed from the dataset or specified by the user.
#'
#' @inheritParams dominant_epoch
#' @param epoch The epoch to use for the gapless sequence. Can be either a
#'   `lubridate::duration()` or a string. If it is a string, it needs to be
#'   either '"dominant.epoch"' (the default) for a guess based on the data or a
#'   valid `lubridate::duration()` string, e.g., `"1 day"` or `"10 sec"`.
#' @param full.days If `TRUE`, the gapless sequence will include the whole first
#'   and last day where there is data.
#'
#' @return A `tibble` with a gapless sequence of `Datetime` as specified by
#'   `epoch`.
#' @export
#'
#' @family regularize
#' @examples
#'   dataset <-
#'   tibble::tibble(Id = c("A", "A", "A", "B", "B", "B"),
#'                  Datetime = lubridate::as_datetime(1) +
#'                  lubridate::days(c(0:2, 4, 6, 8))) %>%
#'                  dplyr::group_by(Id)
#'
#'   dataset %>% gapless_Datetimes()
#'   dataset %>% dplyr::ungroup() %>%  gapless_Datetimes()
#'   dataset %>% gapless_Datetimes(epoch = "1 day")

gapless_Datetimes <- function(dataset, 
                              Datetime.colname = Datetime,
                              epoch = "dominant.epoch",
                              full.days = FALSE) {
  
  # Initial Checks ----------------------------------------------------------
  
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "Datetime.colname must be part of the dataset" = 
      colname.defused({{ Datetime.colname }}) %in% names(dataset),
    "Datetime.colname must be a Datetime" = 
      lubridate::is.POSIXct(
        dataset[[colname.defused({{ Datetime.colname }})]]),
    "epoch must either be a duration or a string" = 
      lubridate::is.duration(epoch) | is.character(epoch),
    "full.days must be a logical" = is.logical(full.days)
  )
  
  # Function ----------------------------------------------------------
  
  #looking for n=1
  dataset2 <- 
  dataset |> dplyr::filter(dplyr::n() == 1)
  
  if(nrow(dataset2) > 0) {
    # message("One ore more groups have only a single observation. These cannot be automatically gap-handled.")
    # message(
    #   dplyr::group_keys(dataset2) |> 
    #   tidyr::unite(keys, dplyr::everything(), sep = " ") |> 
    #   dplyr::pull(keys) |> cat(sep ="\n")
    # )
    dataset <- dataset |> dplyr::filter(dplyr::n() != 1)
  }
  
  #get the epochs based on the data
  epochs <- epoch_list(dataset, Datetime.colname = {{ Datetime.colname }},
                       epoch = epoch)
  
  #create an expression for a sequence from the minimum datetime to the maximum
  #or, if full.days is TRUE, to the end of the day of the maximum datetime
  expr_standard <- rlang::expr(
    seq(
      from = min({{ Datetime.colname }}),
      to = 
      if(full.days) lubridate::ceiling_date(max({{ Datetime.colname }}), 
                                            unit = "day") - 1
      else max({{ Datetime.colname }}),
      by = 
        epochs %>%
        dplyr::filter(group.indices == Id2) %>%
        .[["dominant.epoch"]]
    )[-1]
  )
  
  #extend the above expression by the whole first day if full.days is TRUE
  expr_full_day <- rlang::expr(
    c(
      if(full.days){
        rev(
          seq(
            min({{ Datetime.colname }}),
            lubridate::floor_date(min({{ Datetime.colname }}), 
                                  unit = "day"),
            by = 
              epochs %>% 
              dplyr::filter(group.indices == Id2) %>% 
              .[["dominant.epoch"]] %>% {.*(-1)}
          )
        )  
      } else min({{ Datetime.colname}}),
      !!expr_standard
    )
  )
  
  #create the gapless sequence
  if(nrow(dataset) == 0) return(dataset) else {
  dat <- 
    dataset %>% 
    dplyr::reframe(Id2 = dplyr::cur_group_id(),
      {{ Datetime.colname }} := 
      !!expr_full_day
      ) |> 
    dplyr::select(-Id2) %>% 
    dplyr::group_by(
      dplyr::pick(
        dplyr::group_vars(dataset)
        )
      )
  }
  
  # Return ----------------------------------------------------------
  dat 
}

#' Fill implicit gaps in a light logger dataset
#'
#' Datasets from light loggers often have implicit gaps. These gaps are implicit
#' in the sense that consecutive timestamps (`Datetimes`) might not follow a
#' regular epoch/interval. This function fills these implicit gaps by creating a
#' gapless sequence of `Datetimes` and joining it to the dataset. The gapless
#' sequence is determined by the minimum and maximum `Datetime` in the dataset
#' (per group) and an epoch. The epoch can either be guessed from the dataset or
#' specified by the user. A sequence of gapless `Datetimes` can be created with
#' the [gapless_Datetimes()] function, whereas the dominant epoch in the data
#' can be checked with the [dominant_epoch()] function. The `behaviour` argument
#' specifies how the data is combined. By default, the data is joined with a
#' full join, which means that all rows from the gapless sequence are kept, even
#' if there is no matching row in the dataset.
#'
#' @inheritParams gapless_Datetimes
#' @param behavior The behavior of the join of the `dataset` with the `gapless`
#'   sequence. Can be one of `"full_sequence"` (the default), `"regulars"`,
#'   `"irregulars"`, or `"gaps"`. See @return for details.

#'
#' @return A modified `tibble` similar to `dataset` but with handling of implicit gaps, depending on the `behavior` argument:
#' * `"full_sequence"` adds timestamps to the `dataset` that are missing based on a full sequence of `Datetimes` (i.e., the gapless sequence). The `dataset` is this equal (no gaps) or greater in the number of rows than the input. One column is added. `is.implicit` indicates whether the row was added (`TRUE`) or not (`FALSE`). This helps differentiating measurement values from values that might be imputed later on.
#' * `"regulars"` keeps only rows from the gapless sequence that have a matching row in the dataset. This can be interpreted as a row-reduced `dataset` with only regular timestamps according to the `epoch`. In case of no gaps this tibble has the same number of rows as the input.
#' * `"irregulars"` keeps only rows from the `dataset` that do not follow the regular sequence of `Datetimes` according to the `epoch`. In case of no gaps this tibble has 0 rows.
#' * `"gaps"` returns a `tibble` of all implicit gaps in the dataset. In case of no gaps this tibble has 0 rows.
#'
#' @export
#' @family regularize
#'
#' @examples
#' dataset <-
#' tibble::tibble(Id = c("A", "A", "A", "B", "B", "B"),
#'               Datetime = lubridate::as_datetime(1) +
#'                          lubridate::days(c(0:2, 4, 6, 8)) +
#'                          lubridate::hours(c(0,12,rep(0,4)))) %>% 
#' dplyr::group_by(Id)
#' dataset
#' #assuming the epoch is 1 day, we can add implicit data to our dataset
#' dataset %>% gap_handler(epoch = "1 day")
#' 
#' #we can also check whether there are irregular Datetimes in our dataset
#' dataset %>% gap_handler(epoch = "1 day", behavior = "irregulars")
#' 
#' #to get to the gaps, we can use the "gaps" behavior
#' dataset %>% gap_handler(epoch = "1 day", behavior = "gaps")
#'  
#' #finally, we can also get just the regular Datetimes
#' dataset %>% gap_handler(epoch = "1 day", behavior = "regulars")

gap_handler <- function(dataset, 
                        Datetime.colname = Datetime,
                        epoch = "dominant.epoch",
                        behavior = c("full_sequence", "regulars", "irregulars", "gaps"),
                        full.days = FALSE) {
  
  # Initial Checks ----------------------------------------------------------
  
  #Argument matching
  behavior <- match.arg(behavior)
  
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "Datetime.colname must be part of the dataset" = 
      colname.defused({{ Datetime.colname }}) %in% names(dataset),
    "Datetime.colname must be a Datetime" = 
      lubridate::is.POSIXct(
        dataset[[colname.defused({{ Datetime.colname }})]]),
    "epoch must either be a duration or a string" = 
      lubridate::is.duration(epoch) | is.character(epoch),
    "full.days must be a logical" = is.logical(full.days)
  )
  
  # Function ----------------------------------------------------------
  
  #create the gapless sequence
  gapless <- 
    dataset %>% 
    gapless_Datetimes(
      Datetime.colname = {{ Datetime.colname }},
      epoch = epoch,
      full.days = full.days
      )
  
  #add a column to the dataset to indicate that the provided datetimes are explicit
  dataset <- 
    dataset %>% 
    dplyr::mutate(is.implicit = FALSE, .after = {{ Datetime.colname }})
  
  #collect the grouping variables and the Datetime column (needed for silent joining)
  by_vars <- 
    c(dplyr::group_vars(dataset), colname.defused({{ Datetime.colname }}))
  
  if(nrow(gapless) == 0) return(dataset)
  
  #join the datasets
  dat <- 
    switch(behavior,
           "full_sequence" = 
             dataset %>% dplyr::full_join(gapless, by = by_vars),
           "regulars" = dataset %>% dplyr::inner_join(gapless, by = by_vars),
           "irregulars" = dataset %>% dplyr::anti_join(gapless, by = by_vars),
           "gaps" = gapless %>% dplyr::anti_join(dataset, by = by_vars)
           )
  
  #cleanup
  
  if(behavior == "full_sequence") {
    dat <- dat %>% 
      dplyr::mutate(is.implicit = dplyr::case_when(
        is.na(is.implicit) ~ TRUE,
        .default = is.implicit
      )
      )
  }
  
  dat <- 
    dat %>% dplyr::arrange({{ Datetime.colname }}, .by_group = TRUE)
  
  # Return ----------------------------------------------------------
  dat
}

#' Check for and output gaps in a dataset
#'
#' Quickly check for implicit missing `Datetime` data. Outputs a message with a
#' short summary, and can optionally return the gaps as a `tibble`. Uses
#' `gap_handler()` internally.
#'
#' The `gap_finder()` function is a wrapper around `gap_handler()` with the
#' `behavior` argument set to `"gaps"`. The main difference is that
#' `gap_finder()` returns a message with a short summary of the gaps in the
#' dataset, and that the `tibble` with the gaps contains a column `gap.id` that
#' indicates the gap number, which is useful to determine, e.g., the consecutive
#' number of gaps between measurement data.
#'
#' @inheritParams gap_handler
#' @param gap.data Logical. If `TRUE`, returns a `tibble` of the gaps in the
#'   dataset. Default is `FALSE`.
#' @param silent Logical. If `TRUE`, suppresses the message with the summary of
#'  the gaps in the dataset. Default is `FALSE`. Only used for unit tests.
#'
#' @return Prints message with a short summary of the gaps in the dataset. If
#'   `gap.data = TRUE`, returns a `tibble` of the gaps in the dataset.
#' @export
#'
#' @family regularize
#' @examples
#' dataset <-
#' tibble::tibble(Id = c("A", "A", "A", "B", "B", "B"),
#'               Datetime = lubridate::as_datetime(1) +
#'                          lubridate::days(c(0:2, 4, 6, 8)) +
#'                          lubridate::hours(c(0,12,rep(0,4)))) %>%
#' dplyr::group_by(Id)
#' dataset
#'
#' #look for gaps assuming the epoch is the dominant epoch of each group
#' gap_finder(dataset)
#'
#' #return the gaps as a tibble
#' gap_finder(dataset, gap.data = TRUE)
#'
#' #assuming the epoch is 1 day, we have different gaps, and the datapoint at noon is now `irregular`
#' gap_finder(dataset, epoch = "1 day")

gap_finder <- function(dataset, 
                       Datetime.colname = Datetime,
                       epoch = "dominant.epoch",
                       gap.data = FALSE,
                       silent = FALSE,
                       full.days = FALSE) {
  
  # Initial Checks ----------------------------------------------------------
  
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "Datetime.colname must be part of the dataset" = 
      colname.defused({{ Datetime.colname }}) %in% names(dataset),
    "Datetime.colname must be a Datetime" = 
      lubridate::is.POSIXct(
        dataset[[colname.defused({{ Datetime.colname }})]]),
    "epoch must either be a duration or a string" = 
      lubridate::is.duration(epoch) | is.character(epoch),
    "gap.data must be logical" = is.logical(gap.data),
    "silent must be logical" = is.logical(silent),
    "full.days must be logical" = is.logical(full.days)
  )
  
  # Function ----------------------------------------------------------
  
  #create the full dataset including implicit values
  dat <- 
    dataset %>% gap_handler(
    Datetime.colname = {{ Datetime.colname }},
    epoch = epoch,
    full.days = full.days
    ) 
    
  dat_filtered <- 
    dat %>% 
    dplyr::filter(is.implicit) %>% 
    nrow()
  
  #give a message on the found gaps
  if(dat_filtered == 0 & !silent) {
    message("No gaps found")
  } else if(!silent) {
    
    #how many timestamps fall into the regular sequence
    dat2 <- 
      dataset %>% gap_handler(
        Datetime.colname = {{ Datetime.colname }},
        epoch = epoch,
        behavior = "regulars") %>% 
      nrow()
    
    message(
      "Found ", dat_filtered, " gaps. ", 
      dat2, " Datetimes fall into the regular sequence.")
  }
  
  if(gap.data) {
    dat %>% 
      dplyr::mutate(gap.id = dplyr::consecutive_id(is.implicit)) %>% 
      dplyr::filter(is.implicit) %>% 
      dplyr::mutate(gap.id = dplyr::consecutive_id(gap.id)) %>% 
      dplyr::select(gap.id, {{ Datetime.colname }}, dplyr::group_cols())
  }
  
}


#' Does a dataset have implicit gaps
#'
#' Returns `TRUE` if there are implicit gaps in the dataset and `FALSE` if it is
#' gapless. Gaps can make sense depending on the grouping structure, but the
#' general sequence of Datetimes within a dataset should always be gapless.
#'
#'
#' @inheritParams gap_handler
#'
#' @return logical
#' @export
#'
#' @family regularize
#' @examples
#' #The sample dataset does not have gaps
#' sample.data.environment |> has_gaps()
#' #removing some of the data creates gaps
#' sample.data.environment |> dplyr::filter(MEDI <= 50000) |> has_gaps()
#'
#' #having a grouped dataframe where the groups span multiple unconnected parts 
#' #is considered a gap, which can be relevant, e.g., when searching for clusters
#' sample.data.environment |>
#'   add_photoperiod(c(47.1, 10)) |>
#'   dplyr::group_by(photoperiod.state) |>
#'   has_gaps()
#'
#'  #to avoid this, use `number_states()` for grouping
#'  sample.data.environment |>
#'   add_photoperiod(c(48.52, 9.06)) |>
#'   number_states(photoperiod.state) |>
#'   dplyr::group_by(photoperiod.state.count, .add = TRUE) |>
#'   has_gaps()


has_gaps <-   function(dataset, 
                       Datetime.colname = Datetime,
                       epoch = "dominant.epoch") {
  
  # Initial Checks ----------------------------------------------------------
  
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "Datetime.colname must be part of the dataset" = 
      colname.defused({{ Datetime.colname }}) %in% names(dataset),
    "Datetime.colname must be a Datetime" = 
      lubridate::is.POSIXct(
        dataset[[colname.defused({{ Datetime.colname }})]]),
    "epoch must either be a duration or a string" = 
      lubridate::is.duration(epoch) | is.character(epoch)
  )
  
  # Function ----------------------------------------------------------
  
  #collect gap data
  dat <- 
    dataset %>% gap_handler(
      Datetime.colname = {{ Datetime.colname }},
      epoch = epoch,
      behavior = "gaps"
    ) 
  
  nrow(dat) > 0
  
}


#' Does a dataset have irregular data
#'
#' Returns `TRUE` if there are irregular data in the dataset and `FALSE` if not.
#' Irregular data can make sense if two datasets within a single group are
#' shifted to one another, e.g., if it contains data from two separate recording
#' sessions. The second session will be unlikely to have started at the exact
#' interval timing of the first session. While this is not problematic in itself, it is
#' still recommended to rectify the Datetimes to a common timestamp if time
#' precision permits it, e.g., through [aggregate_Datetime()] or
#' [cut_Datetime()].
#'
#'
#' @inheritParams gap_handler
#'
#' @return logical
#' @export
#'
#' @family regularize
#' @examples
#' #the sample dataset does not have any irregular data
#' sample.data.environment |> has_irregulars()
#' 
#' #even removing some data does not make it irregular, as all the Datetimes
#' #still fall in the regular interval
#' sample.data.environment |> dplyr::filter(MEDI <= 50000) |> has_irregulars()
#' 
#' #shifting some of the data will create irregular data
#' sample.data.environment |> 
#'   dplyr::mutate(
#'    Datetime = dplyr::if_else(
#'      sample(c(TRUE, FALSE), dplyr::n(), replace = TRUE), Datetime, Datetime + 1
#'      )
#'    ) |> 
#'    has_irregulars()

has_irregulars <-   function(dataset, 
                       Datetime.colname = Datetime,
                       epoch = "dominant.epoch") {
  
  # Initial Checks ----------------------------------------------------------
  
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "Datetime.colname must be part of the dataset" = 
      colname.defused({{ Datetime.colname }}) %in% names(dataset),
    "Datetime.colname must be a Datetime" = 
      lubridate::is.POSIXct(
        dataset[[colname.defused({{ Datetime.colname }})]]),
    "epoch must either be a duration or a string" = 
      lubridate::is.duration(epoch) | is.character(epoch)
  )
  
  # Function ----------------------------------------------------------
  
  #collect irregular data
  dat <- 
    dataset %>% gap_handler(
      Datetime.colname = {{ Datetime.colname }},
      epoch = epoch,
      behavior = "irregulars"
    ) 
  
  nrow(dat) > 0
  
}

#' Extract gap episodes from the data
#'
#' Finds and extracts gap episodes from a dataset. If no variable is provided,
#' it will look for implicit gaps (gaps in the regular interval), if a variable
#' is provided, it will look for implicit and explicit gaps (NA in the variable)
#'
#' @inheritParams gap_handler
#' @param Variable.colname Column name of the variable to check for NA values.
#'   Expects a symbol or NULL (only implicit gaps).
#' @param include.implicit.gaps Logical. Whether to expand the datetime sequence
#'   and search for implicit gaps, or not. Default is `TRUE`. If no
#'   `Variable.colname` is provided, this argument will be ignored. **If there
#'   are implicit gaps, gap calculation can be incorrect whenever there are
#'   missing explicit gaps flanking implicit gaps!**
#'
#' @return A dataframe containing gap times per grouping variable
#' @export
#'
#' @family regularize
#' @examples
#' #removing some data to create gaps
#' sample.data.environment |> 
#'  dplyr::filter(MEDI <= 50000) |> 
#'  extract_gaps() |> head()
#' 
#' #not searching for implicit gaps
#' sample.data.environment |> 
#'   dplyr::filter(MEDI <= 50000) |> 
#'  extract_gaps(MEDI, include.implicit.gaps = FALSE)
#' 
#' #making implicit gaps explicit changes the summary
#' sample.data.environment |> 
#'   dplyr::filter(MEDI <= 50000) |> 
#'   gap_handler()|> 
#'   extract_gaps(MEDI, include.implicit.gaps = FALSE) |> head()

extract_gaps <-   function(dataset, 
                           Variable.colname = NULL,
                           Datetime.colname = Datetime,
                           epoch = "dominant.epoch",
                           full.days = TRUE,
                           include.implicit.gaps = TRUE) {
  
  # Initial Checks ----------------------------------------------------------
  
  Variable.colname.defused <- rlang::enexpr(Variable.colname)
  
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "Datetime.colname must be part of the dataset" = 
      colname.defused({{ Datetime.colname }}) %in% names(dataset),
    "Datetime.colname must be a Datetime" = 
      lubridate::is.POSIXct(
        dataset[[colname.defused({{ Datetime.colname }})]]),
    "epoch must either be a duration or a string" = 
      lubridate::is.duration(epoch) | is.character(epoch)
  )
  
  if(!is.null(Variable.colname.defused)) {
    stopifnot(
      "Variable.colname must be part of the dataset" =
        colname.defused({{ Variable.colname }}) %in% names(dataset)
    )
  }
  
  # Function ----------------------------------------------------------

  #collect implicit gap data
  
  if(is.null(Variable.colname.defused)) {
  dat <- 
    dataset %>% gap_finder(
      Datetime.colname = {{ Datetime.colname }},
      epoch = epoch,
      gap.data = TRUE,
      full.days = full.days,
      silent = TRUE
    ) 
  
  if(nrow(dat) == 0) {
    message("No implicit gaps found")
    return(dat)
  }
  
  #get the epochs based on the data
  epochs <- epoch_list(dataset, Datetime.colname = {{ Datetime.colname }},
                       epoch = epoch)
  
  #add epoch data
  dat |> 
    dplyr::mutate(
      epoch = ifelse(
        epochs$dominant.epoch[epochs$group.indices == dplyr::cur_group_id()] |> length() == 0,
        NA,
        epochs$dominant.epoch[epochs$group.indices == dplyr::cur_group_id()]
      )
    ) |> 
    dplyr::group_by(gap.id, .add = TRUE) |> 
    dplyr::summarize(
      epoch = dplyr::first(epoch),
      start = min({{ Datetime.colname }}) - epoch/2,
      end = max({{ Datetime.colname }}) + epoch/2,
      duration = (end - start) |> lubridate::as.duration()
    )
  } else {
    dat <- 
      if(include.implicit.gaps) {
        dataset |> 
          gap_handler(full.days = full.days,
                      epoch = epoch,
                      Datetime.colname = {{ Datetime.colname }})
      } else {
        if(has_gaps(dataset, epoch = epoch)) warning("There are implicit gaps in the dataset that will not be part of the extracted summary, due to `include.implicit.gaps = FALSE`.", call. = FALSE)
        dataset
      }
    
    dat <- 
      dat |> 
      extract_states(gap, 
                     is.na({{ Variable.colname }}), 
                     epoch = epoch,
                     Datetime.colname = {{ Datetime.colname }}) |> 
      dplyr::filter(gap) |> 
      dplyr::ungroup(gap) |> 
      dplyr::mutate(gap.id = seq_along(gap), .after = gap) |> 
      dplyr::select(-state.count, -gap)
    
    if(nrow(dat) == 0) {
      message("No gaps found")
      return(dat)
    } else dat
    
  }
  
  
}

