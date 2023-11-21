
#' Statechange (sc) Timestamps to Intervals
#'
#' Takes an input of `datetimes` and `Statechanges` and creates a column with
#' `Intervals`. If `full = TRUE`, it will also create intervals for the day
#' prior to the first state change and after the last. If `output.dataset =
#' FALSE` it will give a named vector, otherwise a `tibble`. The `state change`
#' info requires a description or name of the state (like `"sleep"` or `"wake"`,
#' or `"wear"`) that goes into effect at the given `Datetime`. Works for grouped
#' data so that it does not mix up intervals between participants. Missing data
#' should be explicit if at all possible. Also, the maximum allowed length of an
#' interval can be set, so that implicit missing timestamps after a set period
#' of times can be enforced.
#'
#' @inheritParams create_Timedata
#' @param Statechange.colname,Interval.colname,State.colname Column names that
#'   do contain the name/description of the `state change` and that will contain
#'   the `Interval` and `State` (which are also the default). Expects a `symbol`. The
#'   `Statechange` column needs do be part of the `dataset`.
#' @param full,starting.state These arguments handle the state on the first day
#'   before the first state change and after the last state change on the last
#'   day. If `full = TRUE`(the default, expects a `logical`), it will create an
#'   interval on the first day from 00:00:00 up until the state change. This
#'   interval will be given the state specified in `starting.state`, which is `NA`
#'   by default, but can be any `character` scalar. It will further extend the
#'   interval for the last state change until the end of the last given day
#'   (more specifically until 00:00:00 the next day).
#' @param length.restriction If the length between intervals is too great, the
#'   interval state can be set to `NA`, which effectively produces a gap in the
#'   data. This makes sense when intervals are implausibly wrong (e.g. someone
#'   slept for 50 hours), because when this data is combined with light logger
#'   data, e.g., through [interval2state()], metrics and visualizations will
#'   remove the interval.
#' @param Datetime.keep If `TRUE`, the original `Datetime` column will be kept.
#'
#' @return One of
#' * a `data.frame` object identical to `dataset` but with the interval instead of the datetime. The original `Statechange` column now indicates the `State` during the `Interval`.
#' * a named `vector` with the intervals, where the names are the states
#' @export
#'
#' @examples
#' library(tibble)
#' library(lubridate)
#' library(dplyr)
#' sample <- tibble::tibble(Datetime = c("2023-08-15 6:00:00",
#'                                       "2023-08-15 23:00:00",
#'                                       "2023-08-16 6:00:00",
#'                                       "2023-08-16 22:00:00",
#'                                       "2023-08-17 6:30:00",
#'                                       "2023-08-18 1:00:00"),
#'                          State = rep(c("wake", "sleep"), 3),
#'                          Id = "Participant")
#' #intervals from sample
#' sc2interval(sample)
#'
#' #compare sample (y) and intervals (x)
#' sc2interval(sample) %>%
#'  mutate(Datetime = int_start(Interval)) %>%
#'  dplyr::left_join(sample, by = c("Id", "State"),
#'                   relationship = "many-to-many") %>%
#'  head()
#' 
sc2interval <- function(dataset, 
                        Datetime.colname = Datetime,
                        Statechange.colname = State,
                        State.colname = State,
                        Interval.colname = Interval,
                        full = TRUE,
                        starting.state = NA,
                        output.dataset = TRUE,
                        Datetime.keep = FALSE,
                        length.restriction = 60*60*24) {

  # Initial Checks ----------------------------------------------------------
  Datetime.colname.defused <- colname.defused({{ Datetime.colname }})
  Statechange.colname.defused <- colname.defused({{ Statechange.colname }})
  
  stopifnot("Datetime.colname must be part of the dataset" = 
              Datetime.colname.defused %in% names(dataset))
  
  if(!lubridate::is.POSIXct(dataset[[Datetime.colname.defused]])) {
    dataset[[Datetime.colname.defused]] <- 
      dataset[[Datetime.colname.defused]] %>% lubridate::as_datetime()
  }
  
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "Statechange.colname must be part of the dataset" = 
      Statechange.colname.defused %in% names(dataset),
    "Datetime.colname must be a Datetime" = 
      lubridate::is.POSIXct(dataset[[Datetime.colname.defused]]),
    "output.dataset must be a logical" = is.logical(output.dataset),
    "full must be a logical" = is.logical(full),
    "length.restriction (in seconds) must be a numeric" = is.numeric(length.restriction)
  )
  
  # Manipulation ----------------------------------------------------------
  
  #add new datetimes, if full = TRUE to fill up the days
  if(full) {
    dataset <- 
      dataset %>% 
      dplyr::group_modify( ~ {
      dplyr::add_row(.x,
        {{ Datetime.colname }} := max(.[[Datetime.colname.defused]]) %>% 
          lubridate::ceiling_date(unit = "day"))
        }) %>% 
      dplyr::group_modify( ~ {
      dplyr::add_row(.x,
        {{ Statechange.colname }} := starting.state,
        {{ Datetime.colname }} := min(.[[Datetime.colname.defused]]) %>% 
          lubridate::floor_date(unit = "day"), .before = 1)
        })
  }
  
  #add the interval column and filter out intervals of unplausible length.
  #also filters out NA intervals
  dataset <- dataset %>% 
    dplyr::mutate(
      {{ Interval.colname }} := 
        lubridate::interval({{ Datetime.colname }}, dplyr::lead({{ Datetime.colname }})
        )
    ) %>% 
    dplyr::mutate(
      {{ State.colname }} := dplyr::case_when(
        lubridate::int_length({{ Interval.colname }}) <= 
          length.restriction ~ {{ Statechange.colname }},
        .default = NA
      )) %>% 
    dplyr::filter(!is.na({{ Interval.colname }}))
  
  if(!Datetime.keep) {
    dataset <- dataset %>% dplyr::select(-{{ Datetime.colname }})
  }
  
  # Return ----------------------------------------------------------
  if(output.dataset) dataset
  else {dataset[[colname.defused({{ Interval.colname }})]] %>% 
      rlang::set_names(dataset[[colname.defused({{ State.colname }})]])
  }
  
}


