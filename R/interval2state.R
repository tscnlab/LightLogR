#' Adds a state column to a dataset from interval data
#'
#' This function can make use of `Interval` data that contain `States` (like
#' `"sleep"`, `"wake"`, `"wear"`) and add a column to a light logger `dataset`,
#' where the `State` of  every `Datetime` is specified, based on the
#' participant's `Id`.
#'
#' @inheritParams sc2interval
#' @param State.interval.dataset Name of the dataset that contains `State` and
#'   `Interval` columns. Interval data can be created, e.g., through
#'   [sc2interval()].
#' @param State.colname,Interval.colname  Column names of the `State` and
#'   `Interval` in the `State.interval.dataset`. Expects a `symbol`. `State`
#'   can't be in the `dataset` yet or the function will give an error. You can
#'   also set `overwrite = TRUE`.
#' @param Id.colname.dataset,Id.colname.interval Column names of the
#'   participant's `Id` in both the `dataset` and the `State.interval.dataset`.
#'   On the off-chance that there are inconsistencies, the names can be
#'   different. If the datasets where imported and preprocessed with
#'   [LightLogR], this just works. Both datasets need an `Id`, because the
#'   states will be added based not only on the `Datetime`, but also depending
#'   on the dataset.
#' @param overwrite If `TRUE` (defaults to `FALSE`), the function will
#'   overwrite the `State.colname` column if it already exists.
#'
#' @return One of
#' * a `data.frame` object identical to `dataset` but with the state column added
#' * a `vector` with the states
#' @export
#'
#' @examples
#' #create a interval dataset
#' library(tibble)
#' library(dplyr)
#' library(lubridate)
#' states <- tibble::tibble(Datetime = c("2023-08-15 6:00:00",
#'                                       "2023-08-15 23:00:00",
#'                                       "2023-08-16 6:00:00",
#'                                       "2023-08-16 22:00:00",
#'                                       "2023-08-17 6:30:00",
#'                                       "2023-08-18 1:00:00",
#'                                       "2023-08-18 6:00:00",
#'                                       "2023-08-18 22:00:00",
#'                                       "2023-08-19 6:00:00",
#'                                       "2023-08-19 23:00:00",
#'                                       "2023-08-20 6:00:00",
#'                                       "2023-08-20 22:00:00"),
#'                          State = rep(c("wake", "sleep"), 6),
#'                          Id = "Participant")
#' intervals <- sc2interval(states)
#'
#' #create a dataset with states
#' dataset_with_states <-
#' sample.data.environment %>%
#' interval2state(State.interval.dataset = intervals)
#'
#' #visualize the states - note that the states are only added to the respective ID in the dataset?
#' library(ggplot2)
#' ggplot(dataset_with_states, aes(x = Datetime, y = MEDI, color = State)) +
#'  geom_point() +
#'  facet_wrap(~Id, ncol = 1)
#'
#' 
interval2state <- function(dataset,
                           State.interval.dataset,
                           Datetime.colname = Datetime,
                           State.colname = State,
                           Interval.colname = Interval,
                           Id.colname.dataset = Id,
                           Id.colname.interval = Id,
                           overwrite = FALSE,
                           output.dataset = TRUE) {
  
  # Initial Checks ----------------------------------------------------------
  Datetime.colname.defused <- colname.defused({{ Datetime.colname }})
  State.colname.defused <- colname.defused({{ State.colname }})
  Interval.colname.defused <- colname.defused({{ Interval.colname }})
  Id.colname.dataset.defused <- colname.defused({{ Id.colname.dataset }})
  Id.colname.interval.defused <- colname.defused({{ Id.colname.interval }})
  
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "State.interval.dataset is not a dataframe" = 
      is.data.frame(State.interval.dataset),
    "State.colname must be part of the State.interval.dataset" = 
      State.colname.defused %in% names(State.interval.dataset),
    "Interval.colname must be part of the State.interval.dataset" = 
      Interval.colname.defused %in% names(State.interval.dataset),
    "Id.colname.interval must be part of the State.interval.dataset" = 
      Id.colname.interval.defused %in% names(State.interval.dataset),
    "Id.colname.dataset must be part of the dataset" = 
      Id.colname.dataset.defused %in% names(dataset),
    "Datetime.colname must be part of the dataset" = 
      Datetime.colname.defused %in% names(dataset),
    "Datetime.colname must be a Datetime" = 
      lubridate::is.POSIXct(dataset[[Datetime.colname.defused]]),
    "output.dataset must be a logical" = is.logical(output.dataset)
  )
  
  #give an error or warning if the reference column is present
  if(all(
    State.colname.defused %in% names(dataset),
    !overwrite,
    State.colname.defused %in% names(State.interval.dataset)))
    stop("A `State` column with the given (or default) name is already part of the dataset. Please remove the column, rename it, or set `overwrite = TRUE`")
    if(all(
      State.colname.defused %in% names(dataset),
      State.colname.defused %in% names(State.interval.dataset))) 
      warning("A `State` column with the given (or default) name is already part of the dataset. It is overwritten, because `overwrite = TRUE ` was set.")
  
  
  # Manipulation ----------------------------------------------------------
  
  #remove the State.colname from the dataset if it is already present
  if(State.colname.defused %in% names(dataset)) {
    dataset <- dataset %>% dplyr::select(- {{ State.colname }})
  }
  
  #add a marker to the dataset that tells us, what the original Timepoints were
  dataset <- 
    dataset %>% 
    tibble::rownames_to_column(var = "original.datapoints.fleeting") %>% 
    dplyr::mutate(original.datapoints.fleeting = 
                    as.numeric(original.datapoints.fleeting))
  
  # create a new dataset from the State.interval.dataset with start times
  State.character <-
    State.interval.dataset[[{{ State.colname.defused }}]] %>% is.character()
  
  State.interval.dataset2 <-
    State.interval.dataset %>%
    dplyr::mutate({{ Datetime.colname }} := lubridate::int_start(
      {{ Interval.colname }})) %>%
    dplyr::select(- {{ Interval.colname }})
  
  if(State.character) {
  State.interval.dataset2 <-
    State.interval.dataset2 %>% 
    dplyr::mutate({{ State.colname }} := 
                    dplyr::case_when(
                      is.na({{ State.colname }}) ~ "NaN",
                      .default = {{ State.colname }}
                    ))
  }
  else{
    State.interval.dataset2 <-
      State.interval.dataset2 %>% 
      dplyr::mutate({{ State.colname }} := 
                      dplyr::case_when(
                        is.na({{ State.colname }}) ~ NaN,
                        .default = {{ State.colname }}
                      ))
  }
  
  #test whether the time differences in the two datasets are acceptable
  are.intervals.smaller <- 
    compare.difftime.any(dataset, State.interval.dataset2)
  
  if(!rlang::is_true(are.intervals.smaller)) {
    cat("Warning: The time differences between consecutive time points in the reference dataset are larger than in the dataset. This means multiple reference data connect to one dataset datum - only the last one prior to each datum will be used. Please use an aggregate function on the reference dataset to resolve this warning. \nThe following output shows what grouping is problematic and what 95% of time intervals in the Dataset compared to the Reference data is.\n\n")
    utils::capture.output(are.intervals.smaller)[c(-1,-3)] %>% cat(sep = "\n")
  }
  
  # join the two datasets together
  dataset <- 
    dataset %>% 
    dplyr::full_join(
      State.interval.dataset2,
      by = dplyr::join_by(
        {{ Id.colname.dataset }} == {{ Id.colname.interval }},
        {{ Datetime.colname }})
    ) %>% 
    dplyr::arrange({{ Datetime.colname }}, .by_group = TRUE)

  # fill downwards
  if(State.character) {
  dataset <-
    dataset %>%
    dplyr::mutate(
      group = cumsum(!is.na({{ State.colname }}) | {{ State.colname }} == "NaN")
      ) %>% 
    dplyr::group_by(group, .add = TRUE) %>% 
    tidyr::fill({{ State.colname }}) %>% 
    dplyr::ungroup(group) %>% 
    dplyr::mutate({{ State.colname }} := 
                    dplyr::na_if({{ State.colname }}, "NaN"))
  }
  else {
    dataset <-
      dataset %>%
      dplyr::mutate(
        group = cumsum(!is.na({{ State.colname }}) | is.nan({{ State.colname }}))
      ) %>% 
      dplyr::group_by(group, .add = TRUE) %>% 
      tidyr::fill({{ State.colname }}) %>% 
      dplyr::ungroup(group) %>% 
      dplyr::mutate({{ State.colname }} := 
                      dplyr::na_if({{ State.colname }}, NaN))
  }
  
  # remove non-data timestamps
  dataset <- 
    dataset %>% 
    dplyr::filter(!is.na(original.datapoints.fleeting)) %>% 
    dplyr::arrange(original.datapoints.fleeting) %>% 
    dplyr::select(-original.datapoints.fleeting, -group)
  
  # Return ----------------------------------------------------------
  if(output.dataset) dataset
  else dataset[[colname.defused({{ State.colname }})]]
  
}
