
#' Adds a state column to a dataset from interval data
#' 
#' This function can make use of `Interval` data that contain `States` (like `"sleep"`, `"wake"`, `"wear"`) and add a column to a light logger `dataset`, where the `State` of  every `Datetime` is specified, based on the participant's `ID`.
#'
#' @inheritParams sc2interval
#' @param State.interval.dataset Name of the dataset that contains `State` and `Interval` columns. Sensibly, it is created through [sc2interval()].
#' @param State.colname,Interval.colname  Column names of the `State` and `Interval` in the `State.interval.dataset`. Expects a `symbol`.
#' @param ID.colname.dataset,ID.colname.interval Column names of the participant's `Id` in both the `dataset` and the `State.interval.dataset`. On the off-chance that there are inconsistencies, the names can be different. If the datasets where imported and preprocessed with [LightLogR], this just works. Both datasets need an `Id`, because the states will be added based not only on the `Datetime`, but also depending on the dataset. 
#'
#' @return One of
#' * a `data.frame` object identical to `dataset` but with the state column added
#' * a `vector` with the states
#' @export
#'
#' @examples
#' #example
interval2state <- function(dataset,
                           Datetime.colname = Datetime,
                           State.interval.dataset,
                           State.colname = State,
                           Interval.colname = Interval,
                           ID.colname.dataset = Id,
                           ID.colname.interval = Id,
                           output.dataset = TRUE) {
  
  # Initial Checks ----------------------------------------------------------
  Datetime.colname.defused <- colname.defused({{ Datetime.colname }})
  State.colname.defused <- colname.defused({{ State.colname }})
  Interval.colname.defused <- colname.defused({{ Interval.colname }})
  ID.colname.dataset.defused <- colname.defused({{ ID.colname.dataset }})
  ID.colname.interval.defused <- colname.defused({{ ID.colname.interval }})
  
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "State.interval.dataset is not a dataframe" = 
      is.data.frame(State.interval.dataset),
    "State.colname must be part of the State.interval.dataset" = 
      State.colname.defused %in% names(State.interval.dataset),
    "Interval.colname must be part of the State.interval.dataset" = 
      Interval.colname.defused %in% names(State.interval.dataset),
    "ID.colname.interval must be part of the State.interval.dataset" = 
      ID.colname.interval.defused %in% names(State.interval.dataset),
    "ID.colname.dataset must be part of the dataset" = 
      ID.colname.dataset.defused %in% names(dataset),
    "Datetime.colname must be part of the dataset" = 
      Datetime.colname.defused %in% names(dataset),
    "Datetime.colname must be a Datetime" = 
      lubridate::is.POSIXct(dataset[[Datetime.colname.defused]]),
    "output.dataset must be a logical" = is.logical(output.dataset)
  )
  # Manipulation ----------------------------------------------------------
  
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
  
  # splice the two datasets together
  dataset <- 
    dataset %>% 
    dplyr::full_join(
      State.interval.dataset2,
      by = dplyr::join_by(
        {{ ID.colname.dataset }} == {{ ID.colname.interval }},
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