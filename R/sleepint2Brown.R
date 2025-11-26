#' Recode Sleep/Wake intervals to Brown state intervals
#'
#' Takes a dataset with sleep/wake intervals and recodes them to Brown state
#' intervals. Specifically, it recodes the `sleep` intervals to `night`, reduces
#' `wake` intervals by a specified `evening.length` and recodes them to
#' `evening` and `day` intervals. The `evening.length` is the time between `day`
#' and `night`. The result can be used as input for [interval2state()] and might
#' be used subsequently with [Brown2reference()].
#'
#' The function will filter out any non-sleep intervals that are shorter than
#' the specified `evening.length`. This prevents problematic behaviour when the
#' `evening.length` is longer than the `wake` intervals or, e.g., when the first
#' state is sleep after midnight and there is a prior `NA` interval from
#' midnight till sleep. This behavior might, however, result in problematic
#' results for specialized experimental setups with ultra short wake/sleep
#' cycles. The `sleep_int2Brown()` function would not be applicable in those
#' cases anyways. Note that any column in the `dataset` outside of
#' `Interval.colname` and `Sleep.colname` will be filled up from previous states
#' (respecting groups).
#'
#' @param dataset A dataset with sleep/wake intervals.
#' @param Interval.colname The name of the column with the intervals. Defaults
#'   to `Interval`.
#' @param Sleep.colname The name of the column with the sleep/wake states.
#'   Defaults to `State`.
#' @param wake.state,sleep.state The names of the wake and sleep states in the
#'   `Sleep.colname`. Default to `"wake"` and `"sleep"`. Expected to be a
#'   `character` scalar and must be an exact match.
#' @param Brown.day,Brown.evening,Brown.night The names of the Brown states that
#'   will be used. Defaults to `"day"`, `"evening"` and `"night"`.
#' @param evening.length The length of the evening interval in seconds. Can also
#'   use \pkg{lubridate} duration or period objects. Defaults to 3 hours.
#' @param Brown.state.colname The name of the column with the newly created
#'   Brown states. Works as a simple renaming of the `Sleep.colname`.
#' @param output.dataset Whether to return the whole `dataset` or a `vector`
#'   with the Brown states.
#'
#' @return A dataset with the Brown states or a vector with the Brown states.
#'   The Brown states are created in a new column with the name specified in
#'   `Brown.state.colname`. The dataset will have more rows than the original
#'   dataset, because the `wake` intervals are split into `day` and `evening`
#'   intervals.
#' @export
#'
#' @references
#' https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.3001571
#' @family Brown
#'
#' @examples
#' #create a sample dataset
#' sample <- tibble::tibble(Datetime = c("2023-08-15 6:00:00",
#'                                          "2023-08-15 23:00:00",
#'                                          "2023-08-16 6:00:00",
#'                                          "2023-08-16 22:00:00",
#'                                          "2023-08-17 6:30:00",
#'                                          "2023-08-18 1:00:00"),
#'                          State = rep(c("wake", "sleep"), 3),
#'                          Id = "Participant")
#' #intervals from sample
#' sc2interval(sample)
#' #recoded intervals
#' sc2interval(sample) %>% sleep_int2Brown()
#'                                          
sleep_int2Brown <- function(dataset,
                           Interval.colname = Interval,
                           Sleep.colname = State,
                           wake.state = "wake",
                           sleep.state = "sleep",
                           Brown.day = "day",
                           Brown.evening = "evening",
                           Brown.night = "night",
                           evening.length = lubridate::dhours(3),
                           Brown.state.colname = State.Brown,
                           output.dataset = TRUE) {
  
  # Initial Checks ----------------------------------------------------------
  
  Interval.colname.defused <- colname.defused({{ Interval.colname }})
  Sleep.colname.defused <- colname.defused({{ Sleep.colname }})
  Brown.state.colname.defused <- colname.defused({{ Brown.state.colname }})
  
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "Interval.colname must be part of the dataset" = 
      Interval.colname.defused %in% names(dataset),
    "Sleep.colname must be part of the dataset" = 
      Sleep.colname.defused %in% names(dataset),
    "Interval.colname must be an Interval" = 
      lubridate::is.interval(dataset[[Interval.colname.defused]]),
    "Sleep/Wake and Brown states must be of type character" = is.character(
      c(wake.state, sleep.state, Brown.day, Brown.evening, Brown.night)
    ),
    "Sleep/Wake and Brown states must be scalars" = 
      is.all.scalar(
        wake.state, sleep.state, Brown.day, Brown.evening, Brown.night
    ),
    "output.dataset must be a logical" = is.logical(output.dataset)
  )
  
  # Function ----------------------------------------------------------
  
  #filter out intervals other than "sleep" and are shorter than the evening length
  dataset <-
    dataset %>%
    dplyr::filter(
      !({{ Sleep.colname }} != sleep.state &
       lubridate::int_length({{ Interval.colname }}) < evening.length)
    )
  
  #get intervals that are of type "sleep" and create start and endpoints for a new interval "evening"
  evening.data <- dataset %>% 
    dplyr::select({{ Sleep.colname }}, {{ Interval.colname }}) |> 
    dplyr::filter({{ Sleep.colname }} == sleep.state) %>%
    dplyr::mutate(End = lubridate::int_start({{ Interval.colname }}),
                  Start = End - evening.length,
                  {{ Sleep.colname }} := Brown.evening,
                  {{ Interval.colname }} := NA)
  
  #extend the original dataset with start and end columns based on the interval
  dataset <-
    dataset %>%
    dplyr::mutate(Start = lubridate::int_start({{ Interval.colname }}),
                  End = lubridate::int_end({{ Interval.colname }}))
  
  #create new intervals across all timestamps
  dataset <-
    dataset %>%
    {suppressMessages(dplyr::full_join(., evening.data))} %>%
    dplyr::arrange(Start, .by_group = TRUE) %>%
    dplyr::mutate(
      {{ Sleep.colname }} := 
        dplyr::case_match({{ Sleep.colname }},
        wake.state ~ Brown.day,
        sleep.state ~ Brown.night,
        .default = {{ Sleep.colname }}),
      Start = pmax(Start, dplyr::lag(Start, default = min(Start))),
      End = dplyr::lead(Start),
      {{ Interval.colname }} := lubridate::interval(Start, End)) %>%
    dplyr::select(-Start, -End) %>% 
    dplyr::rename({{ Brown.state.colname }} := {{ Sleep.colname.defused }}) |> 
    tidyr::fill(-c({{ Brown.state.colname }}, {{ Interval.colname }}))

  
  # Return ----------------------------------------------------------
  if(output.dataset) dataset
  else dataset[[colname.defused({{ Brown.state.colname }})]]
  
}

