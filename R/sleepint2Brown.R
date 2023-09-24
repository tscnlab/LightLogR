# sleepdata <- tibble::tibble(Datetime = rep(c("2023-08-15 6:00:00",
#                                              "2023-08-15 23:00:00",
#                                              "2023-08-16 6:00:00",
#                                              "2023-08-16 22:00:00",
#                                              "2023-08-17 6:30:00",
#                                              "2023-08-18 1:00:00",
#                                              "2023-08-18 6:00:00",
#                                              "2023-08-19 0:30:00",
#                                              "2023-08-19 6:00:00",
#                                              "2023-08-20 1:30:00",
#                                              "2023-08-20 6:15:00",
#                                              "2023-08-20 23:23:00"
# ), 2) , 
# status = rep(c("wake", "sleep"), 12),
# Id = rep(c("Environment", "Participant"), each = 12))


# sample.data.environment %>%
#   interval2state(
#     State.interval.dataset =
#       {interval.sleepdata %>% sleep.int2Brown(Sleep.colname = status)},
#     ID.colname.dataset = Source) %>%
#   Brown2reference(MEDI.colname = `MELANOPIC EDI`) -> test


#' Title
#'
#' @param dataset 
#' @param Interval.colname 
#' @param Sleep.colname 
#' @param wake.state 
#' @param sleep.state 
#' @param Brown.day 
#' @param Brown.evening 
#' @param Brown.night 
#' @param evening.length 
#' @param Brown.state.colname 
#' @param output.dataset 
#'
#' @return ds
#' @export
#'
#' @examples
#' #jf
sleep.int2Brown <- function(dataset,
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
    "output.dataset must be a logical" = is.logical(output.dataset)
  )
  
  # Manipulation ----------------------------------------------------------
  
  #get intervals that are of type "sleep" and create start and endpoints for a new interval "evening"
  evening.data <- dataset %>%
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
    dplyr::full_join(evening.data) %>%
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
    dplyr::rename({{ Brown.state.colname }} := {{ Sleep.colname.defused }})
  
  # Return ----------------------------------------------------------
  if(output.dataset) dataset
  else dataset[[colname.defused({{ Brown.state.colname }})]]
  
}

