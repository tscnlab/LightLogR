#' Add Brown et al. (2022) reference illuminance to a dataset
#'
#' Adds several columns to a light logger dataset. It requires column that
#' contains the Brown states, e.g. "daytime", "evening", and "night". From that
#' the function will add a column with the recommended illuminance, a column
#' that checks if the illuminance of the dataset is within the recommended
#' illuminance levels, and a column that gives a label to the reference.
#'
#' On a lower level, the function uses [Brown.rec()] and [Brown.check()] to
#' create the required information.
#'
#' @param dataset A dataframe that contains a column with the Brown states
#' @param MEDI.colname The name of the column that contains the MEDI values
#'   which are used for checks against the Brown reference illuminance. Must be
#'   part of the dataset.
#' @param Brown.state.colname The name of the column that contains the Brown
#'   states. Must be part of the dataset.
#' @param Brown.rec.colname The name of the column that will contain the
#'   recommended illuminance. Must not be part of the dataset, otherwise it will
#'   throw an error.
#' @param Reference.label.colname The name of the column that will contain the
#'   label for the reference.
#' @param Reference.label The label that will be used for the reference. Expects
#'   a `character` scalar.
#' @param Brown.check.colname The name of the column that will contain the check
#'   if the illuminance is within the recommended levels.
#' @param ... Additional arguments that will be passed to [Brown.rec()] and
#'   [Brown.check()]. This is only relevant to correct the names of the daytime
#'   states or the thresholds used within these states. See the documentation of
#'   these functions for more information.
#'
#' @references
#'   https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.3001571
#'
#' @return A dataframe on the basis of the `dataset` that contains the added
#'   columns.
#' @export
#'
#' @family Brown
#' @examples
#' #add Brown reference illuminance to some sample data
#' library(tibble)
#' testdata <- tibble(MEDI = c(100, 10, 1, 300),
#'                   State.Brown = c("day", "evening", "night", "day"))
#' Brown2reference(testdata)
#' 
Brown2reference <- function(dataset,
                            MEDI.colname = MEDI,
                            Brown.state.colname = State.Brown,
                            Brown.rec.colname = Reference,
                            Reference.label.colname = Reference.label,
                            Reference.label = "Brown et al. (2022)",
                            Brown.check.colname = Reference.check,
                            ...) {
  
  
  # Initial Checks ----------------------------------------------------------
  
  MEDI.colname.defused <- colname.defused({{ MEDI.colname }})
  Brown.state.colname.defused <- colname.defused({{ Brown.state.colname }})
  
  Brown.rec.colname.str <- colname.defused({{ Brown.rec.colname }})
  
  #give an error if the reference column is present
  if(Brown.rec.colname.str %in% names(dataset)) 
    stop("A Reference column with the given (or default) name is already part of the dataset. Please remove the column or choose a different name")
  
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "MEDI.colname must be part of the dataset" = 
      MEDI.colname.defused %in% names(dataset),
    "Brown.state.colname must be part of the dataset" = 
      Brown.state.colname.defused %in% names(dataset),
    "MEDI.colname must be a numeric column" = 
      is.numeric(dataset[[MEDI.colname.defused]])
  )

  #check whether the dataset has the right labels
  
  # Manipulation ----------------------------------------------------------

  #add a column with the reference illuminance
  dataset <- dataset %>% 
    dplyr::mutate(
      {{ Brown.rec.colname }} := 
        Brown.rec(state = {{ Brown.state.colname }},
                  ...)
      )
  #add a column with the checks
  dataset <- dataset %>% 
    dplyr::mutate(
      {{ Brown.check.colname }} := 
        Brown.check(
          value = {{ MEDI.colname }},
          state = {{ Brown.state.colname }},
          ...),
      {{ Reference.label.colname }} :=
        dplyr::if_else(!is.na({{ Brown.check.colname }}), Reference.label, NA)
    )
    
  dataset
}

#' Check whether a value is within the recommended illuminance/MEDI levels by Brown et al. (2022)
#' 
#' This is a lower level function. It checks a given value against a threshold for the states given by Brown et al. (2022). The function is vectorized. For `day` the threshold is a lower limit, for `evening` and `night` the threshold is an upper limit.
#'
#' @param value Illuminance value to check against the recommendation. needs to be numeric, can be a vector.
#' @param state The state from Brown et al. (2022). Needs to be a character vector with the same length as `value`.
#' @param Brown.day,Brown.evening,Brown.night The names of the states from Brown et al. (2022). These are the default values (`"day"`, `"evening"`, `"night"`), but can be changed if the names in `state` are different. Needs to be a character scalar.
#' @param Brown.day.th,Brown.evening.th,Brown.night.th The thresholds for the states from Brown et al. (2022). These are the default values (`250`, `10`, `1`), but can be changed if the thresholds should be different. Needs to be a numeric scalar.
#'
#' @return A logical vector with the same length as `value` that indicates whether the value is within the recommended illuminance levels.
#' @export
#' @references https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.3001571
#'
#' @family Brown
#' @examples
#' states <- c("day", "evening", "night", "day")
#' values <- c(100, 10, 1, 300)
#' Brown.check(values, states, Brown.day.th = 100)
#' 
Brown.check <- function(value,
                        state,
                        Brown.day = "day",
                        Brown.evening = "evening",
                        Brown.night = "night",
                        Brown.day.th = 250,
                        Brown.evening.th = 10,
                        Brown.night.th = 1) {
  
  stopifnot("Thresholds need to be numeric" = 
              is.numeric(
                c(Brown.day.th, Brown.evening.th, Brown.night.th
                )
              )
  )
  
  #check wheter state has the same length as value, give an error if not
  stopifnot(
    "state needs to be a character vector with the same length as value" = 
              is.character(state) & length(state) == length(value)
  )
  
  dplyr::case_when(
    ((state == Brown.day) & (value >= Brown.day.th)) ~ TRUE,
    ((state == Brown.day) & (value < Brown.day.th)) ~ FALSE,
    ((state == Brown.evening) & (value <= Brown.evening.th)) ~ TRUE,
    ((state == Brown.evening) & (value > Brown.evening.th)) ~ FALSE,
    ((state == Brown.night) & (value <= Brown.night.th)) ~ TRUE,
    ((state == Brown.night) & (value > Brown.night.th)) ~ FALSE,
    .default = NA
  )
}

#' Calculate the recommended illuminance/MEDI levels by Brown et al. (2022)
#'
#' This is a lower level function. It calculates the recommended illuminance/MEDI levels by Brown et al. (2022) for a given state. The function is vectorized.
#'
#' @inheritParams Brown.check
#' @param state The state from Brown et al. (2022). Needs to be a character vector.
#'
#' @return df A dataframe with the same length as `state` that contains the recommended illuminance/MEDI levels.
#' @export
#' 
#' @references https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.3001571
#'
#' @family Brown
#' @examples
#' states <- c("day", "evening", "night")
#' Brown.rec(states)
#' Brown.rec(states, Brown.day.th = 100)
#' 
Brown.rec <- function(state,
                      Brown.day = "day",
                      Brown.evening = "evening",
                      Brown.night = "night",
                      Brown.day.th = 250,
                      Brown.evening.th = 10,
                      Brown.night.th = 1){
  
  stopifnot("Thresholds need to be numeric" = 
              is.numeric(
                c(Brown.day.th, Brown.evening.th, Brown.night.th
                )
              )
  )
  
  dplyr::case_when(
    state == Brown.day ~ Brown.day.th,
    state == Brown.evening ~ Brown.evening.th,
    state == Brown.night ~ Brown.night.th
  )
}