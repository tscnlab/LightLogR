#' Title
#'
#' @param dataset 
#' @param MEDI.colname 
#' @param Brown.state.colname 
#' @param Brown.rec.colname 
#' @param Reference.label.colname 
#' @param Reference.label 
#' @param Brown.check.colname 
#' @param ... 
#'
#' @return asdf
#' @export
#'
#' @examples
#' #sdf
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

#' Title
#'
#' @param value 
#' @param state 
#' @param Brown.day 
#' @param Brown.evening 
#' @param Brown.night 
#' @param Brown.day.th 
#' @param Brown.evening.th 
#' @param Brown.night.th 
#'
#' @return asdf
#' @export
#'
#' @examples
#' #daf
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

#' Title
#'
#' @param state 
#' @param Brown.day 
#' @param Brown.evening 
#' @param Brown.night 
#' @param Brown.day.th 
#' @param Brown.evening.th 
#' @param Brown.night.th 
#'
#' @return df
#' @export
#'
#' @examples
#' #asdf
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