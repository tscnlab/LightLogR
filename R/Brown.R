Brown2reference <- function(dataset,
                            MEDI.colname,
                            Brown.state.colname = State.Brown,
                            Brown.rec.colname = Reference,
                            Reference.label.colname = Reference.label,
                            Reference.label = "Brown et al. (2022)",
                            Brown.check.colname = Reference.check,
                            ...) {
  
  
  # Initial Checks ----------------------------------------------------------
  
  MEDI.colname.defused <- colname.defused({{ MEDI.colname }})
  Brown.state.colname.defused <- colname.defused({{ Brown.state.colname }})
  
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
    state == Brown.day & value >= Brown.day.th ~ TRUE,
    state == Brown.day & value < Brown.day.th ~ FALSE,
    state == Brown.evening & value <= Brown.evening.th ~ TRUE,
    state == Brown.evening & value > Brown.evening.th ~ FALSE,
    state == Brown.night & value <= Brown.night ~ TRUE,
    state == Brown.night & value > Brown.night ~ FALSE
  )
}

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