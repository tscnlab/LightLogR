#tests whether the given input is a POSIXct, hms, or string of correct input
test.Time.regex <- function(input) {
  
  if(!is.null(input)){
    
    input.defused <- rlang::enexpr(input)
    
    test <- dplyr::case_when(
      lubridate::is.POSIXct(input) ~ TRUE,
      hms::is_hms(input) ~ TRUE,
      stringr::str_detect(
        input, 
        pattern = "^([01]?[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$") ~ TRUE,
      .default = FALSE
    )
    
    if(!test){
      stop(paste("input:",
        rlang::as_string(
          input.defused), 
        "needs to be in the format 'hh:mm:ss'"))
    }
  }
}

#extracts a symbol and gives it back as a string when necessary
colname.defused <- function(Colname, as_string = TRUE) {
  Colname <- rlang::ensym(Colname) 
  if(as_string) {
    rlang::as_string(Colname)
  }
  else Colname
}

#tests whether the inputs are all scalar
is.all.scalar <- function(...) {
  list(...) %>% 
    purrr::every(\(x) length(x) == 1)
}

#counts the different time differences per group (in a grouped dataset)
count.difftime <- function(dataset, Datetime.column = Datetime) {
  dataset %>% 
    dplyr::mutate(
      difftime = c(NA, diff({{Datetime.column}}) %>% lubridate::as.duration())
      ) %>% 
    tidyr::drop_na(difftime) %>% 
    dplyr::count(difftime, sort = TRUE)
}

#calculate the nth percentile of time differences per group (in a grouped dataset)
nth.difftime <- function(dataset, Datetime.column = Datetime, n = 0.95) {
  dataset %>% 
    dplyr::mutate(
      difftime = c(NA, diff({{Datetime.column}}) %>% lubridate::as.duration())
      ) %>% 
    tidyr::drop_na(difftime) %>% 
    dplyr::summarise(
      percentile = quantile(difftime, probs = n, na.rm = TRUE)
    )
}

#calculate the whether the nth percentile of time differences in one dataset is smaller or equal to the nth percentile of time differences in another dataset
compare.difftime <- function(dataset1, dataset2, Datetime.column = Datetime, n = 0.95) {
  percentile1 <- nth.difftime(dataset1, {{ Datetime.column }}, n = n)
  percentile2 <- nth.difftime(dataset2, {{ Datetime.column }}, n = n)
  #do a full join with every column but percentile
  group_variables <- setdiff(names(percentile2), "percentile")
  dplyr::full_join(percentile1, percentile2, by = group_variables) %>% 
  dplyr::mutate(
    comparison = percentile.x <= percentile.y
  )
}

#calculate whether any of the comparisons in compare.difftime is FALSE
compare.difftime.any <- function(...) {
  comparison <- compare.difftime(...) %>% 
    dplyr::filter(comparison == FALSE) %>% 
    dplyr::rename(Dataset.Interval = percentile.x,
                 Reference.Interval = percentile.y) %>% 
    dplyr::select(-comparison)
  
  if(nrow(comparison) > 0) comparison else TRUE
}
