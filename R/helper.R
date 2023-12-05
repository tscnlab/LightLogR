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
count.difftime <- function(dataset, Datetime.colname = Datetime) {
  dataset %>% 
    dplyr::mutate(
      difftime = c(NA, diff({{Datetime.colname}}) %>% lubridate::as.duration())
      ) %>% 
    tidyr::drop_na(difftime) %>% 
    dplyr::count(difftime, sort = TRUE)
}

#calculate the nth Quantile of time differences per group (in a grouped dataset)
nth.difftime <- function(dataset, Datetime.colname = Datetime, n = 0.95) {
  dataset %>% 
    dplyr::mutate(
      difftime = c(NA, diff({{Datetime.colname}}) %>% lubridate::as.duration())
      ) %>% 
    tidyr::drop_na(difftime) %>% 
    dplyr::summarise(
      Quant = stats::quantile(difftime, probs = n, na.rm = TRUE)
    )
}

#calculate the whether the nth quantile of time differences in one dataset is smaller or equal to the nth quantile of time differences in another dataset
compare.difftime <- function(dataset1, dataset2, Datetime.colname = Datetime, n = 0.95) {
  Quant1 <- nth.difftime(dataset1, {{ Datetime.colname }}, n = n)
  Quant2 <- nth.difftime(dataset2, {{ Datetime.colname }}, n = n)
  #do a full join with every column but Quantile
  group_variables <- setdiff(names(Quant2), "Quant")
  dplyr::full_join(Quant1, Quant2, by = group_variables) %>% 
  dplyr::mutate(
    comparison = Quant.x <= Quant.y
  )
}

#calculate whether any of the comparisons in compare.difftime is FALSE
compare.difftime.any <- function(...) {
  comparison <- compare.difftime(...) %>% 
    dplyr::filter(comparison == FALSE) %>% 
    dplyr::rename(Dataset.Interval = Quant.x,
                 Reference.Interval = Quant.y) %>% 
    dplyr::select(-comparison)
  
  if(nrow(comparison) > 0) comparison else TRUE
}

#create a reference label
create.Reference.label <- function(dataset, 
                                   Reference.column, 
                                   Reference.label = NULL) {
  if(!is.null(Reference.label)) {
    Reference.column.str <- colname.defused({{ Reference.column }})
    Reference.label.column.str <- paste0(Reference.column.str, ".label")
    
    dataset <- 
      dataset %>% 
      dplyr::mutate(!!Reference.label.column.str := 
                      dplyr::if_else(
                        !is.na({{ Reference.column }}), Reference.label, NA
                        )
      )
    dataset
    } else dataset
}

#helper to pick the colums that are used for grouping
pick.grouping.columns <- function(dataset) {
  dplyr::pick(
    dplyr::group_vars(dataset)
  )
}

# Compare with threshold
compare_threshold <- function(Light.vector,
                      threshold,
                      comparison = c("above", "below"),
                      na.replace = FALSE){
  
  comparison = match.arg(comparison)
  
  stopifnot(
    "`Light.vector` must be numeric!" = is.numeric(Light.vector),
    "`threshold` must be numeric!" = is.numeric(threshold),
    "`threshold` must be either one or two values!" = length(threshold) %in% c(1, 2),
    "`na.replace` must be logical!" = is.logical(na.replace)
  )
  
  if(length(threshold) == 1){
    out <- switch(comparison,
                  "above" = Light.vector >= threshold,
                  "below" = Light.vector <= threshold)
  }
  else{
    threshold <- sort(threshold)
    out <- Light.vector >= threshold[1] & Light.vector <= threshold[2]
  }
  
  if(na.replace){
    out <- tidyr::replace_na(out, FALSE)
  }
  
  return(out)
}