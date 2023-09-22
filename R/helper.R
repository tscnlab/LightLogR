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
