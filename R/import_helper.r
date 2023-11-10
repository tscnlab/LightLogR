#This internal helper function prints basic information about a dataset and is used for import function
import.info <- function(tmp, device, tz, ID.colname) {
  #give info about the file
  
  
  min.time <- min(tmp$Datetime)
  max.time <- max(tmp$Datetime)
  interval.time <- 
    tmp %>% 
    dplyr::reframe(
      interval.time = diff(Datetime)
      ) %>% 
    dplyr::group_by({{ ID.colname }}) %>%
    dplyr::count(interval.time) %>% 
    dplyr::mutate(pct = (n/sum(n)) %>% scales::percent(),
                  interval.time = interval.time %>% lubridate::as.duration())

    cat(
    "Successfully read in ", nrow(tmp), " observations from ", device, "-file", 
    "\n",
    "Timezone set is ", tz, ".\n", 
    if(lubridate::tz(tmp$Datetime) != Sys.timezone()) {
      paste0(
        "The system timezone is ",
        Sys.timezone(),
        ". Please correct if necessary!\n")},
    "Start: ", format(min.time), "\n",
    "End: ", format(max.time), "\n",
    "Timespan: " , diff(c(min.time, max.time)) %>% format(digits = 2), "\n",
    "Observation intervals: \n",
    sep = "")
  utils::capture.output(interval.time)[c(-1,-2,-4)] %>% cat(sep = "\n")
}

#This internal helper function looks for the starting row of an import file based on a vector of column names in order.
detect_starting_row <- 
  function(filepath,
           locale = readr::default_locale(),
           column_names,
           n_max = 250) {
    
  #make a regex pattern from the column names
  column_names <- 
    column_names %>% 
    stringr::str_flatten(collapse = ".*")
  
  #read in all the lines and remove junk
  line_read <- 
    readr::read_lines(filepath, n_max = n_max, locale=locale)
  
  #find the row where the column names are
  which_lines <- 
  line_read %>% 
    purrr::map_vec(
      \(x) stringr::str_detect(x,column_names)
      ) %>% which()
  
  #if there is no line with the column names, return an error
  if(length(which_lines) == 0) {
    stop("Could not find a line with this order of column names in the file. Please check the correct order and spelling of the given columns.")
  }
  
  #if there is more than one line with the column names, return an error
  if(length(which_lines) > 1) {
    stop(paste("Found", length(which_lines), "lines with the given column names, but require exactly 1. Please provide a more specific pattern."))
  }
  
  #if there is only one line with the column names, return the line number
  #and reduce it by one to get the lines to skip
  if(length(which_lines) == 1) {
    return(which_lines-1)
  }
  
}