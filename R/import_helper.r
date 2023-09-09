#This internal helper function is used for setup of imports of various device files
import.LL <- function(filename,
                     device,
                     import.expr,
                     file.id = FALSE,
                     n_max = Inf,
                     tz = "UTC") {
  
  import.expr <- rlang::enquo(import.expr)
  tz <- tz
  
  #initial checks
  stopifnot(
    "filename needs to be a character (vector)" = is.character(filename),
    "device needs to be a character" = is.character(device),
    "tz needs to be a character" = is.character(tz),
    "tz needs to be a valid time zone, see `OlsonNames()`" = tz %in% OlsonNames(),
    "file.id needs to be a logical" = is.logical(file.id),
    "n_max needs to be a positive numeric" = is.numeric(n_max)
  )
  #import the file
  tmp <- rlang::eval_tidy(import.expr)
  
  #validate the file
  
  #give info about the file
  import.info(tmp, device, tz)
  
  #return the file
  tmp
}


#This internal helper function prints basic information about a dataset and is used for import function
import.info <- function(tmp, device, tz) {
  #give info about the file
  
  min.time <- min(tmp$Datetime)
  max.time <- max(tmp$Datetime)
  interval.time <- 
    tmp %>% 
    dplyr::reframe(
      interval.time = diff(Datetime)
      ) %>% 
    dplyr::count(interval.time) %>% 
    dplyr::mutate(pct = (n/sum(n)) %>% scales::percent())

    cat(
    "Successfully read in ", nrow(tmp), " observations from ", device, "-file", 
    "\n",
    "Timezone set is ", tz, ".\n", 
    if(lubridate::tz(tmp$Datetime) != Sys.timezone()) {
      paste0(
        "The system timezone is ",
        Sys.timezone(),
        ". Please correct if necessary!\n")},
    "Start: ", format(tmp$Datetime[1]), "\n",
    "End: ", format(max(tmp$Datetime)), "\n",
    "Timespan: " , diff(c(min.time, max.time)) %>% format(digits = 2), "\n",
    "Observation intervals: \n",
    sep = "")
  utils::capture.output(interval.time)[c(-1,-3)] %>% cat(sep = "\n")
}

