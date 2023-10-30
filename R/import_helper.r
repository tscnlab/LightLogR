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
    "Start: ", format(tmp$Datetime[1]), "\n",
    "End: ", format(max(tmp$Datetime)), "\n",
    "Timespan: " , diff(c(min.time, max.time)) %>% format(digits = 2), "\n",
    "Observation intervals: \n",
    sep = "")
  utils::capture.output(interval.time)[c(-1,-2,-4)] %>% cat(sep = "\n")
}