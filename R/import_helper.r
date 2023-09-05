#This internal helper function is used for setup of imports of various device files
import.LL <- function(filename,
                     device,
                     import.expr,
                     file.id = FALSE,
                     n_max = Inf,
                     tz = "UTC") {
  
  import.expr <- rlang::enquo(import.expr)
  
  #initial checks
  stopifnot(
    is.character(filename),
    is.character(device),
    is.character(tz),
    is.logical(file.id),
    is.numeric(n_max)
  )
  #import the file
  tmp <- rlang::eval_tidy(import.expr)
  
  #validate the file
  
  #give info about the file
  import.info(tmp, device)
  
  #return the file
  tmp
}


#This internal helper function prints basic information about a dataset and is used for import function
import.info <- function(tmp, device) {
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
    "Successfully read in ", nrow(tmp), " observations from ", device, "-file", "\n",
    "Start: ", format(tmp$Datetime[1]), "\n",
    "End: ", format(max(tmp$Datetime)), "\n",
    "Timespan: " , diff(c(min.time, max.time)) %>% format(digits = 2), "\n",
    "Observation intervals: \n",
    sep = "")
  utils::capture.output(interval.time)[c(-1,-3)] %>% cat(sep = "\n")
}
