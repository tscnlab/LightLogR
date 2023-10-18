#This internal helper function is used for setup of imports of various device files
import.LL <- function(filename,
                     device = "none",
                     import.expr,
                     n_max = Inf,
                     tz = "UTC",
                     ID.colname = Id,
                     auto.id = ".*",
                     manual.id = NULL) {
  
  id.colname.defused <- colname.defused(id)
  tz <- tz
  
  #initial checks
  stopifnot(
    "filename needs to be a character (vector)" = is.character(filename),
    "device needs to be a character" = is.character(device),
    "tz needs to be a character" = is.character(tz),
    "tz needs to be a valid time zone, see `OlsonNames()`" = tz %in% OlsonNames(),
    "auto.id needs to be a string" = is.character(auto.id),
    "n_max needs to be a positive numeric" = is.numeric(n_max)
  )
  #import the file
  tmp <- rlang::eval_tidy(import.expr)
  
  #validate/manipulate the file
  if(!id.colname.defused %in% names(tmp)) {
    switch(is.null(manual.id) %>% as.character(),
           "TRUE" =
    {tmp <- tmp %>% 
      dplyr::mutate({{ ID.colname }} := 
                      basename(file.name) %>% 
                      tools::file_path_sans_ext() %>% 
                      stringr::str_extract(auto.id),
                    .before = 1)},
    "FALSE" =
      {tmp <- tmp %>% 
      dplyr::mutate({{ ID.colname }} := manual.id, .before = 1)}
    )
  }
  tmp <- tmp %>% 
    dplyr::mutate(file.name = basename(file.name) %>% 
                    tools::file_path_sans_ext(),
                  {{ ID.colname }} := factor({{ ID.colname }})) %>% 
    dplyr::group_by({{ ID.colname }}) %>% 
    dplyr::arrange(Datetime, .by_group = TRUE)
  
  #give info about the file
  import.info(tmp, device, tz, {{ ID.colname }})
  
  #return the file
  tmp
}


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

#This internal helper functions provides a link from the specific import function to the generic import function

import.link <- function(device, ID.colname) {
  
  env <- parent.frame()
  filename <- env$filename
  tz<- env$tz
  auto.id<- env$auto.id
  manual.id <- env$manual.id
  n_max<- env$n_max
  path<- env$path
  import.expr <- env$import.expr
  
  #generic import function
  import.LL(filename = filename,
            device = device,
            import.expr = import.expr,
            n_max = n_max,
            tz = tz,
            manual.id = manual.id,
            ID.colname = {{ ID.colname }},
            auto.id = auto.id)
}