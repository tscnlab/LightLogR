#This internal helper function prints basic information about a dataset and is used for import function
import.info <- function(data, 
                        device, 
                        tz, 
                        Id.colname, 
                        dst_adjustment,
                        dst_info = TRUE,
                        filename,
                        na.count,
                        print_n = 10,
                        not.before) {
  #give info about the file
  min.time <- min(data$Datetime)
  max.time <- max(data$Datetime)
  interval.time <- 
    data %>% 
    dplyr::reframe(
      interval.time = diff(Datetime)
      ) %>% 
    dplyr::group_by({{ Id.colname }}) %>%
    dplyr::count(interval.time) %>% 
    dplyr::mutate(pct = (n/sum(n)) %>% scales::percent(),
                  interval.time = interval.time %>% lubridate::as.duration())

  #number of Ids
  n_ids <- data %>% dplyr::group_keys() %>% nrow()
  #number of files 
  n_files <- filename %>% unique() %>% length()
  
  #check for dst_adjustment
  if(dst_info) {
    dst_info <- 
      data %>% dplyr::group_by(file.name, .add = TRUE) %>% dst_change_summary()
  }
  
  #prepare dst info
  if(rlang::is_true(nrow(dst_info) != 0)) {
    dst_info <- 
      dst_info |> 
      tidyr::unite(col = "Group", -file.name, -dst_start, sep = ", ") |> 
      dplyr::mutate(Group = paste0("File: ", file.name, ", Group:", Group))
    dst_info <- 
      paste0(
        "Observations in the following ", 
        dst_info$file.name %>% unique() %>% length(),
        " file(s) and ",
        dst_info$Group %>% unique() %>% length(),
        " Id(s) cross to or from daylight savings time (DST): \n",
        dst_info$Group %>% paste0(collapse = "\n"), "\n")
    if(dst_adjustment) {
      dst_info <- paste0(dst_info, "The Datetime column was adjusted in these files. For more info on what that entails see `?dst_change_handler`.\n")
    } else {
      dst_info <- paste0(dst_info, "Please make sure that the timestamps in the source files correctly reflect these changes from DST<>ST. \nTo adjust datetimes after a jump, set `dst_adjustment = TRUE` or see `?dst_change_handler` for manual adjustment.\n")
    }
  } else {
    dst_info <- NULL
  }
    
  #prepare NA datetimes
  if(na.count == 0) {
    na.count <- NULL
  } else {
    na.count <- paste0(na.count, " observations were dropped due to a missing or non-parseable Datetime value (e.g., non-valid timestamps during DST jumps). \n")
  }
  
  #prepare not.before
  cutoff <- NULL
  if(!is.null(not.before)){
    cutoff <- paste0("Data from before ", not.before, " were not imported. Adjust with `not.before` if needed. \n")
  }
  
  #print all infos
    cat(
    "\n",
    "Successfully read in ", format(nrow(data), big.mark = "'"), 
    " observations across ", n_ids, " Ids from ",  n_files, " ", device, "-file(s).\n",
    "Timezone set is ", tz, ".\n", 
    if(lubridate::tz(data$Datetime) != Sys.timezone()) {
      paste0(
        "The system timezone is ",
        Sys.timezone(),
        ". Please correct if necessary!\n")},
    dst_info, na.count, "\n",
    "First Observation: ", format(min.time), "\n",
    "Last Observation: ", format(max.time), "\n",
    cutoff,
    "Timespan: " , diff(c(min.time, max.time)) %>% format(digits = 2), "\n\n",
    "Observation intervals: \n",
    sep = "")
  utils::capture.output(interval.time %>% print(n=print_n))[c(-1,-2,-4)] %>% 
    cat(sep = "\n")
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
  
  #read in all the lines
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
    stop("Could not find a line with this order of column names in the file: '",
         basename(filepath),
    "'.\n Please check the correct order and spelling of the given columns: '",
    stringr::str_flatten_comma(column_names), "'")
  }
  
  #if there is more than one line with the column names, return an error
  if(length(which_lines) > 1) {
    stop(
        "Found ", length(which_lines), " lines with the given column names: '", 
        stringr::str_flatten_comma(column_names),
        "', but require exactly 1.\n Please provide a more specific pattern or remove the ambiguous lines from the file: '", 
        basename(filepath), "'"
      )
  }
  
  #if there is only one line with the column names, return the line number
  #and reduce it by one to get the lines to skip
  if(length(which_lines) == 1) {
    return(which_lines-1)
  }
  
}
