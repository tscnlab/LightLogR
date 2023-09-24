# path <- "/Users/zauner/Documents/Arbeit/12-TUM/WP2.3.1/Example_File_Formats/ActLumus/Caroilna_2023-09-05/201_sleepdiary_all.csv"
# 
# test <- import.Statechanges(path,
#                             State.colnames = c("sleep", "offset"),
#                             State.encoding = c("sleep", "wake"),
#                             ID.colname = record_id,
#                             sep = ";",
#                             dec = ",")


#' Title
#'
#' @param filePath 
#' @param sep 
#' @param dec 
#' @param Datetime.format 
#' @param tz 
#' @param State.colnames 
#' @param State.encoding 
#' @param ID.colname 
#' @param State.newname 
#' @param State.valueName 
#' @param ID.newname 
#' @param keepAllColumns 
#'
#' @return a dataset
#' @export
#'
#' @examples
#' #example
import.Statechanges <- function(filePath, 
                       sep = ",", 
                       dec = ".", 
                       Datetime.format = "ymdHMS",
                       tz = "UTC",
                       State.colnames, # a vector
                       State.encoding = State.colnames,
                       ID.colname,
                       State.newname = State,
                       State.valueName = Datetime,
                       ID.newname = Id,
                       keepAllColumns = FALSE) {
  
  if(!is.character(filePath)) {
    stop("The specified filePath must be a character")
  }
  if(!is.character(State.colnames)) {
    stop("The specified State.colnames must be a character (vector)")
  }
  if(!is.character(State.encoding)) {
    stop("The specified State.encoding must be a character (vector)")
  }
  if(length(State.colnames) != length(State.encoding)) {
    stop("The length of State.colnames and State.encoding must be equal")
  }
  
  # Read in the data with the specified separator and decimal
  data <- readr::read_delim(
    filePath, 
    delim = sep, 
    locale = readr::locale(decimal_mark = dec), 
    col_types = readr::cols()
    )
  
  # Convert inputs to strings for matching
  idStr <- as.character(rlang::ensym(ID.colname))
  State.newnameStr <- as.character(rlang::ensym(State.newname))
  State.valueNameStr <- as.character(rlang::ensym(State.valueName))
  stateStrs <- State.colnames
  stateNames <- rlang::set_names(State.encoding, State.colnames)
  
  # Check if the specified columns exist
  if(!all(c(idStr, stateStrs) %in% colnames(data))) {
    stop("The specified Datetime, ID, or one of the State columns is not present in the data!")
  }
  
  # Pivot state columns longer
  data <- data %>%
    tidyr::pivot_longer(cols = dplyr::all_of(stateStrs),
                        names_to = {{ State.newnameStr }},
                        values_to = {{ State.valueNameStr }})

  # Rename columns using the {{ }} syntax, remove NA and group by ID, arrange it, recode, and set Datetimes as POSIXct
  data <- dplyr::rename(data,
                        {{ ID.newname }} := {{ ID.colname }}) %>% 
    dplyr::filter(!is.na({{ ID.newname }}), !is.na({{ State.valueName }})) %>% 
    dplyr::group_by({{ ID.newname }}) %>% 
    dplyr::mutate({{ State.newname }} := stateNames[{{ State.newname }}],
                  {{ State.valueName }} := lubridate::parse_date_time({{ State.valueName }}, orders = Datetime.format, tz),
                  {{ ID.newname }} := factor({{ ID.newname }})) %>% 
    dplyr::arrange({{ State.valueName }}, .by_group = TRUE)
  
  # Decide on whether to keep other columns
  if (!keepAllColumns) {
    data <- dplyr::select(data, {{ ID.newname }}, {{ State.newname }}, {{ State.valueName }})
  }
  
  return(data)
}

import.Sleep <- function(filePath, State.newname = Sleep,
                         ...) {
  import.Statechanges(filePath = filePath, ..., State.newname = Sleep)
}