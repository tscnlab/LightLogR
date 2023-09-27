# path <- "/Users/zauner/Documents/Arbeit/12-TUM/WP2.3.1/Example_File_Formats/ActLumus/Caroilna_2023-09-05/201_sleepdiary_all.csv"
# 
# test <- import.Statechanges(path,
#                             State.colnames = c("sleep", "offset"),
#                             State.encoding = c("sleep", "wake"),
#                             ID.colname = record_id,
#                             sep = ";",
#                             dec = ",")


#' Import data that contain `Datetimes` of `Statechanges`
#' 
#' Auxiliary data greatly enhances data analysis. This function allows the import of files that contain `Statechanges`, i.e., specific time points of when a `State` (like `sleep` or `wake`) starts.
#' 
#' Data can be present in the long or wide format.
#' * In the `wide` format, multiple `Datetime` columns indicate the state through the column name. These get pivoted to the `long` format and can be recoded through the `State.encoding` argument.
#' * In the `long` format, one column indicates the `State`, while the other gives the `Datetime`.
#'
#' @inheritParams import.Dataset
#' @param sep String that separates columns in the import file. Defaults to `","`.
#' @param dec String that indicates a decimal separator in the import file. Defaults to `"."`.
#' @param structure String that specifies whether the import file is in the `long` or `wide` format. Defaults to `"wide"`.
#' @param Datetime.format String that specifies the format of the `Datetimes` in the file. The default `"ymdHMS"` specifies a format like "2023-07-10 10:00:00". In the function, [lubridate::parse_date_time()] does the actual conversion - the documentation can be searched for valid inputs.
#' @param State.colnames Column name or vector of column names (the latter only in the `wide` format). Expects a `character`.
#' * In the `wide` format, the column names indicate the `State` and must contain `Datetimes`. The columns will be pivoted to the columns specified in `Datetime.column` and `State.newname`.
#' * In the `long` format, the column contains the `State`
#' @param State.encoding In the `wide` format, this enables recoding the column names to state names, if there are any differences. The default uses the `State.colnames` argument. Expects a `character` (vector) with the same length as `State.colnames`.
#' @param Datetime.column Symbol of the `Datetime` column (which is also the default). 
#' * In the `wide` format, this is the newly created column from the `Datetimes` in the `State.colnames`.
#' * In the `long` format, this is the existing column that contains the `Datetimes`.
#' @param ID.colname 
#' @param State.newname 
#' @param ID.newname 
#' @param keepAllColumns 
#'
#' @return a dataset
#' @export
#'
#' @examples
#' #example
import.Statechanges <- function(filename, path, 
                       sep = ",", 
                       dec = ".", 
                       structure = "wide",
                       Datetime.format = "ymdHMS",
                       tz = "UTC",
                       State.colnames, # a vector
                       State.encoding = State.colnames,
                       Datetime.column = Datetime,
                       ID.colname,
                       State.newname = State,
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
  import.Statechanges(filePath = filePath, ..., State.newname = State.newname)
}