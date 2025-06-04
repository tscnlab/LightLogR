#' Import data that contain `Datetimes` of `Statechanges`
#'
#' Auxiliary data greatly enhances data analysis. This function allows the
#' import of files that contain `Statechanges`, i.e., specific time points of
#' when a `State` (like `sleep` or `wake`) starts.
#'
#' Data can be present in the long or wide format.
#' * In the `wide` format, multiple `Datetime` columns indicate the state through the column name. These get pivoted to the `long` format and can be recoded through the `State.encoding` argument.
#' * In the `long` format, one column indicates the `State`, while the other gives the `Datetime`.
#'
#' @param filename Filename(s) for the Dataset. Can also contain the filepath,
#' but `path` must then be `NULL`. Expects a `character`. If the vector is
#' longer than `1`, multiple files will be read in into one Tibble.
#' @param path Optional path for the dataset(s). `NULL` is the default. Expects
#' a `character`.
#' @param tz Timezone of the data. `"UTC"` is the default. Expects a
#' `character`. You can look up the supported timezones with [OlsonNames()].
#' @param sep String that separates columns in the import file. Defaults to
#'   `","`.
#' @param dec String that indicates a decimal separator in the import file.
#'   Defaults to `"."`.
#' @param structure String that specifies whether the import file is in the
#'   `long` or `wide` format. Defaults to `"wide"`.
#' @param Datetime.format String that specifies the format of the `Datetimes` in
#'   the file. The default `"ymdHMS"` specifies a format like "2023-07-10
#'   10:00:00". In the function, [lubridate::parse_date_time()] does the actual
#'   conversion - the documentation can be searched for valid inputs.
#' @param State.colnames Column name or vector of column names (the latter only
#'   in the `wide` format). Expects a `character`.
#' * In the `wide` format, the column names indicate the `State` and must contain `Datetimes`. The columns will be pivoted to the columns specified in `Datetime.column` and `State.newname`.
#' * In the `long` format, the column contains the `State`
#' @param State.encoding In the `wide` format, this enables recoding the column
#'   names to state names, if there are any differences. The default uses the
#'   `State.colnames` argument. Expects a `character` (vector) with the same
#'   length as `State.colnames`.
#' @param Datetime.column Symbol of the `Datetime` column (which is also the
#'   default).
#' * In the `wide` format, this is the newly created column from the `Datetimes` in the `State.colnames`.
#' * In the `long` format, this is the existing column that contains the `Datetimes`.
#' @param Id.colname Symbol of the column that contains the `ID` of the subject.
#' @param State.newname Symbol of the column that will contain the `State` of
#'   the subject. In the `wide` format, this is the newly created column from
#'   the `State.colnames`. In the `long` format, this argument is used to rename
#'   the `State` column.
#' @param Id.newname Column name used for renaming the `Id.colname` column.
#' @param keep.all Logical that specifies whether all columns should be
#'   kept in the output. Defaults to `FALSE`.
#' @param silent Logical that specifies whether a summary of the
#'   imported data should be shown. Defaults to `FALSE`.
#'
#' @return a dataset with the `ID`, `State`, and `Datetime` columns. May contain
#'   additional columns if `keep.all` is `TRUE`.
#' @export
#'
#' @examples
#' #get the example file from within the package
#' path <- system.file("extdata/",
#' package = "LightLogR")
#' file.sleep <- "205_sleepdiary_all_20230904.csv"
#'
#' #import Data in the wide format (sleep/wake times)
#' import_Statechanges(file.sleep, path,
#' Datetime.format = "dmyHM",
#' State.colnames = c("sleep", "offset"),
#' State.encoding = c("sleep", "wake"),
#' Id.colname = record_id,
#' sep = ";",
#' dec = ",")
#'
#' #import in the long format (Comments on sleep)
#' import_Statechanges(file.sleep, path,
#'                    Datetime.format = "dmyHM",
#'                    State.colnames = "comments",
#'                    Datetime.column = sleep,
#'                    Id.colname = record_id,
#'                    sep = ";",
#'                    dec = ",", structure = "long")

import_Statechanges <- function(filename, 
                       path = NULL, 
                       sep = ",", 
                       dec = ".", 
                       structure = c("wide", "long"),
                       Datetime.format = "ymdHMS",
                       tz = "UTC",
                       State.colnames, # a vector
                       State.encoding = State.colnames,
                       Datetime.column = Datetime,
                       Id.colname,
                       State.newname = State,
                       Id.newname = Id,
                       keep.all = FALSE,
                       silent = FALSE) {
  
  # Initial Checks ----------------------------------------------------------
  
  # Match input arguments
  structure <- match.arg(structure)
  
  # Check inputs
  if(!is.character(filename)) {
    stop("The specified filename must be a character")
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
  if(structure == "long" & length(State.colnames) > 1) {
    stop("In the long format, State.colnames must be a scalar")
  }
  if(!is.logical(keep.all)) {
    stop("The specified keep.all must be a logical")
  }
  if(!is.character(Datetime.format)) {
    stop("The specified Datetime.format must be a character that works with lubridate::parse_date_time")
  }
  if(!is.character(tz)) {
    stop("The specified tz must be a character")
  }
  if(!tz %in% OlsonNames()) {
    stop("The specified tz must be a valid time zone from the OlsonNames vector")
  }
  if(!is.logical(silent)) {
    stop("The specified silent must be a logical")
  }
  
  # Logic ----------------------------------------------------------
  
  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
  
  # Read in the data with the specified separator and decimal
  data <- readr::read_delim(
    filename, 
    delim = sep, 
    locale = readr::locale(decimal_mark = dec), 
    col_types = readr::cols()
    )

  # Convert inputs to strings for matching
  idStr <- as.character(rlang::ensym(Id.colname))
  State.newnameStr <- as.character(rlang::ensym(State.newname))
  Datetime.columnStr <- as.character(rlang::ensym(Datetime.column))
  stateStrs <- State.colnames
  stateNames <- rlang::set_names(State.encoding, State.colnames)
  
  
  # Check if the specified columns exist
  if(!all(c(idStr, stateStrs) %in% colnames(data))) {
    stop("The specified ID, or one of the State columns is not present in the data!")
  }
  #check if the Datetime column is present when the structure argument is set to `long`
  if(structure == "long" & !Datetime.columnStr %in% colnames(data)) {
    stop("The specified Datetime column is not present in the data!")
  }
  
  # Pivot state columns longer when the structure argument is set to `wide`
  if(structure == "wide") {
  data <- data %>%
    tidyr::pivot_longer(cols = dplyr::all_of(stateStrs),
                        names_to = {{ State.newnameStr }},
                        values_to = {{ Datetime.columnStr }})
  }
  
  # Rename columns using the {{ }} syntax, remove NA and group by ID, arrange 
  # it, recode, and set Datetimes as POSIXct
  data <- dplyr::rename(data,
                        {{ Id.newname }} := {{ Id.colname }},
                        Datetime = {{ Datetime.column }}) %>% 
    dplyr::filter(!is.na({{ Id.newname }}), !is.na(Datetime)) %>% 
    dplyr::group_by({{ Id.newname }})
  
  data <- 
    if(structure == "wide") {
    data %>% 
        dplyr::mutate({{ State.newname }} := stateNames[{{ State.newname }}])
    } else data %>% 
        dplyr::rename({{ State.newname }} := {{ State.colnames }})
  
  if(!lubridate::is.POSIXct(data$Datetime)){
    data <- data %>%  
      dplyr::mutate(Datetime = 
                      lubridate::parse_date_time(Datetime,   
                                                 orders = Datetime.format, tz
                      ))
  } else {
    data <- data %>%  
      dplyr::mutate(Datetime = 
                      lubridate::force_tz(Datetime, tz = tz)
                      )
  }
  
  #make sure there were no parsing errors
  if(any(is.na(data$Datetime))) {
    stop("Some of the Datetime values could not be parsed! Check the Datetime.format parameter and the data. If you read in more than one file, make sure that the format is consistent across all files. If not, try `files %>% purrr::map(import_Statechanges, *arguments here* ) %>% dplyr::bind_rows()`")
  }
  
  #create a factor from Id and arrange the data by Datetime
  data <- data %>%  
    dplyr::mutate({{ Id.newname }} := factor({{ Id.newname }})) %>% 
    dplyr::arrange(Datetime, .by_group = TRUE)
  
  #if there are Datetimes with NA value, drop them
  na.count <- 0
  if(any(is.na(data$Datetime))) {
    na.count <- sum(is.na(data$Datetime))
    data <- data %>% tidyr::drop_na(Datetime)
  }
  
  # Return the Data ----------------------------------------------------------
  
  # Decide on whether to keep other columns
  if (!keep.all) {
    data <- 
      dplyr::select(
        data, {{ Id.newname }}, {{ State.newname }}, Datetime
        )
  }
  
  #create a warning if consecutive states are the same
  if(any(dplyr::lag(data$State) == data$State, na.rm = TRUE)) {
    warning(
      "There are consecutive states that are the same. This may or may not be an error in the data."
      )
  }
  
  #give a summary about the imported data
  if(!silent) 
    import.info(data = data, 
                device = "Statechanges", 
                tz = tz, 
                Id.colname = {{ Id.newname }}, 
                dst_adjustment = FALSE, 
                dst_info = FALSE, 
                filename = filename, 
                na.count = na.count,
                not.before = NULL)
  
  return(data)
}
