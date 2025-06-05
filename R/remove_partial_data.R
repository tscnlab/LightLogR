#' Remove groups that have too few data points
#'
#' This function removes groups from a dataframe that do not have sufficient
#' data points. Groups of one data point will automatically be removed. Single
#' data points are common after using [aggregate_Datetime()].
#'
#' @param dataset A light logger dataset. Expects a dataframe. If not imported
#'   by LightLogR, take care to choose sensible variables for the
#'   Datetime.colname and Variable.colname.
#' @param Datetime.colname Column name that contains the datetime. Defaults to
#'   "Datetime" which is automatically correct for data imported with LightLogR.
#'   Expects a symbol. Needs to be part of the dataset. Must be of type POSIXct.
#' @param Variable.colname Column name that contains the variable for which to
#'   assess sufficient datapoints. Expects a symbol. Needs to be part of the
#'   dataset. Default is `Datetime`, which makes only sense in the presence of
#'   single data point groups that need to be removed.
#' @param threshold.missing either
#'   - percentage of missing data, before that group gets removed. Expects a numeric scalar.
#'   - duration of missing data, before that group gets removed. Expects either a [lubridate::duration()] or a character that can be converted to one, e.g., "30 mins".
#' @param handle.gaps Logical, whether the data shall be treated with
#'   [gap_handler()]. Is set to `FALSE` by default. If `TRUE`, it will be used
#'   with the argument `full.days = TRUE`.
#' @param show.result Logical, whether the output of the function is summary of
#'   the data (TRUE), or the reduced dataset (FALSE, the default)
#' @param by.date Logical. Should the data be (additionally) grouped by day?
#'   Defaults to `FALSE`. Additional grouping is not persitant beyond the
#'   function call.
#'
#' @return if `show.result = FALSE`(default), a reduced dataframe without the
#'   groups that did not have sufficient data
#'
#' @export
#'
#' @examples
#' #create sample data with gaps
#' gapped_data <-
#'   sample.data.environment |>
#'   dplyr::filter(MEDI < 30000)
#'
#' #check their status, based on the MEDI variable
#' gapped_data |> remove_partial_data(MEDI, handle.gaps = TRUE, show.result = TRUE)
#'
#' #the function will produce a warning if implicit gaps are present
#' gapped_data |> remove_partial_data(MEDI, show.result = TRUE)
#'
#' #one group (Environment) does not make the cut of 20% missing data
#' gapped_data |> remove_partial_data(MEDI, handle.gaps = TRUE) |> dplyr::count(Id)
#' #for comparison
#' gapped_data |> dplyr::count(Id)
#' #If the threshold is set differently, e.g., to 2 days allowed missing, results vary
#' gapped_data |>
#'   remove_partial_data(MEDI, handle.gaps = TRUE, threshold.missing = "2 days") |>
#'   dplyr::count(Id)
#'
#' #The removal can be automatically switched to daily detections within groups
#' gapped_data |>
#'  remove_partial_data(MEDI, handle.gaps = TRUE, by.date = TRUE, show.result = TRUE) |>
#'  head()

remove_partial_data <- function(
    dataset,
    Variable.colname = Datetime,
    threshold.missing = 0.2,
    by.date = FALSE,
    Datetime.colname = Datetime,
    show.result = FALSE,
    handle.gaps = FALSE
) {
  # Check for valid inputs
  if (!is.data.frame(dataset)) {
    stop("dataset must be a data frame")
  }
  
  # Convert to standard evaluation for checking
  Variable.colname_quo <- rlang::enquo(Variable.colname)
  Datetime.colname_quo <- rlang::enquo(Datetime.colname)
  
  # Check if Variable.colname exists in the dataset
  Variable.colname_str <- rlang::as_name(Variable.colname_quo)
  if (!Variable.colname_str %in% colnames(dataset)) {
    stop(paste0("Variable.colname '", Variable.colname_str, "' not found in dataset"))
  }
  
  # Check if Datetime.colname exists in the dataset
  Datetime.colname_str <- rlang::as_name(Datetime.colname_quo)
  if (!Datetime.colname_str %in% colnames(dataset)) {
    stop(paste0("Datetime.colname '", Datetime.colname_str, "' not found in dataset"))
  }
  
  # Check if Datetime column is of type POSIXct
  if (!inherits(dataset[[Datetime.colname_str]], "POSIXct")) {
    stop(paste0("Datetime.colname '", Datetime.colname_str, "' must be of type POSIXct"))
  }
  
  if (is.numeric(threshold.missing) && (threshold.missing < 0 || threshold.missing > 1)) {
    stop("threshold.missing as percentage must be between 0 and 1")
  }
  
  # Check show.result and handle.gaps
  if (!is.logical(show.result)) {
    stop("show.result must be logical (TRUE or FALSE)")
  }
  
  if (!is.logical(handle.gaps)) {
    stop("handle.gaps must be logical (TRUE or FALSE)")
  }
  
  if (!is.logical(by.date)) {
    stop("by.date must be logical (TRUE or FALSE)")
  }
  
  groups <- dplyr::groups(dataset)
  
  if (by.date) {
    dataset <- 
      dataset |> 
      dplyr::group_by(.date= lubridate::date({{ Datetime.colname }}), .add = TRUE)
  }
  
  #make gaps explicit
  if(handle.gaps) {
    data <- 
      dataset |> 
      gap_handler(full.days = TRUE, Datetime.colname = {{ Datetime.colname }})
  } else {
    if(has_gaps(dataset)) message("This dataset has implicit gaps. Please make sure to convert them to explicit gaps or that you really know what you are doing")
    if(has_irregulars(dataset)) message("This dataset has irregular or singular data. Singular data will automatically be removed. If you are uncertain about irregular data, you can check them with `gap_finder`, `gap_table`, and `gg_gaps`.")
    data <- dataset
  }
  
  #convert character threshold to duration
  if(is.character(threshold.missing)) {
    threshold.missing <- lubridate::as.duration(threshold.missing)
  }
  
  #calculate missing times
  data <- 
  data |> 
    durations(
      Variable.colname = {{ Variable.colname }},
      Datetime.colname = {{ Datetime.colname }},
      show.missing = TRUE, 
      show.interval = TRUE
    ) |> 
    dplyr::mutate(
      missing_pct = missing/total, .after = total,
      threshold = threshold.missing
    )
  
  #set marker for removal either based on percentage missing or on duration
  if(inherits(threshold.missing, "Duration")) {
    data <- 
      data |> 
      dplyr::mutate(
        marked.for.removal = missing > threshold.missing,
        .before = 1
      )
  } else {
    data <- 
      data |> 
      dplyr::mutate(
        marked.for.removal = missing_pct > threshold.missing,
        .before = 1
      )
    
  }
  
  if(any(is.na(data$marked.for.removal))){
    data <- 
      data |> 
      dplyr::mutate(
        marked.for.removal = ifelse(is.na(marked.for.removal), TRUE, marked.for.removal)
      )
  }
  
  #return the result
  if(show.result) {return(data)}
  
  # Identify groups to keep (not marked for removal)
  groups_to_keep <- data |>
    dplyr::filter(!marked.for.removal)
  
  # Determine the group columns (all columns except the ones created by durations)
  excluded_cols <- c("marked.for.removal", "missing", "total", "interval", "missing_pct", "duration", "threshold")
  group_cols <- setdiff(colnames(groups_to_keep), excluded_cols)
  
  # return the dataset if nothing is to be removed
  if(!any(data$marked.for.removal)) return(dataset |> dplyr::group_by(!!!groups))
  
  # return a message, if nothing is left
  if(nrow(groups_to_keep) == 0) {
    message("No groups are left after removing insufficient groups")
    return(data)
  }
  
  # Filter the original dataset to keep only the groups not marked for removal
  dataset <- 
  dataset |>
    dplyr::semi_join(groups_to_keep |> 
                       dplyr::select(dplyr::all_of(group_cols)), by = group_cols) |> 
    dplyr::group_by(!!!groups)
  
  # If by.date is TRUE, remove the temporary date grouping
  if (by.date) {
    dataset |> dplyr::select(-.date)
  } else dataset
  
}
