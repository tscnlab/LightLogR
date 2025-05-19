#' Calculate duration of data in each group
#'
#' This function calculates the total duration of data in each group of a
#' dataset, based on a datetime column and a variable column. It uses the
#' dominant epoch (interval) of each group to calculate the duration.
#'
#' @param dataset A light logger dataset. Expects a dataframe. If not imported
#'   by LightLogR, take care to choose sensible variables for the
#'   Datetime.colname and Variable.colname.
#' @param Datetime.colname Column name that contains the datetime. Defaults to
#'   "Datetime" which is automatically correct for data imported with LightLogR.
#'   Expects a symbol. Needs to be part of the dataset. Must be of type POSIXct.
#' @param Variable.colname Column name that contains the variable for which to
#'   calculate the duration. Expects a symbol. Needs to be part of the dataset.
#' @param count.NA Logical. Should NA values in Variable.colname be counted as
#'   part of the duration? Defaults to FALSE.
#' @param show.missing Logical. Should the duration of NAs be provided in a
#'   separate column "Missing"? Defaults to FALSE.
#' @param show.interval Logical. Should the dominant epoch (interval) be shown
#'   in a column "interval"? Defaults to FALSE.
#' @param FALSE.as.NA Logical. Should FALSE values in the Variable.colname be
#'   treated as NA (i.e., missing)?
#'
#' @return A tibble with one row per group and a column "duration" containing
#'   the duration of each group as a [lubridate::duration()]. If `show.missing =
#'   TRUE`, a column "missing" is added with the duration of NAs, and a column
#'   "total" with the total duration. If `show.interval = TRUE`, a column
#'   "interval" is added with the dominant epoch of each group.
#'
#' @export
#'
#' @examples
#' # Calculate the duration of a dataset
#' durations(sample.data.environment)
#'
#' # create artificial gaps in the data
#' gapped_data <-
#' sample.data.environment |>
#'   dplyr::filter(MEDI >= 10) |>
#'   gap_handler(full.days = TRUE)
#'
#' #by default, the Datetime column is selected for the `Variable.colname`, 
#' #basically ignoring NA measurement values
#' gapped_data |>
#'  durations(count.NA = TRUE)
#'
#' # Calculate the duration where MEDI are available
#' durations(gapped_data, MEDI)
#'
#' # Calculate the duration, show the duration of NAs separately
#' durations(gapped_data, MEDI, show.missing = TRUE)
#'
#' # Calculate the duration, show the dominant epoch
#' durations(gapped_data, Variable.colname = MEDI, show.interval = TRUE)
#'
#' # Calculate durations for day and night separately
#' gapped_data |>
#'   add_photoperiod(coordinates = c(48.52, 9.06)) |>
#'   dplyr::group_by(photoperiod.state, .add = TRUE) |>
#'   durations(Variable.colname = MEDI, show.interval = TRUE, show.missing = TRUE)
durations <- function(
    dataset,
    Variable.colname = Datetime,
    Datetime.colname = Datetime,
    count.NA = FALSE,
    show.missing = FALSE,
    show.interval = FALSE,
    FALSE.as.NA = FALSE
) {
  # Check for valid inputs
  if (!is.data.frame(dataset)) {
    stop("dataset must be a data frame")
  }
  
  if(FALSE.as.NA) {
    dataset <- 
      dataset |> 
      dplyr::mutate({{ Variable.colname }} := ifelse({{ Variable.colname }}, {{ Variable.colname }}, NA))
  }
  
  # Define a function to calculate duration for a single group
  calc_durations <- function(data) {
    # Get the datetime and variable vectors
    datetime_vec <- dplyr::pull(data, {{Datetime.colname}})
    variable_vec <- dplyr::pull(data, {{Variable.colname}})
    
    # Calculate the dominant epoch
    epoch_df <- count_difftime(tibble::tibble(Datetime = datetime_vec))
    epoch <- epoch_df$difftime[1]  # Use the most common interval
    
    # Calculate counts
    total_count <- length(datetime_vec)
    non_na_count <- sum(!is.na(variable_vec))
    na_count <- sum(is.na(variable_vec))
    
    # Calculate duration
    duration <- if (!count.NA) {
      (non_na_count * epoch) %>% lubridate::as.duration()
    } else {
      (total_count * epoch) %>% lubridate::as.duration()
    }
    
    # Build result
    result <- tibble::tibble(duration = duration)
    
    # Add Missing column if requested
    if (show.missing) {
      result$missing <- (na_count * epoch) %>% lubridate::as.duration()
      result$total <- result$missing + result$duration
    }
    
    # Add interval column if requested
    if (show.interval) {
      result$interval <- epoch
    }
    
    return(result)
  }
  
  # Apply the calculation function to each group
  result <- dataset %>%
    dplyr::group_by(dplyr::across(dplyr::group_vars(.))) %>%
    dplyr::group_modify(~calc_durations(.x))
  
  return(result)
}
