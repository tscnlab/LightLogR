#' Handle jumps in Daylight Savings (DST) that are missing in the data
#' 
#' @description
#' When data is imported through `LightLogR` and a timezone applied, it is 
#' assumed that the timestamps are correct - which is the case, e.g., if 
#' timestamps are stored in `UTC`, or they are in local time. Some if not most 
#' measurement devices are set to local time before a recording interval starts. 
#' If during the recording a daylight savings jump happens (in either 
#' direction), the device might not adjust timestamps for this change. This 
#' results in an unwanted shift in the data, starting at the time of the DST
#' jump and likely continues until the end of a file. `dst_change_handler` is 
#' used to detect such jumps within a group and apply the correct shift in the 
#' data (i.e., the shift that should have been applied by the device). 
#' 
#' **important** Note that this function is only useful if the time stamp in
#' the raw data deviates from the actual date-time. Note also, that this 
#' function results in a gap during the DST jump, which should be handled by
#' `gap_handler()` afterwards. It will also result in potentially double the
#' timestamps during the jum back from DST to standard time. This will result
#' in some inconsistencies with some functions, so we recommend to use
#' `aggregate_Datetime()` afterwards with a `unit` equal to the dominant epoch.
#' Finally, the function is not equipped to handle more than one jump per group.
#' The jump is based on whether the group starts out with DST or not. **the
#' function will remove datetime rows with `NA` values**.
#'
#' @inheritParams dst_change_summary
#' @param filename.colname (optional) column name that contains the filename. 
#' If provided, it will use this column as a temporary grouping variable
#' additionally to the `dataset` grouping.
#'
#' @return A `tibble` with the same columns as the input dataset, but shifted
#' @export
#' @details The detection of a DST jump is based on the function `lubridate::dst()` and jumps are only applied within a group. During import, this function is used if `dst_adjustment = TRUE` is set and includes by default the filename as the grouping variable, additionally to `Id`.
#'
#' @family DST
#' 
#' @examples
#' #create some data that crosses a DST jump
#' data <- 
#'  tibble::tibble(
#'  Datetime = seq.POSIXt(from = as.POSIXct("2023-03-26 01:30:00", tz = "Europe/Berlin"),
#'                      to = as.POSIXct("2023-03-26 03:00:00", tz = "Europe/Berlin"),
#'                      by = "30 mins"),
#'                      Value = 1)
#'  #as can be seen next, there is a gap in the data - this is necessary when
#'  #using a timezone with DST. 
#'  data$Datetime
#'  #Let us say now, that the device did not adjust for the DST - thus the 03:00 
#'  #timestamp is actually 04:00 in local time. This can be corrected for by:
#'  data %>% dst_change_handler() %>% .$Datetime

dst_change_handler <- function(dataset,
                          Datetime.colname = Datetime,
                          filename.colname = NULL
                          ) {
  
  # Initial Checks ----------------------------------------------------------
  Datetime.colname <- rlang::enexpr(Datetime.colname)
  filename.colname <- rlang::enexpr(filename.colname)
  
  #check if dataset is a dataframe
  if (!is.data.frame(dataset)) {
    stop("dataset is not a dataframe")
  }
  #check if Datetime.colname is part of the dataset
  Datetime.colname.defused <- colname.defused({{ Datetime.colname }})
  if (!Datetime.colname.defused %in% names(dataset)) {
    stop("Datetime.colname must be part of the dataset")
  }
  #check if Datetime.colname is a Datetime
  if (!lubridate::is.POSIXct(dataset[[Datetime.colname.defused]])) {
    stop("Datetime.colname must be a Datetime")
  }
  #check if filename.colname is part of the dataset, if provided
  if (!is.null(filename.colname)) {
    if(!rlang::is_symbol(filename.colname)) {
      stop("filename.colname must be a symbol")
    }
    filename.colname.defused <- colname.defused({{ filename.colname }})
    if (!filename.colname.defused %in% names(dataset)) {
      stop("filename.colname must be part of the dataset")
    }
  }
  
  # Data Preparation --------------------------------------------------------
  
  #save the starting grouping keys
  grouping_keys <- dataset %>% dplyr::groups()
  
  #add the filename to the grouping if filename.colname is provided
  if (!is.null(filename.colname)) {
    dataset <- dataset %>% dplyr::group_by(!!filename.colname, .add = TRUE)
  }  
  
  #do the dst change handling
  dataset_summary <- 
    dataset %>% dst_change_summary(Datetime.colname = !!Datetime.colname)
  
  #join the summary to the dataset to check affected groupings
  dataset <- 
    #if there is no dst change, just return the dataset
    if(dataset_summary %>% .[[1]] %>% length() == 0) {
      dataset %>%
        dplyr::mutate(dst_start = NA)
    }
  #if there is a dst change but no grouping, perform a cross join
    else if (dplyr::group_vars(dataset) %>% length() == 0) {
      dataset %>% 
        dplyr::cross_join(dataset_summary)
  #if there is a dst change and grouping, perform a left join based on the grouping
    }
    else {
        dataset %>% dplyr::left_join(dataset_summary, by = dplyr::group_vars(dataset))
    }
    
  
  #change the Datetime in the affected ids depending on whether the change is from dst to non-dst or vice versa
  dataset <- 
    dataset %>% 
    dst_indicator() %>% 
    tidyr::drop_na(.dst) %>% 
    dplyr::mutate(
      .change =
        dplyr::case_when(
          dst_start & !.dst ~ 
            lubridate::dhours(-1),
          !dst_start & .dst ~ 
            lubridate::dhours(1),
          .default = lubridate::dhours(0)
        ),
      {{ Datetime.colname }} :=
        {{ Datetime.colname }} + .change
    ) %>% 
    dplyr::select(-dst_start, -.dst, -.change)
  
  #restore the old grouping structure
  dataset <- dataset %>% dplyr::group_by(!!!grouping_keys)
  
  dataset
  
}  



# dst_change_summary ------------------------------------------------------

#return a dataframe with the groups where a dst change occurs and whether the transition is from dst to non-dst or vice versa
#' Get a summary of groups where a daylight saving time change occurs.
#'
#' @param dataset dataset to be summarized, must be a `dataframe`
#' @param Datetime.colname name of the column that contains the Datetime data, expects a `symbol`
#'
#' @return a `tibble` with the groups where a dst change occurs. The column `dst_start` is a boolean that indicates whether the start of this group occurs during daylight savings.
#' @export
#' @family DST
#'
#' @examples
#' sample.data.environment %>% 
#'   dplyr::mutate(Datetime = 
#'   Datetime + lubridate::dweeks(8)) %>%
#'   dst_change_summary()
dst_change_summary <- function(dataset, Datetime.colname = Datetime) {
  #check if dataset is a dataframe
  if (!is.data.frame(dataset)) {
    stop("dataset is not a dataframe")
  }
  #check if Datetime.colname is part of the dataset
  Datetime.colname.defused <- colname.defused({{ Datetime.colname }})
  if (!Datetime.colname.defused %in% names(dataset)) {
    stop("Datetime.colname must be part of the dataset")
  }
  
  #add a column to the dataset that indicates DST
  dataset <- 
    dataset %>% dst_indicator({{ Datetime.colname }}) %>% tidyr::drop_na(.dst)
  #summarize the dataset to check whether there are any DST changes
  dataset <- 
    dataset %>% dplyr::summarize(
    .dst2 = sum(.dst)/sum(!is.na(.dst)),
    dst_start = dplyr::first(.dst)
  ) %>% 
    dplyr::filter(.dst2 < 1 & .dst2 > 0) %>% 
    dplyr::select(-.dst2)
  #return the dataset
  dataset
}

# dst_indicator ------------------------------------------------------

#return an extended dataframe with a column that indicates whether the time is in dst or not
dst_indicator <- function(dataset, Datetime.colname = Datetime) {
  #add a column to the dataset that indicates DST
  dataset %>% 
    dplyr::mutate(
      .dst = lubridate::dst({{ Datetime.colname }})
    )
}