#' Counts the Time differences (epochs) per group (in a grouped dataset)
#'
#' @inheritParams cut_Datetime
#'
#' @return a `tibble` with the number of occurences of each time difference per
#'   group
#' @export
#'
#' @examples
#' #get a dataset with irregular intervals
#' filepath <- system.file("extdata/sample_data_LYS.csv", package = "LightLogR")
#' dataset <- import$LYS(filepath)
#' 
#' #count_difftime returns the number of occurences of each time difference
#' #and is more comprehensive in terms of a summary than `gap_finder` or 
#' #`dominant_epoch`
#' count_difftime(dataset)
#' dominant_epoch(dataset)
#' gap_finder(dataset)
#' 
#' #irregular data can be regularized with `aggregate_Datetime`
#' dataset %>% aggregate_Datetime(unit = "15 secs") %>% count_difftime()

count_difftime <- function(dataset, Datetime.colname = Datetime) {
  dataset %>% 
    dplyr::mutate(
      difftime = c(NA, diff({{Datetime.colname}}) %>% lubridate::as.duration())
    ) %>% 
    tidyr::drop_na(difftime) %>% 
    dplyr::count(difftime = difftime %>% lubridate::as.duration(), sort = TRUE)
}

#calculate the nth Quantile of time differences per group (in a grouped dataset)
nth.difftime <- function(dataset, Datetime.colname = Datetime, n = 0.95) {
  dataset %>% 
    dplyr::mutate(
      difftime = c(NA, diff({{Datetime.colname}}) %>% lubridate::as.duration())
    ) %>% 
    tidyr::drop_na(difftime) %>% 
    dplyr::summarise(
      Quant = stats::quantile(difftime, probs = n, na.rm = TRUE)
    )
}


#' Determine the dominant epoch/interval of a dataset
#'
#' Calculate the dominant epoch/interval of a dataset. The dominant
#' epoch/interval is the epoch/interval that is most frequent in the dataset.
#' The calculation is done per group, so that you might get multiple variables.
#' If two or more epochs/intervals are equally frequent, the first one (shortest
#' one) is chosen.
#'
#' @param dataset A light logger dataset. Needs to be a dataframe.
#' @param Datetime.colname The column that contains the datetime. Needs to be a
#'   `POSIXct` and part of the dataset.
#'
#' @return A `tibble` with one row per group and a column with the
#'   `dominant.epoch` as a [lubridate::duration()]. Also a column with the
#'   `group.indices`, which is helpful for referencing the `dominant.epoch`
#'   across dataframes of equal grouping.
#' @export
#'
#' @family regularize
#' @examples
#' dataset <-
#' tibble::tibble(Id = c("A", "A", "A", "B", "B", "B"),
#'               Datetime = lubridate::as_datetime(1) +
#'                          lubridate::days(c(0:2, 4, 6, 8)))
#' dataset
#' #get the dominant epoch by group
#' dataset %>%
#' dplyr::group_by(Id) %>%
#' dominant_epoch()
#'
#' #get the dominant epoch of the whole dataset
#' dataset %>%
#' dominant_epoch()              
dominant_epoch <- function(dataset, 
                           Datetime.colname = Datetime) {
  
  # Initial Checks ----------------------------------------------------------
  
  #dataset needs to be a dataframe and Datetime.colname needs to be part of the dataset and a POSIXct
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "Datetime.colname must be part of the dataset" = 
      colname.defused({{ Datetime.colname }}) %in% names(dataset),
    "Datetime.colname must be a Datetime" = 
      lubridate::is.POSIXct(
        dataset[[colname.defused({{ Datetime.colname }})]])
  )
  
  # Function ----------------------------------------------------------
  
  dat <- 
    dataset %>% 
    count_difftime(Datetime.colname = {{ Datetime.colname }}) %>% 
    dplyr::summarize(
      dominant.epoch = difftime[which.max(n)] %>% lubridate::as.duration(),
      group.indices = dplyr::cur_group_id()
    )
  
  # Return ----------------------------------------------------------
  dat
}