
#' Create reference data from other data
#'
#' Create reference data from almost any other data that has a datetime column
#' and a data column. The reference data can even be created from subsets of the
#' same data. Examples are that one participant can be used as a reference for
#' all other participants, or that the first (second,...) day of every
#' participant data is the reference for any other day. **This function needs to
#' be carefully handled, when the reference data time intervals are shorter than
#' the data time intervals. In that case, use `aggregate_Datetime()` on the
#' reference data beforehand to lengthen the interval.**
#'
#' To use subsets of data, use the `filter.expression.reference` argument to
#' specify the subsets of data. The `across.id` argument specifies whether the
#' reference data should be used across all or some grouping variables (e.g.,
#' across participants). The `shift.start` argument enables a shift of the
#' reference data start time to the start of the respective group.
#'
#' @param dataset A light logger dataset
#' @param Reference.data The data that should be used as reference. By default
#'   the `dataset` will be used as reference.
#' @param Datetime.column Datetime column of the `dataset` and `Reference.data`.
#'   Need to be the same in both sets. Default is `Datetime`.
#' @param Data.column Data column in the `Reference.data` that is then converted
#'   to a reference. Default is `MEDI`.
#' @param Id.column Name of the `Id.column` in both the `dataset` and the
#'   `Reference.data`.
#' @param Reference.column Name of the reference column that will be added to
#'   the `dataset`. Default is `Reference`. Cannot be the same as any other
#'   column in the `dataset` and will throw an error if it is.
#' @param overwrite If `TRUE` (defaults to `FALSE`), the function will
#'   overwrite the `Reference.colname` column if it already exists.
#' @param filter.expression.reference Expression that is used to filter the
#'   `Reference.data` before it is used as reference. Default is `NULL`. See
#' @details and @examples for more information. The expression is evaluated
#'   within [dplyr::filter()].
#' @param across.id Grouping variables that should be ignored when creating the
#'   reference data. Default is `FALSE`. If `TRUE`, all grouping variables are
#'   ignored. If `FALSE`, no grouping variables are ignored. If a vector of
#'   grouping variables is given, these are ignored.
#' @param shift.start If `TRUE`, the reference data is shifted to the start of
#'   the respective group. Default is `FALSE`. The shift ignores the groups
#'   specified in `across.id`.
#' @param length.restriction.seconds Restricts the application of reference data
#'   to a maximum length in seconds. Default is `60` seconds. This is useful to
#'   avoid reference data being applied to long periods of time, e.g., when
#'   there are gaps in the reference data
#' @param shift.intervals Time shift in seconds, that is applied to every data
#'   point in the reference data. Default is `"auto"`. If `"auto"`, the shift is
#'   calculated by halving the most frequent time difference between two data
#'   points in the reference data. If a number is given, this number in seconds
#'   is used as the shift. Can also use [lubridate::duration()] to specify the
#'   shift.
#' @param Reference.label Label that is added to the reference data. If `NULL`,
#'   no label is added.
#'
#' @return A `dataset` with a new column `Reference` that contains the reference
#'   data.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#' library(ggplot2)
#'
#' gg_reference <- function(dataset) {
#' dataset %>%
#' ggplot(aes(x = Datetime, y = MEDI, color = Id)) +
#' geom_line(linewidth = 1) +
#' geom_line(aes(y = Reference), color = "black", size = 0.25, linetype = "dashed") +
#' theme_minimal() + facet_wrap(~ Id, scales = "free_y")
#' }
#'
#' #in this example, each data point is its own reference
#' sample.data.environment %>%
#'   data2reference() %>%
#'   gg_reference()
#'
#' #in this example, the first day of each ID is the reference for the other days
#' #this requires grouping of the Data by Day, which is then specified in across.id
#' #also, shift.start needs to be set to TRUE, to shift the reference data to the
#' #start of the groupings
#' sample.data.environment %>% group_by(Id, Day = as_date(Datetime)) %>%
#' data2reference(
#'   filter.expression.reference =  as_date(Datetime) == min(as_date(Datetime)),
#'   shift.start = TRUE,
#'   across.id = "Day") %>%
#'   gg_reference()
#'
#' #in this example, the Environment Data will be used as a reference
#' sample.data.environment %>%
#' data2reference(
#'   filter.expression.reference =  Id == "Environment",
#'   across.id = TRUE) %>%
#'   gg_reference()
data2reference <- function(dataset, 
                           Reference.data = dataset,
                           Datetime.column = Datetime,
                           Data.column = MEDI,
                           Id.column = Id,
                           Reference.column = Reference,
                           overwrite = FALSE,
                           filter.expression.reference = NULL,
                           across.id = FALSE,
                           shift.start = FALSE,
                           length.restriction.seconds = 60,
                           shift.intervals = "auto",
                           Reference.label = NULL) {
  
  # Initial Checks ----------------------------------------------------------

  Datetime.column.str <- colname.defused({{ Datetime.column }})
  Data.column.str <- colname.defused({{ Data.column }})
  Id.column.str <- colname.defused({{ Id.column }})
  Reference.column.str <- colname.defused({{ Reference.column }})

  existing.names <- c(Datetime.column.str, Data.column.str, Id.column.str)
  
  #give an error if dataset is not a data.frame
  if(!is.data.frame(dataset)) stop("dataset is not a data.frame")
  #give an error if the Reference.data is not a data.frame
  if(!is.data.frame(Reference.data)) stop("Reference.data is not a data.frame")
  
  #give an error if the Datetime, MEDI, and ID columns are not present in the dataset and the Reference.data
  if(!(all(existing.names %in% names(dataset)))) stop("Given columns are not all present in dataset")
  if(!(all(existing.names %in% names(Reference.data)))) stop("Given columns are not all present in Reference.data")
  
  #give an error if shift.start is not a logical
  if(!is.logical(shift.start)) stop("shift.start is not a logical")
  
  #give an error if length.restriction.seconds is not a numeric
  if(!is.numeric(length.restriction.seconds)) stop("length.restriction.seconds is not a numeric")
  
    #give an error or warning if the reference column is present
  if(Reference.column.str %in% names(dataset) & !overwrite) 
    stop("A Reference column with the given (or default) name is already part of the dataset. Please remove the column or choose a different name")
  if(Reference.column.str %in% names(dataset)) 
    warning("A Reference column with the given (or default) name is already part of the dataset. It is overwritten, because `overwrite = TRUE ` was set.")

  
  # Manipulation ----------------------------------------------------------
  
  filter.expression.reference <- rlang::enexpr(filter.expression.reference)
  
  #if the dataset has no grouping, group by the ID column
  if(dplyr::n_groups(dataset) == 0) {
    dataset <- dataset %>% dplyr::group_by({{ Id.column }})
  }
  
  #if the Reference.data has no grouping, group by the ID column
  if(dplyr::n_groups(Reference.data) == 0) {
    Reference.data <- Reference.data %>% dplyr::group_by({{ Id.column }})
  }
  
  #set arguments based on the across.id argument
  across.which.id <- NULL
  
  #make sure that no shift is applied, if across.id is FALSE
  if(rlang::is_false(across.id)) {
    shift.start <- FALSE
  }
  #if across.id is TRUE, use everything() as the across.id
  else if(rlang::is_true(across.id)) {
    across.which.id <- rlang::expr(tidyr::everything())
  }
  #if across ID is a 
  else {
    across.which.id <- rlang::enexpr(across.id)
    across.id <- TRUE
  }
  
  #make a snapshop of all the grouping keys
  group.keys <- Reference.data %>% dplyr::group_keys() %>% names()
  
   #filter the Reference data if necessary
  if(!is.null(filter.expression.reference)) {
    Reference.data <- 
      Reference.data %>% 
      dplyr::ungroup(rlang::eval_tidy(!!across.which.id)) %>% #here the ungrouping variable must go!
      dplyr::filter(!!filter.expression.reference) %>% 
      dplyr::group_by(dplyr::across(dplyr::all_of(group.keys))) #here the ungrouping variable must go!
    }
    
    #check for Reference data
    if(nrow(Reference.data) == 0) {
      stop("No data left after filters were applied. Please select a less restrictive choice of filter conditions")
  }
  
  #if shift.start = TRUE, here the reference baselines are set
  if(shift.start) {
    #get all the group keys of the reference data
    Reference.start.indices <- 
      Reference.data %>% dplyr::group_keys()
    
    #get all the start dates from the dataset based on the grouping
    start.times <- 
      dataset %>% 
      dplyr::summarise(
        Start.date.shift = min({{ Datetime.column }}) %>% lubridate::as_date())
    
    #join the start dates from the dataset with the start dates from the reference, pick out the Start date shift dates and repeat the vector as long as necessary for all keys
    Reference.start.times <- 
    start.times %>% dplyr::right_join(Reference.start.indices) %>%
      dplyr::mutate(Reference.start.times = Start.date.shift) %>% 
      dplyr::select(-Start.date.shift)
    }
  
   #if shift.intervals is set to "auto", calculate the main difference and half it
  if(rlang::is_true(shift.intervals == "auto")) {
    shift.intervals <- 
      Reference.data %>% 
      dplyr::mutate(
        diff = c(diff({{ Datetime.column }}), NA_real_), .keep = "none",
        .by = NULL) %>% 
      dplyr::count(diff) %>% 
      dplyr::filter(n == max(n)) %>% 
      dplyr::select(-n) %>% 
      dplyr::mutate(diff = lubridate::as.duration(diff)/2) %>% 
      dplyr::pull(diff)
  }

  #create a Reference column and a interval column that is shifted (centered) by shift.intervals
  Reference.data <-
    Reference.data %>%
    sc2interval(
      Statechange.colname = {{ Data.column }},
      length.restriction = length.restriction.seconds,
      full = TRUE,
      State.colname = {{ Reference.column }},
      Datetime.keep = TRUE
      ) %>%
    dplyr::select(dplyr::group_cols(), Interval, {{ Reference.column }}) %>%
    dplyr::mutate(
      Interval =
        lubridate::int_shift(Interval, - shift.intervals[1]), .before = 1)
  
  #making the reference data work across id´s
  if(across.id) {
    #nest the reference data, ungroup it to the desired level and select the data
  Reference.data.nested <- 
    Reference.data %>% 
    tidyr::nest() %>% 
    dplyr::ungroup(rlang::eval_tidy(!!across.which.id)) %>% #here the ungrouping variable must go!
    dplyr::select(data) 
  
   #create a table of all the groups present in the dataset
  Reference.data <- 
    dataset %>% 
    tidyr::nest() %>% dplyr::select(!data)
  
   #join the data to the table of groups present in the dataset
  Reference.data <- 
    #check whether all groups are dropped for the reference
    if(dplyr::group_keys(Reference.data.nested) %>% length() == 0) {
      Reference.data %>% 
      dplyr::mutate(data = list(Reference.data.nested %>% tidyr::unnest(data)))
    }
    #if groups are left, then a different method is used
  else {
    Reference.data %>% 
    dplyr::left_join(Reference.data.nested)
  }
  
  #shifting by start date
  if(shift.start) {
  
    # join the (still nested) Reference data with the start times from the original dataset
    Reference.data <- 
      Reference.data %>% 
      dplyr::left_join(start.times)
    
    # to that, add the start dates of groups in the reference data
    #make a snapshop of all the grouping keys
    group.keys <- Reference.data %>% dplyr::group_keys() %>% names()
    
    Reference.data <- 
    Reference.data %>% 
      dplyr::ungroup(rlang::eval_tidy(!!across.which.id)) %>% 
      dplyr::left_join(Reference.start.times) %>% 
      tidyr::fill(Reference.start.times, .direction = c("downup")) %>% 
      dplyr::group_by(dplyr::across(dplyr::all_of(group.keys)))
    # Reference.data$Reference.Start.date.shift <- Reference.start.times
    
    # now, calculate the necessary shift and remove extra columns
    Reference.data <- 
      Reference.data %>% 
      dplyr::mutate(Shift = Start.date.shift - Reference.start.times) %>% 
      dplyr::select(-Start.date.shift, -Reference.start.times)
  }
  
  # unnest the reference data
  Reference.data <-
    Reference.data %>%
    tidyr::unnest(data)
  }
  
  #Perfom the actual shifting by start date
  if(shift.start) {
    Reference.data <- 
      Reference.data %>% 
      dplyr::mutate(
        Interval =
          lubridate::int_shift(Interval, Shift %>% lubridate::as.period())) %>% 
      dplyr::select(-Shift)
  }
  
  #apply the Reference to the dataset
  dataset <-
    dataset %>%
    interval2state(
      State.interval.dataset = Reference.data,
      State.colname = {{ Reference.column }},
      Id.colname.dataset = {{ Id.column }},
      Id.colname.interval = {{ Id.column }})
  
  #if there is a reference label given, apply it to the dataset
  dataset <-
    create.Reference.label(dataset, {{ Reference.column }}, Reference.label)
  
  # Return --------------------------------------------------------------
  
  dataset
}
