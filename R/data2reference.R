
#' Title
#'
#' @param dataset 
#' @param Reference.data 
#' @param Datetime.column 
#' @param Data.column 
#' @param Reference.column 
#' @param filter.expression.reference 
#' @param across.id 
#' @param shift.start 
#' @param length.restriction.seconds 
#' @param shift.intervals 
#'
#' @return da
#' @export
#'
#' @examples
#' #ex
data2reference <- function(dataset, 
                           Reference.data = dataset,
                           Datetime.column = Datetime,
                           Data.column = MEDI,
                           Reference.column = Reference,
                           filter.expression.reference = NULL,
                           across.id = FALSE,
                           shift.start = FALSE,
                           length.restriction.seconds = 60,
                           shift.intervals = "auto") {
  
  # Initial Checks ----------------------------------------------------------

  Reference.column.str <- colname.defused({{ Reference.column }})
  
  #give an error if the reference column is present
  if(Reference.column.str %in% names(dataset)) 
    stop("A Reference column with the given (or default) name is already part of the dataset. Please remove the column or choose a different name")
  
  #is grouping between the Reference data and the dataset the same, when not across.id?
  across.which.id <- NULL
  
  if(rlang::is_false(across.id)) {
    shift.start <- FALSE
  }
  else if(rlang::is_true(across.id)) {
    across.which.id <- rlang::expr(tidyr::everything())
  }
  else {
    across.which.id <- rlang::enexpr(across.id)
    across.id <- TRUE
  }

  # Manipulation ----------------------------------------------------------
  filter.expression.reference <- rlang::enexpr(filter.expression.reference)
  
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
      full = FALSE,
      State.colname = {{ Reference.column }},
      Datetime.keep = TRUE
      ) %>%
    dplyr::select(Interval, {{ Reference.column }}) %>% 
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
      tidyr::fill(Reference.start.times) %>% 
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
      State.colname = {{ Reference.column }})
  
  # Return --------------------------------------------------------------
  
  dataset
}
