
data2reference <- function(dataset, 
                           Reference.data = dataset,
                           Datetime.column = Datetime,
                           Data.column,
                           Reference.column = Reference,
                           filter.expression.reference = NULL,
                           across.id = FALSE,
                           shift.start = FALSE,
                           length.restriction.seconds = 60,
                           shift.intervals = "auto") {
  
  # Initial Checks ----------------------------------------------------------

  #is grouping between the Reference data and the dataset the same, when not across.id?
  
  
  
  # Manipulation ----------------------------------------------------------
  filter.expression.reference <- rlang::enexpr(filter.expression.reference)
  
   #filter the Reference data if necessary
  if(!is.null(filter.expression.reference)) {
    Reference.data <- 
      Reference.data %>% dplyr::filter(!!filter.expression.reference)
    if(nrow(Reference.data) == 0) 
      stop("No data left after filters were applied. Please select a less restrictive choice of filter conditions")
  }
  
  #if shift.start = TRUE, here the reference baselines are set
  if(shift.start) {
    Reference.start.indices <- 
      Reference.data %>% dplyr::group_keys()
    
    start.times <- 
      dataset %>% 
      dplyr::summarise(
        Start.date.shift = min({{ Datetime.column }}) %>% lubridate::as_date())
    
    Reference.start.times <- 
    start.times %>% dplyr::right_join(Reference.start.indices) %>% 
      dplyr::pull(Start.date.shift) %>% rep(length.out = dplyr::group_keys(dataset) %>% 
                                       nrow())
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
    dplyr::select(Id, Interval, {{ Reference.column }}) %>% 
    dplyr::mutate(
      Interval = 
        lubridate::int_shift(Interval, - shift.intervals[1]), .before = 1)
  
  #making the reference data work across id´s
  if(across.id) {
  Reference.data.nested <- 
    Reference.data %>% 
    tidyr::nest() %>% 
    dplyr::ungroup() %>% 
    dplyr::select(data) %>% 
    tidyr::unnest(data)
  
  Reference.data <- 
    dataset %>% 
    tidyr::nest() %>% 
    dplyr::mutate(data = list(Reference.data.nested))
  
  #shifting by start date
  if(shift.start) {
  
    Reference.data <- 
      Reference.data %>% 
      dplyr::left_join(start.times)
    
    Reference.data$Reference.Start.date.shift <- Reference.start.times
    
    Reference.data <- 
      Reference.data %>% 
      dplyr::mutate(Shift = Start.date.shift - Reference.Start.date.shift) %>% 
      dplyr::select(-Start.date.shift, -Reference.Start.date.shift)
  }
  
  Reference.data <- 
    Reference.data %>% 
    tidyr::unnest(data)
  }
  
  #shifting by start date
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
