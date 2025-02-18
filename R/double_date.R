# these are all helper functions for double plots
# helper function to select the type of double date, either repeat, next, or auto
double_date <- function(dataset,
                        Datetime.colname = Datetime,
                        type = c("auto", "repeat", "next")
                        ) {

  # Match input arguments
  type <- match.arg(type)
  #select the type of double date
  type <- double_selector(dataset, {{ Datetime.colname }}, type)
  
  #call the correct function depending on type
  switch(type,
         "repeat" = repeat_date(dataset, Datetime.colname = {{ Datetime.colname }}),
         "next" = next_date(dataset, Datetime.colname = {{ Datetime.colname }})
         )
}
  
#helper function that groups by date, then duplicates that day and shifts it by one day
repeat_date <- function(dataset,
                        Datetime.colname = Datetime) {
  #group by date
 dataset <- 
   dataset %>% 
   dplyr::group_by(
     Date.data = lubridate::date({{ Datetime.colname }}), .add = TRUE
     )
 
 #duplicate the data and shift it by one day
 dataset2 <- 
   dataset %>% 
   dplyr::mutate(
     dplyr::across(lubridate::is.POSIXct, \(x) x + lubridate::days(1))
     ) %>% 
   dplyr::ungroup(Date.data)
 
 #combine the two datasets
 dataset <- 
   dplyr::bind_rows(dataset, dataset2) %>% 
   dplyr::arrange({{ Datetime.colname }}, .by_group = TRUE)
 
 dataset
  
}
  
#helper function that makes groups of dates + 1 day
next_date <- function(dataset,
                      Datetime.colname = Datetime) {
  
  #add a Date.data column to the data
  dataset <- 
    dataset %>% 
    dplyr::mutate(
      Date.data = lubridate::date({{ Datetime.colname }})
      )
  
  #create a helper dataset, where the minimum date is filtered out and the remaining dates are shifted by one day back
  dataset2 <-
    dataset %>%
    dplyr::filter(Date.data != min(Date.data)) %>% 
    dplyr::mutate(
      Date.data = Date.data - lubridate::days(1)
      )
   
  #combine the two datasets
  dataset <-
    dplyr::bind_rows(dataset, dataset2) %>%
    dplyr::group_by(Date.data, .add = TRUE) %>%
    dplyr::arrange({{ Datetime.colname }}, .by_group = TRUE)
  
  dataset
}

#helper function to select the type of double date, either repeat, next, or auto
double_selector <- function(dataset, Datetime.colname, type) {
  #if the type is "auto" then determine the type based on the data
  #if there is only one day in the data, then repeat, otherwise next
  if(type == "auto") {
    dates <- dataset %>% 
      dplyr::summarize(
        n_distinct = dplyr::n_distinct(lubridate::date({{ Datetime.colname }}))
      )
    if(all(dates$n_distinct == 1)) {
      type <- "repeat"
    } else {
      type <- "next"
    }
  }
  type
  
}
