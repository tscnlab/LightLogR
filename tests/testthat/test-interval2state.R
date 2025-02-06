test_that("interval2state works as expected", {
  #create a interval dataset
  states <- tibble::tibble(Datetime = c("2023-08-29 6:00:00",
                                        "2023-08-29 23:00:00",
                                        "2023-08-30 6:00:00",
                                        "2023-08-30 22:00:00",
                                        "2023-08-31 6:30:00",
                                        "2023-09-01 1:00:00",
                                        "2023-09-01 6:00:00",
                                        "2023-09-01 22:00:00",
                                        "2023-09-02 6:00:00",
                                        "2023-09-02 23:00:00",
                                        "2023-09-03 6:00:00",
                                        "2023-09-03 22:00:00"),
                           State = rep(c("wake", "sleep"), 6),
                           Id = 
                             rep(c("Participant", "Environment"), each = 6)) %>% 
    dplyr::group_by(Id)
  
  states <- 
  states |> 
    dplyr::mutate(Datetime = 
                    lubridate::force_tz(
                      lubridate::as_datetime(Datetime), tzone = "Europe/Berlin"))
  
  intervals <- sc2interval(states)
  
  #create a dataset with states
  data <-
    sample.data.environment %>%
    interval2state(State.interval.dataset = intervals) %>% 
    dplyr::slice(6000, 28000) %>% dplyr::pull(State)
  
  #test
  expect_equal(data,c(NA, "wake", "sleep"))
  
  #if a state starts before the first datapoint, it is still included in it
  data2 <- 
    suppressWarnings(
      sample.data.environment %>% 
        dplyr::group_by(Id) %>% 
        dplyr::slice(6000, 28000) %>% 
        interval2state(State.interval.dataset = intervals) %>% 
        dplyr::pull(State)
    )
  expect_equal(data2,c(NA, "wake", "sleep"))
})

test_that("interval2state throws errors as expected", {
  #create a interval dataset
  states <- tibble::tibble(Datetime = c("2023-08-29 6:00:00",
                                        "2023-08-29 23:00:00",
                                        "2023-08-30 6:00:00",
                                        "2023-08-30 22:00:00",
                                        "2023-08-31 6:30:00",
                                        "2023-09-01 1:00:00",
                                        "2023-09-01 6:00:00",
                                        "2023-09-01 22:00:00",
                                        "2023-09-02 6:00:00",
                                        "2023-09-02 23:00:00",
                                        "2023-09-03 6:00:00",
                                        "2023-09-03 22:00:00"),
                           State = rep(c("wake", "sleep"), 6),
                           Id = "Participant")
  intervals <- sc2interval(states)
  
  #create a dataset with states
  expect_error(interval2state("Data", State.interval.dataset = intervals),
               "dataset is not a dataframe")
  
  expect_error(interval2state(dataset = sample.data.environment,
                              State.interval.dataset = intervals,
                              Id.colname.dataset = Source),
               "Id.colname.dataset must be part of the dataset")
  
  expect_error(interval2state(dataset = sample.data.environment,
                              Interval.colname = Interval2,
                              State.interval.dataset = intervals),
               "Interval.colname must be part of the State.interval.dataset")
  
  expect_error(interval2state(dataset = sample.data.environment,
                              State.interval.dataset = intervals,
                              Id.colname.interval = Source),
               "Id.colname.interval must be part of the State.interval.dataset")
  
  expect_error(interval2state(dataset = sample.data.environment,
                              State.interval.dataset = sample.data.environment,
                              State.colname = Source,
                              Interval.colname = Datetime))
  
  expect_warning(interval2state(dataset = sample.data.environment %>%
                                  dplyr::mutate(
                                    Datetime = lubridate::with_tz(
                                      Datetime, "Europe/Berlin")
                                  ),
                                State.interval.dataset = 
                                  intervals %>% dplyr::group_by(Id)
                                ),
                 "The time zone of the dataset and the State.interval.dataset are not the same. This might lead to unexpected results or time shifts.")
  
})
