test_that("interval2state works as expected", {
  #create a interval dataset
  states <- tibble::tibble(Datetime = c("2023-08-15 6:00:00",
                                        "2023-08-15 23:00:00",
                                        "2023-08-16 6:00:00",
                                        "2023-08-16 22:00:00",
                                        "2023-08-17 6:30:00",
                                        "2023-08-18 1:00:00",
                                        "2023-08-18 6:00:00",
                                        "2023-08-18 22:00:00",
                                        "2023-08-19 6:00:00",
                                        "2023-08-19 23:00:00",
                                        "2023-08-20 6:00:00",
                                        "2023-08-20 22:00:00"),
                           State = rep(c("wake", "sleep"), 6),
                           Id = 
                             rep(c("Participant", "Environment"), each = 6)) %>% 
    dplyr::group_by(Id)
  intervals <- sc2interval(states)
  
  #create a dataset with states
  states <-
    sample.data.environment %>% dplyr::group_by(Id) %>%
    interval2state(State.interval.dataset = intervals) %>% 
    dplyr::slice(6000, 28000) %>% dplyr::pull(State)
  
  expect_equal(states,c(NA, "wake", "sleep"))
  
})

test_that("interval2state throws errors as expected", {
  #create a interval dataset
  states <- tibble::tibble(Datetime = c("2023-08-15 6:00:00",
                                        "2023-08-15 23:00:00",
                                        "2023-08-16 6:00:00",
                                        "2023-08-16 22:00:00",
                                        "2023-08-17 6:30:00",
                                        "2023-08-18 1:00:00",
                                        "2023-08-18 6:00:00",
                                        "2023-08-18 22:00:00",
                                        "2023-08-19 6:00:00",
                                        "2023-08-19 23:00:00",
                                        "2023-08-20 6:00:00",
                                        "2023-08-20 22:00:00"),
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
  
})
