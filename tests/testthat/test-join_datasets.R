test_that("join_datasets works as expected",{
  # create test data
  dataset <- 
    tibble::tibble(
      Id = 1,
      Datetime = lubridate::ymd_hms(
                         c("2019-01-01 00:00:00", 
                           "2019-01-01 00:00:01", 
                           "2019-01-01 00:00:02", 
                           "2019-01-01 00:00:03", 
                           "2019-01-01 00:00:04", 
                           "2019-01-01 00:00:05", 
                           "2019-01-01 00:00:06", 
                           "2019-01-01 00:00:07", 
                           "2019-01-01 00:00:08", 
                           "2019-01-01 00:00:09")),
      State = 1
      )
  
  dataset2 <- 
    tibble::tibble(
      Id = 2,
      Datetime = dataset$Datetime,
      State = 2
      )

  # test the function
  expect_equal(dataset %>% join_datasets(dataset2), 
               dataset %>% dplyr::bind_rows(dataset2))
})

test_that("join_datasets gives errors as expected",{
  # create test data
  dataset <- 
    tibble::tibble(
      Id = 1,
      Datetime = lubridate::ymd_hms(
                         c("2019-01-01 00:00:00", 
                           "2019-01-01 00:00:01", 
                           "2019-01-01 00:00:02", 
                           "2019-01-01 00:00:03", 
                           "2019-01-01 00:00:04", 
                           "2019-01-01 00:00:05", 
                           "2019-01-01 00:00:06", 
                           "2019-01-01 00:00:07", 
                           "2019-01-01 00:00:08", 
                           "2019-01-01 00:00:09")),
      State = 1
      )
  
  dataset2 <- 
    tibble::tibble(
      Id = 2,
      Datetime = dataset$Datetime,
      State = 2
      )

  # test the function
  expect_error(dataset %>% join_datasets(dataset2, 
                                         Id.column = "Id2"), 
               "Not all datasets have the required Datetime and ID columns.")
  expect_error(dataset %>% join_datasets(dataset2, 
                                         Datetime.column = "Datetime2"), 
               "Not all datasets have the required Datetime and ID columns.")
  expect_error(dataset %>% join_datasets("State"), 
               "all given datasets must be data.frames")
})
