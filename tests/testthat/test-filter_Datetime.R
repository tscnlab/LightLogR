test_that("filtering works", {
  #end datetime and length
  expect_equal(
    {sample.data.environment %>% 
      filter_Datetime(
        end = "2023-08-31 12:00:00", length = lubridate::days(2)) %>% 
      dplyr::pull(Datetime) %>% 
      range()},
    as.POSIXct(c("2023-08-29 12:00:04", "2023-08-31 11:59:54"), 
               tz = "Europe/Berlin")
  )
  
  #start datetime
  expect_equal(
    {sample.data.environment %>% 
        filter_Datetime(start = "2023-08-31 12:00:00") %>% 
        dplyr::pull(Datetime) %>% 
        range()},
    as.POSIXct(c("2023-08-31 12:00:04", "2023-09-03 23:59:44"), 
               tz = "Europe/Berlin")
  )
  
  #date
  expect_equal(
    {sample.data.environment %>% 
        filter_Date(end = "2023-08-31") %>% 
        dplyr::pull(Datetime) %>% 
        max()},
    as.POSIXct(c("2023-08-31 23:59:54"), 
               tz = "Europe/Berlin")
  )
  
  #end date is not the same when using filter_Date and _Datetime
  expect_false(
    identical(
    {sample.data.environment %>% 
        filter_Date(end = "2023-08-31") %>% 
        dplyr::pull(Datetime) %>% 
        max()},
    {sample.data.environment %>% 
            filter_Datetime(end = "2023-08-31") %>% 
            dplyr::pull(Datetime) %>% 
            max()}
  ))
  
  #string input of duration
  expect_equal(
    sample.data.environment %>% filter_Datetime(length = "2 mins") %>% nrow(),
    16
  )
  
})

test_that("giving wrong input types gives an error", {
  
  #Datetime is not part of the function
  expect_error(
    sample.data.environment %>% 
      filter_Datetime(
        Datetime.colname = Fun,
        start = "2023-08-31 12:00:00")
  )
  
})

test_that("filter_multiple_Datetimes works as expected", {
  arguments <- list(
    list(start = "2023-08-31", only_Id = quote(Id == "Participant")),
    list(end = "2023-08-31", only_Id = quote(Id == "Environment")))
  result <- 
    sample.data.environment %>%
    filter_Datetime_multiple(arguments = arguments, filter_Date) %>%
    dplyr::summarize(max = max(Datetime), min = min(Datetime))
  expectation <- 
    tibble::tibble(Id = factor(c("Environment", "Participant")),
                  max = as.POSIXct(c("2023-08-31 23:59:38", "2023-09-03 23:59:44"), 
                                   tz = "Europe/Berlin"),
                  min = as.POSIXct(c("2023-08-29 00:00:08", "2023-08-31 00:00:04"), 
                                   tz = "Europe/Berlin"))
  expect_equal(result, expectation)
})
  