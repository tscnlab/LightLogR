test_that("filtering works", {
  #end datetime and length
  expect_equal(
    {sample.data.environment %>% 
      filter_Datetime(
        end = "2023-08-17 12:00:00", length = lubridate::days(2)) %>% 
      dplyr::pull(Datetime) %>% 
      range()},
    as.POSIXct(c("2023-08-15 12:00:01", "2023-08-17 11:59:51"), tz = "UTC")
  )
  
  #start datetime
  expect_equal(
    {sample.data.environment %>% 
        filter_Datetime(start = "2023-08-17 12:00:00") %>% 
        dplyr::pull(Datetime) %>% 
        range()},
    as.POSIXct(c("2023-08-17 12:00:01", "2023-08-20 23:59:51"), tz = "UTC")
  )
  
  #date
  expect_equal(
    {sample.data.environment %>% 
        filter_Date(end = "2023-08-17") %>% 
        dplyr::pull(Datetime) %>% 
        max()},
    as.POSIXct(c("2023-08-17 23:59:51"), tz = "UTC")
  )
  
  #end date is not the same when using filter_Date and _Datetime
  expect_false(
    identical(
    {sample.data.environment %>% 
        filter_Date(end = "2023-08-17") %>% 
        dplyr::pull(Datetime) %>% 
        max()},
    {sample.data.environment %>% 
            filter_Datetime(end = "2023-08-17") %>% 
            dplyr::pull(Datetime) %>% 
            max()}
  ))
  
})

test_that("giving wrong input types gives an error", {
  #string input of duration
  expect_error(
    sample.data.environment %>% filter_Datetime(length = "2 mins")
  )
  
  #Datetime is not part of the function
  expect_error(
    sample.data.environment %>% 
      filter_Datetime(
        Datetime.colname = Fun,
        start = "2023-08-17 12:00:00")
  )
  
})
