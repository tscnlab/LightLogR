test_that("filtering works", {
  expect_equal(
    {sample.data.environment %>% 
      filter_Datetime(
        end = "2023-08-17 12:00:00", length = lubridate::days(2)) %>% 
      dplyr::pull(Datetime) %>% 
      range()},
    as.POSIXct(c("2023-08-15 12:00:01", "2023-08-17 11:59:51"), tz = "UTC")
  )
  
  expect_equal(
    {sample.data.environment %>% 
        filter_Datetime(start = "2023-08-17 12:00:00") %>% 
        dplyr::pull(Datetime) %>% 
        range()},
    as.POSIXct(c("2023-08-17 12:00:01", "2023-08-20 23:59:51"), tz = "UTC")
  )
  
})

test_that("giving wrong input types gives an error", {
  expect_error(
    sample.data.environment %>% filter_Datetime(length = "2 mins")
  )
  expect_error(
    sample.data.environment %>% 
      filter_Datetime(
        Datetime.colname = Fun,
        start = "2023-08-17 12:00:00")
  )
  expect_error(
    sample.data.environment %>% 
      filter_Datetime(
        Datetime.colname = Fun,
        start = "2023-08-17 12:00")
  )
})
