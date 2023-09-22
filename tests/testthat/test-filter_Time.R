test_that("filtering works", {
  #end and length argument
  expect_equal(
    {sample.data.environment %>% 
        filter_Time(
          end = "12:00:00", length = "5:00:00") %>% 
        dplyr::pull(Datetime) %>% 
        range()},
    as.POSIXct(c("2023-08-15 07:00:01", "2023-08-20 11:59:51"), tz = "UTC")
  )
  #start argument
  expect_equal(
    {sample.data.environment %>% 
        filter_Time(start = "12:00:00") %>% 
        dplyr::pull(Datetime) %>% 
        range()},
    as.POSIXct(c("2023-08-15 12:00:01", "2023-08-20 23:59:51"), tz = "UTC")
  )
  
  #length argument
  expect_equal(
    {sample.data.environment %>% 
        filter_Time(length = "12:00:00") %>% 
        dplyr::pull(Datetime) %>% 
        range()},
    as.POSIXct(c("2023-08-15 00:00:01", "2023-08-20 12:00:01"), tz = "UTC")
  )
  
  #giving a POSIXct datetime as a start
  expect_equal(
    sample.data.environment %>% 
      filter_Time(
        start = .$Datetime[1000]) %>% dplyr::pull(Datetime) %>% min(),
    as.POSIXct(c("2023-08-15 02:46:31"), tz = "UTC")
  )
  
})

test_that("giving wrong input types gives an error", {
  #length argument is not correct
  expect_error(
    sample.data.environment %>% filter_Time(length = "2 mins")
  )
  #start argument gets a datetime
  expect_error(
    sample.data.environment %>% 
      filter_Time(
        start = "2023-08-17 12:00:00")
  )
})
