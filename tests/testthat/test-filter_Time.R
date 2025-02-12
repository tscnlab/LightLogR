test_that("filtering works", {
  #end and length argument
  expect_equal(
    {sample.data.environment %>% 
        filter_Time(
          end = "12:00:00", length = "5:00:00") %>% 
        dplyr::pull(Datetime) %>% 
        range()},
    as.POSIXct(c("2023-08-29 07:00:04", "2023-09-03 11:59:54"), 
               tz = "Europe/Berlin"))
  #start argument
  expect_equal(
    {sample.data.environment %>% 
        filter_Time(start = "12:00:00") %>% 
        dplyr::pull(Datetime) %>% 
        range()},
    as.POSIXct(c("2023-08-29 12:00:04", "2023-09-03 23:59:54"), 
               tz = "Europe/Berlin")
  )
  
  #length argument
  expect_equal(
    {sample.data.environment %>% 
        filter_Time(length = "12:00:00") %>% 
        dplyr::pull(Datetime) %>% 
        range()},
    as.POSIXct(c("2023-08-29 00:00:04", "2023-09-03 12:00:04"), 
               tz = "Europe/Berlin")
  )
  
  #giving a POSIXct datetime as a start
  expect_equal(
    sample.data.environment %>% 
      filter_Time(
        start = .$Datetime[1000]) %>% dplyr::pull(Datetime) %>% min(),
    as.POSIXct(c("2023-08-29 02:46:34"), 
               tz = "Europe/Berlin")
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
        start = "2023-08-31 12:00:00")
  )
})
