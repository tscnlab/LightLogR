test_that("aggregate_Date works", {
  dates <- sample.data.environment
  #aggregate_Date works
  expect_no_error(aggregate_Date(dates))
  
  #only one day left
  dates_agg <- aggregate_Date(dates)
  expect_equal(lubridate::date(dates_agg$Datetime) %>% unique() %>% length(), 1)
  #leaving unit = "none" (default) yields different results depending on Id
  expect_equal(dates_agg %>% dominant_epoch() %>% 
                 dplyr::pull(dominant.epoch) %>% 
                 as.numeric, 
               c(30, 10)
               )
  #changing the date.handler to max works
  dates_agg2 <- aggregate_Date(dates, date.handler = max)
  expect_equal(lubridate::date(dates_agg2$Datetime) %>% 
                 unique() %>% 
                 as.character(), 
               "2023-09-03")
  #changing the numeric unit works
  dates_agg3 <- aggregate_Date(dates, unit = "15 mins")
  expect_equal(dates_agg3 %>% 
                 count_difftime() %>% 
                 dplyr::pull(difftime) %>% 
                 as.numeric, 
               c(15*60, 15*60)
               )
  })