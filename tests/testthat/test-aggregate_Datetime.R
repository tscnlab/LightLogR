test_that("aggregate_Datetime works", {
  
  expect_equal(
    sample.data.environment %>% dplyr::ungroup() %>% 
      aggregate_Datetime(unit = "1 day") %>%
      dominant_epoch() %>% .$dominant.epoch, 
    lubridate::duration(1, "day"))
  
  expect_equal(
    sample.data.environment %>% 
      aggregate_Datetime(unit = "1 day") %>%
      dominant_epoch() %>% .$dominant.epoch, 
    rep(lubridate::duration(1, "day"),2))
  
  expect_equal(
    sample.data.environment %>% 
      aggregate_Datetime() %>% dominant_epoch(),
    sample.data.environment %>% dominant_epoch())
  

})
