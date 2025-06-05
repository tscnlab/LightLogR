test_that("create a new column with time data", {
  expect_equal(
    sample.data.environment %>% 
      add_Time_col() %>% length(), 4)
  
  expect_equal(
    sample.data.environment %>% 
      add_Time_col(Time = Test) %>% names() %>% .[4], "Test")
  
  expect_equal(
    sample.data.environment %>% 
      add_Time_col() %>% 
      dplyr::pull(Time) %>% 
      hms::is_hms() , TRUE)
})
