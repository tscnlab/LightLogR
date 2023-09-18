test_that("create a new column with time data", {
  expect_equal(
    sample.data.environment %>% 
      create_Time.data() %>% length(), 4)
  
  expect_equal(
    sample.data.environment %>% 
      create_Time.data(Time.data = Test) %>% names() %>% .[4], "Test")
  
  expect_equal(
    sample.data.environment %>% 
      create_Time.data() %>% 
      dplyr::pull(Time.data) %>% 
      hms::is_hms() , TRUE)
})
