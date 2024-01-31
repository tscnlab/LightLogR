
test_that("Calculation works", {
  expect_equal(frequency_crossing_threshold(c(0,2,1,0,0,2,1,0), 1), 4)
  expect_equal(frequency_crossing_threshold(c(3,2,1,3,3,2,3,1), 1), 0)
})

test_that("Handling of missing values", {
  expect_equal(frequency_crossing_threshold(c(0,2,1,NA,NA,2,1,0), 1), NA)
  expect_equal(frequency_crossing_threshold(c(0,2,1,NA,NA,2,1,0), 1, na.rm = TRUE), 2)
})

test_that("Return data frame", {
  expect_equal(frequency_crossing_threshold(c(0,2,1,0,0,2,1,0), 1, as.df = TRUE), 
               tibble::tibble("frequency_crossing_1" = 4))
})

test_that("Input checks", {
  expect_error(frequency_crossing_threshold(c("0","2","1","0"), 1), 
               "`Light.vector` must be numeric!")
  expect_error(frequency_crossing_threshold(c(0,2,1,0,0,2,1,0), "1"), 
               "`threshold` must be numeric!")
  expect_error(frequency_crossing_threshold(c(0,2,1,0,0,2,1,0), c(1,2)), 
               "`threshold` must be one value!")
  expect_error(frequency_crossing_threshold(c(0,2,1,0,0,2,1,0), 1, na.rm="FALSE"), 
               "`na.rm` must be logical!")
  expect_error(frequency_crossing_threshold(c(0,2,1,0,0,2,1,0), 1, as.df="TRUE"), 
               "`as.df` must be logical!")
})
