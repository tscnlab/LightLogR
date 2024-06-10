
test_that("Calculation works", {
  expect_equal(disparity_index(c(1)), 0)
  expect_equal(disparity_index(c(1,1,1)), 0)
  expect_equal(round(disparity_index(c(100,100,100,100,50,50,50,50)),2), 0.10)
  expect_equal(round(disparity_index(c(100,50,100,50,100,50,100,50)),2), 0.68)
})

test_that("Handling of missing values", {
  expect_equal(disparity_index(c(1,NA,1)), NA)
  expect_equal(round(disparity_index(c(100,100,100,100,NA,50,50,50,50), na.rm = TRUE), 2), 0.10)
})

test_that("Return data frame", {
  expect_equal(round(disparity_index(c(100,100,100,100,50,50,50,50), as.df = TRUE), 2), 
               tibble::tibble("disparity_index" = 0.10))
})

test_that("Input checks", {
  expect_error(disparity_index(c("0","2","1","0")), "`Light.vector` must be numeric!")
  expect_error(disparity_index(c(0,2,1,0,0,2,1,0), na.rm="FALSE"), 
               "`na.rm` must be logical!")
  expect_error(disparity_index(c(0,2,1,0,0,2,1,0), as.df="TRUE"), 
               "`as.df` must be logical!")
})
