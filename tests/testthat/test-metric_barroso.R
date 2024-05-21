test_that("Calculation works", {
  MEDI = c(rep(0,6), rep(250,4), rep(500,8), rep(0,6))
  Datetime = lubridate::as_datetime(lubridate::dhours(0:23))
  expect_equal(
    barroso_lighting_metrics(MEDI, Datetime),
    list(
      "bright_threshold" = 500 ,
      "dark_threshold" = 0,
      "bright_mean_level" = 500,
      "dark_mean_level" = 0,
      "bright_cluster" = lubridate::dhours(8),
      "dark_cluster" = lubridate::dhours(6),
      "circadian_variation" = 1.10
    )
  )
  expect_equal(
    barroso_lighting_metrics(MEDI, Datetime, loop = TRUE),
    list(
      "bright_threshold" = 500 ,
      "dark_threshold" = 0,
      "bright_mean_level" = 500,
      "dark_mean_level" = 0,
      "bright_cluster" = lubridate::dhours(8),
      "dark_cluster" = lubridate::dhours(12),
      "circadian_variation" = 1.10
    )
  )
})

test_that("Handling of missing values works", {
  MEDI = c(rep(0,6), rep(250,4), rep(500,4), NA, rep(500,3), rep(0,6))
  Datetime = lubridate::as_datetime(lubridate::dhours(0:23))
  expect_equal(
    barroso_lighting_metrics(MEDI, Datetime),
    list(
      "bright_threshold" = as.double(NA),
      "dark_threshold" = as.double(NA),
      "bright_mean_level" = as.double(NA),
      "dark_mean_level" = as.double(NA),
      "bright_cluster" = lubridate::dhours(NA),
      "dark_cluster" = lubridate::dhours(NA),
      "circadian_variation" = as.double(NA)
    )
  )
  expect_equal(
    barroso_lighting_metrics(MEDI, Datetime, na.rm = TRUE),
    list(
      "bright_threshold" = 500,
      "dark_threshold" = 0,
      "bright_mean_level" = 500,
      "dark_mean_level" = 0,
      "bright_cluster" = lubridate::dhours(4),
      "dark_cluster" = lubridate::dhours(6),
      "circadian_variation" = 1.15
    )
  )
})

test_that("Return data frame works", {
  MEDI = c(rep(0,6), rep(250,4), rep(500,8), rep(0,6))
  Datetime = lubridate::as_datetime(lubridate::dhours(0:23))
  expect_equal(
    barroso_lighting_metrics(MEDI, Datetime, as.df = TRUE),
    tibble::tibble(
      "bright_threshold" = 500 ,
      "dark_threshold" = 0,
      "bright_mean_level" = 500,
      "dark_mean_level" = 0,
      "bright_cluster" = lubridate::dhours(8),
      "dark_cluster" = lubridate::dhours(6),
      "circadian_variation" = 1.10
    )
  )
})
