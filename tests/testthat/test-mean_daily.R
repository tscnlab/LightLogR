test_that("mean_daily works with basic numeric data", {
  # Create sample data
  sample_data <- data.frame(
    Date = factor(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), 
                 levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    lux = c(250, 300, 275, 280, 290, 350, 320),
    duration = c(120, 130, 125, 135, 140, 180, 160)
  )
  
  result <- mean_daily(sample_data)
  
  expect_equal(nrow(result), 3)
  expect_equal(result$Date, c("Mean daily", "Weekday", "Weekend"))
  
  # Check calculations
  expect_equal(result$`average_lux`[2], mean(sample_data$lux[1:5]))
  expect_equal(result$`average_lux`[3], mean(sample_data$lux[6:7]))
  expect_equal(result$`average_lux`[1], (mean(sample_data$lux[1:5]) * 5 + mean(sample_data$lux[6:7]) * 2) / 7)
})

test_that("mean_daily handles NA values correctly", {
  sample_data <- data.frame(
    Date = factor(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), 
                 levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    lux = c(250, NA, 275, 280, 290, 350, NA)
  )
  
  # With na.rm = TRUE
  result_remove_na <- mean_daily(sample_data)
  expect_false(is.na(result_remove_na$`average_lux`[1]))
  expect_false(is.na(result_remove_na$`average_lux`[2]))
  
  # With na.rm = FALSE
  result_keep_na <- mean_daily(sample_data, na.rm = FALSE)
  expect_true(is.na(result_keep_na$`average_lux`[1]))
  expect_true(is.na(result_keep_na$`average_lux`[2]))
})

test_that("mean_daily handles Duration objects", {
  
  sample_data <- data.frame(
    Date = factor(c("Mon", "Tue"), levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    duration = c(lubridate::duration(120, "seconds"), lubridate::duration(180, "seconds"))
  )
  
  result <- mean_daily(sample_data)
  
  expect_true(inherits(result$`average_duration`[2], "Duration"))
  expect_equal(as.numeric(result$`average_duration`[2]), 150)
})

test_that("mean_daily calculates from Date column", {
  sample_data <- data.frame(
    Date = seq(as.Date("2023-05-01"), as.Date("2023-05-07"), by = "day"),
    lux = c(250, 300, 275, 280, 290, 350, 320)
  )
  
  result <- mean_daily(sample_data, Weekend.type = DayOfWeek, calculate.from.Date = Date)
  
  expect_equal(result$DayOfWeek, c("Mean daily", "Weekday", "Weekend"))
  expect_equal(nrow(result), 3)
})

test_that("mean_daily handles grouped data", {
  sample_data <- data.frame(
    Group = rep(c("A", "B"), each = 7),
    Date = factor(rep(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), 2), 
                 levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    lux = c(250, 300, 275, 280, 290, 350, 320, 200, 220, 210, 230, 240, 300, 290)
  )
  
  grouped_data <- dplyr::group_by(sample_data, Group)
  result <- mean_daily(grouped_data)
  
  expect_equal(nrow(result), 6)  # 3 rows per group
  expect_equal(unique(result$Group), c("A", "B"))
})

test_that("mean_daily provides informative error for invalid inputs", {
  # Not a dataframe
  expect_error(mean_daily(c(1, 2, 3)), "must be a dataframe")
  
  # Missing Weekend.type column
  sample_data <- data.frame(lux = c(250, 300))
  expect_error(mean_daily(sample_data), "not found in the input data")
  
  # Invalid na.rm
  sample_data <- data.frame(Day = factor(c("Mon", "Tue")), lux = c(250, 300))
  expect_error(mean_daily(sample_data, na.rm = "yes"), "must be a logical value")
})

test_that("mean_daily works with only one day type", {
  # Only weekdays
  weekday_data <- data.frame(
    Date = factor(c("Mon", "Tue", "Wed"), levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    lux = c(250, 300, 275)
  )
  
  result <- mean_daily(weekday_data)
  expect_equal(nrow(result), 3)  # Weekday and Mean daily (no Weekend)
  expect_equal(result$average_lux[1], NaN)
  
  # Only weekend
  weekend_data <- data.frame(
    Date = factor(c("Sat", "Sun"), levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    lux = c(350, 320)
  )
  
  result <- mean_daily(weekend_data)
  expect_equal(nrow(result), 3)  # Weekend and Mean daily (no Weekday)
  expect_equal(result$average_lux[1], NaN)
})

test_that("mean_daily_metric calculates metrics correctly", {
  
  # Mock a simple duration_above_threshold function for testing
  mock_duration_above_threshold <- function(Light.vector, Time.vector, threshold = 100, ...) {
    tibble::tibble(mock = sum(Light.vector > threshold))
  }
  
  # Create sample data
  dates <- seq(as.POSIXct("2023-05-01"), as.POSIXct("2023-05-07"), by = "day")
  sample_data <- data.frame(
    Datetime = dates,
    lux = c(250, 300, 275, 280, 290, 350, 320)
  )
  
  result <- mean_daily_metric(
    data = sample_data,
    Variable = lux,
    metric_type = mock_duration_above_threshold,
    threshold = 100
  )
  
  expect_equal(nrow(result), 3)
  expect_setequal(result$Date, c("Weekday", "Weekend", "Mean daily"))
  expect_true("average_mock" %in% colnames(result))
})

test_that("mean_daily_metric provides informative error for invalid inputs", {
  # Not a dataframe
  expect_error(mean_daily_metric(c(1, 2, 3), Variable = lux), 
               "must be a dataframe")
  
  # Missing Datetime column
  sample_data <- data.frame(lux = c(250, 300))
  expect_error(mean_daily_metric(sample_data, Variable = lux), 
               "not found in the input data")
  
  # metric_type not a function
  sample_data <- data.frame(
    Datetime = seq(as.POSIXct("2023-05-01"), as.POSIXct("2023-05-02"), by = "day"),
    lux = c(250, 300)
  )
  expect_error(mean_daily_metric(sample_data, Variable = lux, 
                                 metric_type = "not_a_function"), 
               "must be a function")
})

test_that("mean_daily_metric works with different metric types", {
  # Mock a different metric function
  mock_metric_function <- function(Light.vector, Time.vector, ...) {
    tibble::tibble(mock = mean(Light.vector))
  }
  
  # Create sample data
  dates <- seq(as.POSIXct("2023-05-01"), as.POSIXct("2023-05-07"), by = "day")
  sample_data <- data.frame(
    Datetime = dates,
    lux = c(250, 300, 275, 280, 290, 350, 320)
  )
  
  result <- mean_daily_metric(
    data = sample_data,
    Variable = lux,
    metric_type = mock_metric_function
  )
  
  expect_equal(nrow(result), 3)
  expect_true("average_mock" %in% colnames(result))
})

test_that("mean_daily_metric handles grouped data", {
  # Mock a simple metric function
  mock_metric_function <- function(Light.vector, Time.vector, ...) {
    tibble::tibble(mock = mean(Light.vector))
  }
  
  # Create sample data with groups
  dates <- seq(as.POSIXct("2023-05-01"), as.POSIXct("2023-05-07"), by = "day")
  sample_data <- data.frame(
    Group = rep(c("A", "B"), each = 7),
    Datetime = rep(dates, 2),
    lux = c(250, 300, 275, 280, 290, 350, 320, 200, 220, 210, 230, 240, 300, 290)
  )
  
  grouped_data <- dplyr::group_by(sample_data, Group)
  result <- mean_daily_metric(
    data = grouped_data,
    Variable = lux,
    metric_type = mock_metric_function
  )
  
  expect_equal(nrow(result), 6)  # 3 rows per group
  expect_equal(unique(result$Group), c("A", "B"))
})

test_that("mean_daily_metric passes additional arguments to metric function", {
  # Mock a metric function that uses additional arguments
  mock_metric_with_args <- function(Light.vector, Time.vector, threshold = 0, multiplier = 1, ...) {
    tibble::tibble(mock = sum(Light.vector > threshold) * multiplier)
  }
  
  # Create sample data
  dates <- seq(as.POSIXct("2023-05-01"), as.POSIXct("2023-05-07"), by = "day")
  sample_data <- data.frame(
    Datetime = dates,
    lux = c(250, 300, 275, 280, 290, 350, 320)
  )
  
  result1 <- mean_daily_metric(
    data = sample_data,
    Variable = lux,
    metric_type = mock_metric_with_args,
    threshold = 300,
    multiplier = 1
  )
  
  result2 <- mean_daily_metric(
    data = sample_data,
    Variable = lux,
    metric_type = mock_metric_with_args,
    threshold = 300,
    multiplier = 2
  )
  
  # Result2 should be double result1 due to the multiplier
  expect_equal(result2$average_mock[1], result1$average_mock[1] * 2)
})
