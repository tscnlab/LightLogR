test_that("Circular2Time converts circular columns back to hms", {
  times <- lubridate::as_datetime("2023-01-01 06:00:00") + lubridate::hours(0:2)
  circular_data <- Datetime2Time(tibble::tibble(Time = times), circular = TRUE)
  result <- Circular2Time(circular_data)
  expect_s3_class(result$Time, "hms")
  expect_equal(hms::as_hms(result$Time[1]), hms::as_hms("06:00:00"))
})

test_that("Circular2Time handles data with no circular columns gracefully", {
  test_data <- tibble::tibble(ID = 1:2, Value = c(5, 6))
  expect_message(result <- Circular2Time(test_data), "No columns were affected")
  expect_identical(result, test_data)
})
