test_that("durations calculates basic durations correctly", {
  # Basic functionality test
  result <- durations(sample.data.environment, Variable.colname = MEDI)
  
  expect_s3_class(result, "tbl_df")
  expect_true("duration" %in% names(result))
  
  # Verify against count_difftime results
  dominant_interval <- LightLogR::dominant_epoch(sample.data.environment)$dominant.epoch[1]
  expected_duration <- sample.data.environment |> dplyr::filter(Id == "Environment") |> dplyr::pull(MEDI) |> length() * dominant_interval
  expect_equal(result$duration[1], expected_duration)
})

test_that("durations handles edge cases", {
  # Empty dataset
  empty_df <- tibble::tibble(Datetime = as.POSIXct(character()), MEDI = numeric())
  expect_error(durations(empty_df), NA)
  
  # Single observation
  single_row <- tibble::tibble(Datetime = Sys.time(), MEDI = 100)
  expect_silent(durations(single_row))
})

test_that("parameters work as expected", {
  
  # Test NA handling
  res_missing <- durations(sample.data.environment, 
                           Variable.colname = MEDI, 
                           show.missing = TRUE)
  expect_true(all(c("duration", "missing", "total") %in% names(res_missing)))
  
  # Test interval display
  res_interval <- durations(sample.data.environment, 
                            Variable.colname = MEDI, 
                            show.interval = TRUE)
  expect_equal(res_interval$interval[1], 
               LightLogR::dominant_epoch(sample.data.environment)$dominant.epoch[1])
})

test_that("grouped data handling works", {
  # Create grouped data
  grouped_data <- sample.data.environment %>%
    dplyr::mutate(Group = rep(c("A", "B"), length.out = dplyr::n())) %>%
    dplyr::group_by(Group)
  
  result <- durations(grouped_data, Variable.colname = MEDI)
  
  # Verify group structure
  expect_equal(dplyr::groups(result), dplyr::groups(grouped_data))
  expect_equal(nrow(result), 2)
  
  # Verify group-specific calculations
  group_a_duration <- sum(!is.na(grouped_data$MEDI[grouped_data$Group == "A"])) *
    LightLogR::dominant_epoch(dplyr::filter(grouped_data, Group == "A"))$dominant.epoch[1]
  
  expect_equal(result$duration[result$Group == "A"], group_a_duration)
})

test_that("NA handling works with count.NA", {
  # Create controlled test data
  test_data <- tibble::tibble(
    Datetime = seq.POSIXt(Sys.time(), by = "15 sec", length.out = 100),
    Value = c(rep(NA, 30), rnorm(70))
  )
  
  # Test count.NA = FALSE (default)
  res_default <- durations(test_data, Variable.colname = Value)
  expected_default <- 70 * lubridate::duration(15, "seconds")
  expect_equal(res_default$duration, expected_default)
  
  # Test count.NA = TRUE
  res_count_na <- durations(test_data, Variable.colname = Value, count.NA = TRUE)
  expected_total <- 100 * lubridate::duration(15, "seconds")
  expect_equal(res_count_na$duration, expected_total)
})
