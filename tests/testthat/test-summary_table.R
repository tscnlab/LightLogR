test_that("summary_overview reports counts and photoperiod when available", {
  overview <- summary_overview(
    sample.data.environment,
    coordinates = c(48.5, 9.1),
    location = "Tuebingen",
    site = "DE"
  )

  expect_s3_class(overview, "tbl_df")
  expect_true("Participants" %in% overview$name)
  expect_true("Photoperiod" %in% overview$name)
  expect_equal(
    overview$mean[overview$name == "Participants"],
    length(unique(sample.data.environment$Id))
  )
  expect_match(attr(overview, "location_string"), "TZ:")
})

test_that("summary_metrics aggregates daily and participant metrics", {
  metrics <- summary_metrics(sample.data.environment)

  expect_s3_class(metrics, "tbl_df")
  expect_true(all(metrics$type == "Metrics"))
  expect_true(all(c("dose", "dur_above250", "IS", "IV") %in% metrics$name))
  expect_false(any(is.na(metrics$mean)))
})

test_that("summary_table builds a gt table", {
  tbl <- summary_table(
    sample.data.environment,
    coordinates = c(48.5, 9.1),
    location = "Tuebingen",
    site = "DE",
    histograms = FALSE
  )

  expect_s3_class(tbl, "gt_tbl")
  expect_equal(tbl$`_heading`$title, "Summary table")
  expect_true(stringr::str_detect(tbl$`_heading`$subtitle, "TZ:"))
})

test_that("summary_table formats rows by name when photoperiod is missing", {
  tbl <- summary_table(sample.data.irregular, histograms = FALSE)

  expect_false("Photoperiod" %in% tbl$`_data`$name)

  selection <- LightLogR:::format_row_selection(
    tbl$`_data`,
    complete_day_label = glue::glue("Days ≥{round((1 - 0.2) * 100)}% complete")
  )

  expect_false("dose" %in% selection$durations)
  expect_true(all(c("dur_above250", "dur_1_10", "dur_below1", "period_above250", "dur_above1000") %in% selection$durations))
})

