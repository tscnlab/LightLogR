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
  metrics <- 
    summary_metrics(sample.data.environment |> filter_Date(length = "2 days"),
                    programmatic.use = TRUE)

  expect_s3_class(metrics, "tbl_df")
  expect_true(all(metrics$type == "Metrics"))
  expect_true(all(
    c("dose", "duration_above_250", "interdaily_stability", "intradaily_variability") %in% metrics$name
  ))
  expect_false(any(is.na(metrics$mean)))
})

test_that("summary_table builds a gt table", {
  tbl <- summary_table(
    sample.data.environment |> filter_Date(length = "2 days"),
    coordinates = c(48.5, 9.1),
    location = "Tuebingen",
    site = "DE",
    histograms = FALSE
  )

  expect_s3_class(tbl, "gt_tbl")
  expect_equal(tbl$`_heading`$title, "Summary table")
  expect_true(stringr::str_detect(tbl$`_heading`$subtitle, "TZ:"))
})
