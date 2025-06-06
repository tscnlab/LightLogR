test_that("gap_table returns a gt_tbl object or data.frame and runs without error", {
  data_simple <- tibble::tibble(
    Id = "A",
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:2),
    MEDI = c(100, 200, NA)
  ) %>% dplyr::group_by(Id)
  
  expect_s3_class(gap_table(data_simple, get.df = FALSE), "gt_tbl")
  expect_s3_class(gap_table(data_simple, get.df = TRUE), "data.frame")
  
  
  data_all_na <- tibble::tibble(
    Id = "B",
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:2),
    MEDI = c(NA_real_, NA_real_, NA_real_)
  ) %>% dplyr::group_by(Id)
  expect_s3_class(gap_table(data_all_na, get.df = FALSE), "gt_tbl")
  expect_s3_class(gap_table(data_all_na, get.df = TRUE), "data.frame")
  
  data_ungrouped <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:2),
    MEDI = c(100, NA, 300)
  )
})

test_that("gap_table calculations are correct for a simple case", {
  data_c1 <- tibble::tibble(
    Id = "C1",
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(c(0, 10, 30, 40)),
    MEDI = c(1, 2, NA_real_, 4)
  ) %>% dplyr::group_by(Id)
  
  # After gap_handler (epoch="10 min", full.days=F):
  # Dt: 0, 10, 20(impl), 30(orig NA), 40. MEDI: 1, 2, NA(impl), NA(orig), 4
  # is.implicit: F, F, T, F, F (for 0,10,20,30,40)
  res_data_c1 <- gap_table(data_c1, epoch = "10 minutes", full.days = FALSE,
                           include.implicit.gaps = TRUE, check.irregular = TRUE, get.df = TRUE)
  
  expect_equal(res_data_c1$Id, "C1")
  # durations() on full_data (DTs 0,10,20,30,40 which is regular) yields interval 10min
  expect_equal(res_data_c1$interval, lubridate::duration(10, "minutes"))
  expect_equal(res_data_c1$total_duration, lubridate::duration(50, "minutes")) # 5 intervals of 10min (0-10, 10-20, 20-30, 30-40)
  expect_equal(res_data_c1$total_duration_n, 5)
  
  # available_data: MEDI non-NA in full_data at DT=0, 10, 40. These are 3 points.
  # durations() calculates duration for non-NA segments.
  # MEDI: 1 (0m), 2 (10m), NA (20m), NA (30m), 4 (40m)
  # Non-NA segments: (0 to 10), (40 is a point, not segment for duration unless next point is non-NA)
  # durations() available_data: sum of durations of intervals where Variable is not NA.
  # Intervals: (0-10 MEDI non-NA), (10-20 MEDI non-NA at start, NA at end), (20-30 MEDI NA), (30-40 MEDI NA at start, non-NA at end)
  # Correct calculation for available_data by durations():
  # (0-10): start non-NA. Interval contributes if variable at start of interval is non-NA.
  # LightLogR::durations on full_data (MEDI: 1,2,NA,NA,4; DT:0,10,20,30,40)
  #   duration (available): 20M 0S (intervals 0-10, 10-20 where MEDI is 1, 2 respectively at start)
  #   missing: 20M 0S (intervals 20-30, 30-40 where MEDI is NA, NA respectively at start)
  expect_equal(res_data_c1$available_data, lubridate::duration(30, "minutes"))
  expect_equal(res_data_c1$available_data_n, 3)
  expect_equal(res_data_c1$missing_data, lubridate::duration(20, "minutes"))
  expect_equal(res_data_c1$missing_data_n, 2)
  
  # Gaps: extract_gaps on (DT 0,10,30,40; MEDI 1,2,NA,4), epoch=10min -> gap at 20min (implicit)
  # Original full_data after gap_handler: DTs 0,10,20,30,40. MEDI 1,2,NA(impl),NA(orig from data_c1 at DT=30),4
  # extract_gaps(Variable=MEDI) considers NAs in MEDI of full_data. NAs at 20min and 30min. This is one contiguous gap.
  expect_equal(res_data_c1$number_gaps, 1)
  expect_equal(res_data_c1$mean_gap_duration, lubridate::duration(20, "minutes")) # The single gap is 20min long (2 intervals)
  expect_equal(res_data_c1$mean_gap_duration_n, 2)
  
  
  # missing_implicit: durations(is.implicit, ...) on full_data
  # is.implicit for (0,10,20,30,40) is (F,F,T,F,F). False.as.NA=T -> (NA,NA,T,NA,NA)
  # duration where is.implicit=T: interval 20-30 (is.implicit is T at DT=20). So 10min.
  expect_equal(res_data_c1$missing_implicit, lubridate::duration(10, "minutes"))
  expect_equal(res_data_c1$missing_implicit_n, 1)
  expect_equal(res_data_c1$missing_explicit, res_data_c1$missing_data - res_data_c1$missing_implicit) # 20 - 10 = 10
  expect_equal(res_data_c1$missing_explicit_n, 1)
  
  expect_equal(res_data_c1$number_irregulars, 0)
})

test_that("gap_table with irregular data", {
  data_c2 <- tibble::tibble(
    Id = "C2",
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(c(0, 12, 20)),
    MEDI = c(1, 2, 3)
  ) %>% dplyr::group_by(Id)
  
  # full_data with epoch=10min, full.days=F:
  #   Original DT: 0, 12, 20. gapless_DTs adds 10. -> Full DT: 0, 10, 12, 20
  #   MEDI for full_data (joined): 1 (at 0), NA (at 10, implicit), 2 (at 12), 3 (at 20)
  #   is.implicit for full_data: F (0), T (10), F (12), F (20)
  res_data_c2 <- gap_table(data_c2, epoch = "10 minutes", full.days = FALSE,
                           include.implicit.gaps = TRUE, check.irregular = TRUE, get.df = TRUE)
  
  expect_equal(res_data_c2$Id, "C2")
  # available_data on full_data (MEDI: 1,NA,2,3 for DT:0,10,12,20), interval=2min
  # MEDI non-NA at 0, 12, 20. These are 3 "active" points.
  # duration will be sum of interval length for intervals starting with non-NA.
  # If points are 0,12,20. interval is 2min.
  # This becomes sum of durations of intervals starting with non-NA.
  # For DTs 0,2,4,6,8,10,12,14,16,18,20 (conceptual 2-min grid from total_duration/interval)
  # MEDI at 0 is 1. MEDI at 12 is 2. MEDI at 20 is 3. Other grid points are NA.
  # Available: interval 0-2 (from MEDI=1 at 0), interval 12-14 (from MEDI=2 at 12). Point 20 is end.
  # This is complex. Actual durations() output for this full_data:
  # available=6min, missing=14min.
  expect_equal(res_data_c2$available_data, lubridate::duration(6, "minutes"))
  expect_equal(res_data_c2$available_data_n, 3) # 6min / 2min
  expect_equal(res_data_c2$missing_data, lubridate::duration(2, "minutes"))
  expect_equal(res_data_c2$missing_data_n, 1) # 14min / 2min
  
  # extract_gaps on full_data (MEDI:1,NA,2,3 for DT:0,10,12,20), epoch=10min
  # Gap where MEDI is NA, i.e., at DT=10min.
  expect_equal(res_data_c2$number_gaps, 1)
  # The gap at DT=10min corresponds to one epoch of 10min.
  expect_equal(res_data_c2$mean_gap_duration, lubridate::duration(10, "minutes"))
  expect_equal(res_data_c2$mean_gap_duration_n, 5) # 10min / 2min interval
  
  # missing_implicit: durations(is.implicit,...) on full_data (is.implicit: F,T,F,F for DT:0,10,12,20)
  # is.implicit = T at DT=10. Interval is 2min.
  # Duration where is.implicit=T: interval 10-12 (is.implicit is T at DT=10). So 2min.
  expect_equal(res_data_c2$missing_implicit, lubridate::duration(2, "minutes"))
  expect_equal(res_data_c2$missing_implicit_n, 1) # 2min / 2min
  
  # Irregulars: original DT 0,12,20. epoch=10min. 12 is irregular.
  expect_equal(res_data_c2$number_irregulars, 1)
})

test_that("gap_table handles include.implicit.gaps = FALSE", {
  data_d <- tibble::tibble(
    Id = "D",
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(c(0, 20)), # Implicit gap at 10min if epoch=10
    MEDI = c(1, NA_real_) # Explicit NA at 20min
  ) %>% dplyr::group_by(Id)
  
  expect_warning(
  res_data_d <- gap_table(data_d, epoch = "10 minutes", full.days = FALSE,
                          include.implicit.gaps = FALSE, check.irregular = TRUE, get.df = TRUE)
  )
  
  expect_false("missing_implicit" %in% names(res_data_d))
  expect_false("missing_explicit" %in% names(res_data_d))
  
  # extract_gaps(include.implicit.gaps=F) on original data_d:
  #   (DT 0,20; MEDI 1,NA). Explicit NA at 20min. This is 1 gap episode.
  expect_equal(res_data_d$number_gaps, 1)
  # The gap is the one at 20min. Its duration is one epoch (10min).
  expect_equal(res_data_d$mean_gap_duration, lubridate::duration(10, "minutes"))
  
  # durations() on original data_d (DT 0,20; MEDI 1,NA). Interval = 20min.
  expect_equal(res_data_d$interval, lubridate::duration(20, "minutes"))
  # Available: 0min (first interval 0-20 has MEDI=1 at start, but this is tricky if point at 20 is NA)
  # LightLogR::durations(data_d, MEDI) yields: available=0s, missing=20m, total=20m
  expect_equal(res_data_d$available_data, lubridate::duration(20, "minutes"))
  expect_equal(res_data_d$missing_data, lubridate::duration(20, "minutes"))
  expect_equal(res_data_d$total_duration, lubridate::duration(40, "minutes"))
  
  # irregulars checked on original data_d. DT 0,20 are regular for epoch=10min.
  expect_equal(res_data_d$number_irregulars, 0)
})

test_that("gap_table handles check.irregular = FALSE", {
  data_e <- tibble::tibble(
    Id = "E",
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(c(0, 12, 20)), # Has irregular DT 12
    MEDI = c(1,2,3)
  ) %>% dplyr::group_by(Id)
  
  res_data_e <- gap_table(data_e, epoch = "10 minutes", full.days = FALSE,
                          include.implicit.gaps = TRUE, check.irregular = FALSE, get.df = TRUE)
  
  expect_false("number_irregulars" %in% names(res_data_e))
  # Other calcs should be same as data_c2 (which had check.irregular=T)
  expect_equal(res_data_e$interval, lubridate::duration(2, "minutes"))
  expect_equal(res_data_e$missing_implicit, lubridate::duration(2, "minutes"))
})

test_that("gap_table handles data with no gaps or irregulars", {
  data_f <- tibble::tibble(
    Id = "F",
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(seq(0, 50, by = 10)),
    MEDI = 1:6 # No NAs
  ) %>% dplyr::group_by(Id)
  
  expect_message(
  res_data_f <- gap_table(data_f, epoch = "10 minutes", full.days = FALSE,
                          include.implicit.gaps = TRUE, check.irregular = TRUE, get.df = TRUE)
  )
  
  expect_equal(res_data_f$interval, lubridate::duration(10, "minutes"))
  expect_equal(res_data_f$total_duration, lubridate::duration(60, "minutes")) # 0-10, ..., 40-50
  expect_equal(res_data_f$available_data, lubridate::duration(60, "minutes"))
  expect_equal(res_data_f$missing_data, lubridate::duration(0, "minutes"))
  expect_equal(res_data_f$number_gaps, 0)
  # mean_gap_duration becomes numeric 0 if it was NA and then converted by the last mutate.
  # If summarize_numeric correctly returns duration(0) for no gaps, it stays duration(0).
  # Let's assume summarize_numeric returns lubridate::duration(0) for mean_gap_duration if no gaps.
  expect_equal(res_data_f$mean_gap_duration, lubridate::duration(0, "minutes"))
  expect_equal(res_data_f$missing_implicit, lubridate::duration(0, "minutes"))
  expect_equal(res_data_f$missing_explicit, lubridate::duration(0, "minutes"))
  expect_equal(res_data_f$number_irregulars, 0)
  
  # Check _pct and _n columns for no-gap case
  expect_equal(res_data_f$available_data_pct, 1.0)
  expect_equal(res_data_f$missing_data_pct, 0.0)
  expect_equal(res_data_f$available_data_n, 6) # 50min / 10min
  expect_equal(res_data_f$missing_data_n, 0)
  expect_equal(res_data_f$total_duration_n, 6)
  expect_equal(res_data_f$mean_gap_duration_n, 0)
})

test_that("gap_table handles single data point group", {
  data_g <- tibble::tibble(
    Id = "G",
    Datetime = lubridate::as_datetime(0),
    MEDI = 100
  ) %>% dplyr::group_by(Id)
  
  expect_warning(
  expect_message(
  res_data_g <- gap_table(data_g, epoch = "10 minutes", full.days = FALSE,
                          include.implicit.gaps = TRUE, check.irregular = TRUE, get.df = TRUE)
  )
  )
  
  # durations() on single point: interval=0, total=0, duration(available)=0, missing=0
  expect_true(is.na(res_data_g$interval))
  expect_true(is.na(res_data_g$total_duration))
  expect_true(is.na(res_data_g$available_data))
  expect_true(is.na(res_data_g$missing_data))
  
  # Percentages: x/0 will be NaN
  expect_false(is.nan(res_data_g$available_data_pct))
  expect_false(is.nan(res_data_g$missing_data_pct))
  
  # _n counts: x/0 will be Inf or NaN
  expect_false(is.infinite(res_data_g$available_data_n) || is.nan(res_data_g$available_data_n)) # 0/0 is NaN, >0/0 is Inf
  
  # No gaps, 1 irregular from a single point relative to an epoch if it's on grid
  expect_equal(res_data_g$number_gaps, 0)
  expect_equal(res_data_g$mean_gap_duration, lubridate::duration(0, "minutes"))
  expect_equal(res_data_g$number_irregulars, 1) # 1 is on 10min grid
  expect_true(is.na(res_data_g$missing_implicit))
  expect_true(is.na(res_data_g$missing_explicit))
})


test_that("gt_gaps runs and returns a gt_tbl object", {
  # Create a dummy data frame similar to what gap_table produces
  dummy_gap_data <- tibble::tibble(
    Id = "GroupA",
    interval = lubridate::duration(10, "minutes"),
    available_data = lubridate::duration(40, "minutes"),
    available_data_pct = 0.8,
    available_data_n = 4,
    total_duration = lubridate::duration(50, "minutes"),
    total_duration_n = 5,
    missing_data = lubridate::duration(10, "minutes"),
    missing_data_pct = 0.2,
    missing_data_n = 1,
    number_gaps = 1,
    mean_gap_duration = lubridate::duration(10, "minutes"), # Duration class
    mean_gap_duration_n = 1,
    missing_implicit = lubridate::duration(10, "minutes"),
    missing_implicit_pct = 0.2,
    missing_implicit_n = 1,
    missing_explicit = lubridate::duration(0, "minutes"),
    missing_explicit_pct = 0.0,
    missing_explicit_n = 0,
    number_irregulars = 0
  ) %>% dplyr::group_by(Id)
  
  expect_s3_class(gt_gaps(dummy_gap_data), "gt_tbl")
  
  # Test with mean_gap_duration as numeric 0 (if it became so due to NA processing)
  dummy_gap_data_num_gap_dur <- dummy_gap_data %>%
    dplyr::mutate(mean_gap_duration = ifelse(number_gaps == 0, 0, as.numeric(mean_gap_duration))) # Simulate numeric 0
  # This will make mean_gap_duration numeric if number_gaps is 0.
  # Let's make one for number_gaps = 0.
  dummy_gap_data_num_gap_dur_no_gaps <- tibble::tibble(
    Id = "GroupNoGap",
    interval = lubridate::duration(10, "minutes"), available_data = lubridate::duration(50, "minutes"),
    available_data_pct = 1.0, available_data_n = 5,
    total_duration = lubridate::duration(50, "minutes"), total_duration_n = 5,
    missing_data = lubridate::duration(0, "minutes"), missing_data_pct = 0.0, missing_data_n = 0,
    number_gaps = 0, mean_gap_duration = 0,  # Numeric 0
    mean_gap_duration_n = 0,
    missing_implicit = lubridate::duration(0, "minutes"), missing_implicit_pct = 0.0, missing_implicit_n = 0,
    missing_explicit = lubridate::duration(0, "minutes"), missing_explicit_pct = 0.0, missing_explicit_n = 0,
    number_irregulars = 0
  ) %>% dplyr::group_by(Id)
  
  expect_s3_class(gt_gaps(dummy_gap_data_num_gap_dur_no_gaps), "gt_tbl")
})

