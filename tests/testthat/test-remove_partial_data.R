test_that("Input validation for remove_partial_data", {
  data_ok <- tibble::tibble(
    Id = "A",
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:2),
    Value = 1:3
  ) %>% dplyr::group_by(Id)
  
  expect_error(remove_partial_data(list()), "dataset must be a data frame")
  expect_error(remove_partial_data(data_ok, Variable.colname = NonExistent),
               "Variable.colname 'NonExistent' not found in dataset")
  expect_error(remove_partial_data(data_ok, Datetime.colname = NonExistentDT),
               "Datetime.colname 'NonExistentDT' not found in dataset")
  
  data_bad_dt_type <- data_ok %>% dplyr::mutate(Datetime = as.character(Datetime))
  expect_error(remove_partial_data(data_bad_dt_type),
               "Datetime.colname 'Datetime' must be of type POSIXct")
  
  expect_error(remove_partial_data(data_ok, threshold.missing = -0.1),
               "threshold.missing as percentage must be between 0 and 1")
  expect_error(remove_partial_data(data_ok, threshold.missing = 1.1),
               "threshold.missing as percentage must be between 0 and 1")
  
  expect_error(remove_partial_data(data_ok, show.result = "TRUE"),
               "show.result must be logical")
  expect_error(remove_partial_data(data_ok, handle.gaps = "FALSE"),
               "handle.gaps must be logical")
  expect_error(remove_partial_data(data_ok, by.date = "FALSE"),
               "by.date must be logical")
})

test_that("remove_partial_data with show.result = TRUE", {
  data_test <- tibble::tibble(
    Id = rep(c("X", "Y", "Z"), each = 10),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(rep(seq(0, 90, by = 10), 3)), # 10 pts, 9 intervals of 10min
    Value = c(rep(1, 2), rep(NA, 8),  # X: 8 NA -> 80min missing / 90min total = 0.888 miss_pct
              rep(1, 8), rep(NA, 2),  # Y: 2 NA -> 20min missing / 90min total = 0.222 miss_pct
              rep(1, 10))             # Z: 0 NA -> 0min missing / 90min total = 0 miss_pct
  ) %>% dplyr::group_by(Id)
  
  # Percentage threshold
  res_pct <- remove_partial_data(data_test, Value, threshold.missing = 0.5, show.result = TRUE, handle.gaps = FALSE)
  expect_true(is.data.frame(res_pct))
  expect_true("marked.for.removal" %in% names(res_pct))
  expect_equal(res_pct$marked.for.removal, c(TRUE, FALSE, FALSE)) # X removed, Y Z kept
  expect_equal(res_pct$Id, c("X", "Y", "Z"))
  
  # Duration threshold
  res_dur <- remove_partial_data(data_test, Value, threshold.missing = "30 min", show.result = TRUE, handle.gaps = FALSE)
  # X: 80min missing > 30min. Y: 20min missing < 30min. Z: 0min missing < 30min
  expect_equal(res_dur$marked.for.removal, c(TRUE, FALSE, FALSE))
  
  # handle.gaps = TRUE
  data_implicit_gaps <- tibble::tibble(
    Id = "P",
    Datetime = lubridate::as_datetime(0) + lubridate::hours(c(0, 1, 3, 4)), # implicit gap at 2h
    Value = c(1, 2, NA, 5) # explicit NA at 3h (becomes 2h after gap filling)
  ) %>% dplyr::group_by(Id)
  # gap_handler(full.days=T) on P: DTs 0,1,2,3,4. Values: 1,2,NA(implicit),NA(original),5
  # Total 5 points, 4 intervals.
  # Missing values at DT=2h (implicit) and DT=3h (original).
  # Missing duration is 2 hours. Total duration is 4 hours. missing_pct = 0.5
  res_hg <- remove_partial_data(data_implicit_gaps, Value, threshold.missing = 0.4, show.result = TRUE, handle.gaps = TRUE)
  expect_true(res_hg$marked.for.removal) # 0.5 > 0.4
  
  # by.date = TRUE
  data_daily <- tibble::tibble(
    Id = "A",
    Datetime = lubridate::as_datetime(0) + lubridate::dhours(c(0:2, 24:25)), # Day1: 0-2h (3pts, 2 int), Day2: 24-25h (2pts, 1 int)
    Value = c(1, NA, 3, NA, 5) # Day1: 1 NA at 1h. Day2: 1 NA at 24h
  ) %>% dplyr::group_by(Id)
  # Day1 (date 0): DTs 0,1,2. Vals 1,NA,3. missing 1h, total 2h. pct 0.33
  # Day2 (date 1): DTs 24,25. Vals NA,5. missing 1h, total 1h. pct 0.5
  res_bd <- remove_partial_data(data_daily, Value, threshold.missing = 0.4, show.result = TRUE, by.date = TRUE, handle.gaps = FALSE)
  expect_equal(nrow(res_bd), 2) # One row per Id-Date group
  expect_equal(res_bd$marked.for.removal, c(FALSE, TRUE)) # Day1 kept (0.5 < 0.6), Day2 removed (1.0 > 0.6)
  expect_true(".date" %in% names(res_bd))
  
  # Single point group (marked.for.removal should be TRUE due to NA)
  data_single_pt <- tibble::tibble(Id = "S", Datetime = lubridate::now(), Value = 1) %>% dplyr::group_by(Id)
  
  expect_message(
  expect_message(
    res_sng <- remove_partial_data(data_single_pt, Value, threshold.missing = 0.1, show.result = TRUE)
    )
  )
  expect_true(res_sng$marked.for.removal)
})

test_that("remove_partial_data with show.result = FALSE (dataset filtering)", {
  data_test <- tibble::tibble(
    Id = rep(c("X", "Y", "Z"), each = 10),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(rep(seq(0, 90, by = 10), 3)),
    Value = c(rep(1, 2), rep(NA, 8), rep(1, 8), rep(NA, 2), rep(1, 10))
  ) %>% dplyr::group_by(Id) # X: 88.8% miss, Y: 22.2% miss, Z: 0% miss
  
  # Percentage threshold
  res_pct_filt <- remove_partial_data(data_test, Value, threshold.missing = 0.5, show.result = FALSE)
  expect_equal(unique(res_pct_filt$Id), c("Y", "Z"))
  expect_equal(nrow(res_pct_filt), 20) # Y and Z groups, 10 rows each
  
  # Duration threshold
  res_dur_filt <- remove_partial_data(data_test, Value, threshold.missing = "30 min", show.result = FALSE)
  expect_equal(unique(res_dur_filt$Id), c("Y", "Z")) # X (80min missing) removed
  
  # No groups removed
  res_none_removed <- remove_partial_data(data_test, Value, threshold.missing = 0.9, show.result = FALSE)
  expect_equal(nrow(res_none_removed), 30)
  expect_equal(unique(res_none_removed$Id), c("X", "Y", "Z"))
  
  # All groups removed
  data_all_bad <- tibble::tibble(Id = c("U", "V"), Datetime = lubridate::now(), Value = c(NA, NA)) %>% dplyr::group_by(Id)
  # Each group is a single NA point, will be marked for removal.
  expect_message(
  expect_message(
  expect_message(res_all_removed <- remove_partial_data(data_all_bad, Value, threshold.missing = 0.1, show.result = FALSE),
                 "No groups are left after removing insufficient groups"),
  "This dataset has irregular or singular data")
  )
  # Current behavior returns the summary table, not an empty dataset
  expect_equal(nrow(res_all_removed), 0) # Summary table has 0 rows for U, V
  
  # handle.gaps = FALSE with messages
  data_implicit_gaps_msg <- tibble::tibble(Id = "P", Datetime = lubridate::as_datetime(0) + lubridate::hours(c(0, 2, 6)), Value = 1:3) %>% dplyr::group_by(Id)
  expect_message(remove_partial_data(data_implicit_gaps_msg, Value, handle.gaps = FALSE, show.result = FALSE),
                 "This dataset has implicit gaps.")
  
  data_irreg_msg <- tibble::tibble(Id = "Q", Datetime = lubridate::as_datetime(0) + lubridate::dhours(c(0, 1, 2, 2.5)), Value = 1:4) %>% dplyr::group_by(Id)
  expect_message(remove_partial_data(data_irreg_msg, Value, handle.gaps = FALSE, show.result = FALSE),
                 "This dataset has irregular or singular data.")
  
  # Variable.colname = Datetime (removes single point groups)
  data_single_mix <- tibble::tibble(
    Id = c("S1", "S2", "S2", "S3", "S3", "S3"),
    Datetime = lubridate::as_datetime(0) + lubridate::days(c(0, 10, 11, 20, 21, 22))
  ) %>% dplyr::group_by(Id) # S1 is single point
  expect_message(
    res_dt_var <- remove_partial_data(data_single_mix, Variable.colname = Datetime, threshold.missing = 0.1, show.result = FALSE, handle.gaps = FALSE),
    "This dataset has irregular or singular data")
  expect_equal(unique(res_dt_var$Id), c("S2", "S3"))
  expect_equal(nrow(res_dt_var), 5)
  
  # by.date = TRUE, show.result = FALSE
  data_daily_filt <- tibble::tibble(
    Id = "A",
    Datetime = lubridate::as_datetime(0) + lubridate::hours(c(0:2, 24:25)), # Day1: 0-2h (3pts), Day2: 24-25h (2pts)
    Value = c(1, 1, 3, NA, NA) # Day1: 0% NA. Day2: 100% NA (2 NAs / 2 total points)
  ) %>% dplyr::group_by(Id)
  # Day1: (date 0) DTs 0,1,2. Vals 1,1,3. missing 0h, total 2h. pct 0. Keep if thresh > 0.
  # Day2: (date 1) DTs 24,25. Vals NA,NA. missing 1h (1 interval), total 1h. pct 1.0. Remove if thresh < 1.
  res_bd_filt <- remove_partial_data(data_daily_filt, Value, threshold.missing = 0.6, show.result = FALSE, by.date = TRUE, handle.gaps = FALSE)
  expect_equal(nrow(res_bd_filt), 3) # Only Day1 data for Id A
  expect_equal(unique(lubridate::date(res_bd_filt$Datetime)), lubridate::date(lubridate::as_datetime(0)))
  expect_false(".date" %in% names(res_bd_filt))
  expect_equal(dplyr::group_vars(res_bd_filt), "Id") # Original grouping restored
})

test_that("remove_partial_data handles ungrouped data", {
  data_ungrouped <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:9),
    Value = c(rep(1,2), rep(NA,8)) # 80% missing
  )
  # Ungrouped data is treated as one large group
  res_filt <- remove_partial_data(data_ungrouped, Value, threshold.missing = 0.8, show.result = FALSE)
  expect_message(res_filt_msg <- remove_partial_data(data_ungrouped, Value, threshold.missing = 0.5, show.result = FALSE),
                 "No groups are left after removing insufficient groups")
  res_kept <- remove_partial_data(data_ungrouped, Value, threshold.missing = 0.9, show.result = FALSE)
  expect_equal(nrow(res_kept), 10) # Kept as 0.8 < 0.9
})

test_that("remove_partial_data preserves original grouping if not by.date", {
  data_grp <- tibble::tibble(
    GrpCol = rep(c("G1", "G2"), each = 10),
    Id = rep(c("X", "Y"), each = 5, times = 2),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(rep(seq(0, 40, by = 10), 4)),
    Value = c(rep(1,10), rep(c(1,NA,NA,NA,NA),2)) # G1/X, G1/Y ok. G2/X, G2/Y 80% NA for Value
  ) %>% dplyr::group_by(GrpCol, Id)
  
  # Threshold 0.5. G2 groups (X, Y) should be removed.
  res <- remove_partial_data(data_grp, Value, threshold.missing = 0.5, show.result = FALSE)
  expect_equal(dplyr::group_vars(res), c("GrpCol", "Id"))
  expect_equal(unique(res$GrpCol), "G1")
  expect_equal(nrow(res), 10)
})

test_that("remove_partial_data handles explicit NAs in Variable.colname correctly with handle.gaps = TRUE", {
  # Test case from a user finding where NAs in Value column + handle.gaps = TRUE caused issue
  # with how 'missing' and 'total' were calculated by durations.
  # Durations calculates missing based on NAs in Variable.colname over intervals defined by Datetime.
  # gap_handler(full.days=TRUE) ensures Datetime sequence is dense.
  dataset <- tibble::tibble(
    Id = "Bird1",
    Datetime = lubridate::ymd_hms("2023-01-01 00:00:00") + lubridate::hours(0:5),
    Light = c(10, 20, NA, NA, 5, 1) # 2 NAs out of 6 points (5 intervals)
  ) %>% dplyr::group_by(Id)
  
  # handle.gaps=TRUE, full.days=TRUE for gap_handler
  # Datetime: 0,1,2,3,4,5. Light: 10,20,NA,NA,5,1
  # Epoch determined by dominant_epoch (1 hour).
  # gap_handler will make Datetime 00:00 to 23:00 for that day.
  # Original data 0-5h. So 6 points. Implicit NAs for Light from 6h to 23h (18 points).
  # Total points after gap_handler = 24. Total intervals = 23.
  # Explicit NAs: 2 (at 2h, 3h). Implicit NAs: 18 (from 6h to 23h). Total NAs = 20.
  # Missing duration = 20 * 1hour = 20 hours. Total duration = 23 hours.
  # Missing_pct = 20/23 approx 0.869
  
  # If threshold.missing = 0.5, Bird1 should be removed.
  res_show_true <- remove_partial_data(dataset, Light, threshold.missing = 0.5, handle.gaps = TRUE, show.result = TRUE)
  expect_true(res_show_true$marked.for.removal)
  expect_equal(res_show_true$missing, lubridate::duration(20, "hours"))
  expect_equal(res_show_true$total, lubridate::duration(24, "hours"))
  
  expect_message(res_show_false <- remove_partial_data(dataset, Light, threshold.missing = 0.5, handle.gaps = TRUE, show.result = FALSE))
  
  # If threshold.missing = 0.9, Bird1 should be kept.
  res_kept <- remove_partial_data(dataset, Light, threshold.missing = 0.9, handle.gaps = TRUE, show.result = FALSE)
  expect_equal(nrow(res_kept), 6) # Original 6 rows are kept
  expect_equal(unique(res_kept$Id), "Bird1")
})

