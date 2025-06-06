test_that("gapless_Datetimes works", {
  dataset <- 
    tibble::tibble(Id = c("A", "A", "A", "B", "B", "B"),
                   Datetime = lubridate::as_datetime(1) +
                     lubridate::days(c(0:2, 4, 6, 8)))
  
  expect_equal(dataset %>%
                 dplyr::group_by(Id) %>%
                 gapless_Datetimes() %>% nrow(),
               6)
  expect_equal(dataset %>%
                 dplyr::group_by(Id) %>%
                 gapless_Datetimes(epoch = "1 day") %>% nrow(),
               8)
  expect_equal(dataset %>%
                 gapless_Datetimes() %>% nrow(),
               5)  
  expect_equal(dataset %>%
                 gapless_Datetimes(epoch = "1 day") %>% nrow(),
               9)
  expect_equal(dataset %>%
                 gapless_Datetimes(epoch = "1 day") %>% ncol(),
               1)
  expect_equal(dataset %>%
                 dplyr::group_by(Id) %>%
                 gapless_Datetimes(epoch = "1 day") %>% ncol(),
               2)
  
})

test_that("gapless_Datetimes throws appropriate errors", {
  dataset <- 
    tibble::tibble(Id = c("A", "A", "A", "B", "B", "B"),
                   Datetime = lubridate::as_datetime(1) +
                     lubridate::days(c(0:2, 4, 6, 8)))
  expect_error(dataset$Id %>% gapless_Datetimes(),
               "dataset is not a dataframe")
  expect_error(dataset %>% gapless_Datetimes(Datetime.colname = Status),
               "Datetime.colname must be part of the dataset")
  expect_error(dataset %>% gapless_Datetimes(Datetime.colname = Id),
               "Datetime.colname must be a Datetime")
  expect_error(dataset %>% gapless_Datetimes(epoch = 5),
               "epoch must either be a duration or a string")
})


test_that("gap_handler works", {
  dataset <-
  tibble::tibble(Id = c("A", "A", "A", "B", "B", "B"),
                Datetime = lubridate::as_datetime(1) +
                           lubridate::days(c(0:2, 4, 6, 8)) +
                           lubridate::hours(c(0,12,rep(0,4))))
  
  expect_equal(dataset %>% gap_handler(epoch = "1 day") %>% dim(),c(10,3))
  expect_equal(dataset %>% 
                 gap_handler(epoch = "1 day", behavior = "irregulars") %>% 
                 dim(),c(1,3))
  expect_equal(dataset %>% 
                 gap_handler(epoch = "1 day", behavior = "gaps") %>% dim(),
               c(4,1))
  expect_equal(dataset %>% 
                 gap_handler(epoch = "1 day", behavior = "regulars") %>% dim(),
               c(5,3))
  expect_equal(dataset %>% dplyr::group_by(Id) %>% 
                 gap_handler(epoch = "1 day") %>% dim(),c(9,3))
  expect_equal(dataset %>% dplyr::group_by(Id) %>% 
                 gap_handler(epoch = "1 day", behavior = "irregulars") %>% 
                 dim(),c(1,3))
  expect_equal(dataset %>% dplyr::group_by(Id) %>% 
                 gap_handler(epoch = "1 day", behavior = "gaps") %>% dim(),
               c(3,2))
  expect_equal(dataset %>% dplyr::group_by(Id) %>% 
                 gap_handler(epoch = "1 day", behavior = "regulars") %>% dim(),
               c(5,3))

})

test_that("gapless_Datetimes throws appropriate errors", {
  dataset <- 
    tibble::tibble(Id = c("A", "A", "A", "B", "B", "B"),
                   Datetime = lubridate::as_datetime(1) +
                     lubridate::days(c(0:2, 4, 6, 8)))
  expect_error(dataset$Id %>% gap_handler(),
               "dataset is not a dataframe")
  expect_error(dataset %>% gap_handler(Datetime.colname = Status),
               "Datetime.colname must be part of the dataset")
  expect_error(dataset %>% gap_handler(Datetime.colname = Id),
               "Datetime.colname must be a Datetime")
  expect_error(dataset %>% gap_handler(epoch = 5),
               "epoch must either be a duration or a string")
  expect_error(dataset %>% gap_handler(behavior = "foo"),
               "'arg' should be one of ")
  
})

test_that("gap_finder works", {
  dataset <-
    tibble::tibble(Id = c("A", "A", "A", "B", "B", "B"),
                   Datetime = lubridate::as_datetime(1) +
                     lubridate::days(c(0:2, 4, 6, 8)) +
                     lubridate::hours(c(0,12,rep(0,4)))) %>% 
    dplyr::group_by(Id)
  
  expect_equal(gap_finder(dataset, gap.data = TRUE, silent = TRUE) %>% 
                 dim(),c(2,3))
  testthat::expect_message(gap_finder(dataset), 
                           regexp = "Found 2 gaps. 6 Datetimes fall into the regular sequence.")
  testthat::expect_message(gap_finder(dataset, epoch = "1 day"), 
                           regexp = "Found 3 gaps. 5 Datetimes fall into the regular sequence.")
})

#-----------

test_that("has_gaps works with ungrouped data", {
  # No gaps
  data_no_gaps <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:5)
  )
  expect_false(has_gaps(data_no_gaps, epoch = "1 hour"))
  
  # With gaps
  data_with_gaps <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::hours(c(0, 1, 3, 5)) # gap between 1 and 3, 3 and 5
  )
  expect_true(has_gaps(data_with_gaps, epoch = "1 hour"))
  
  # Custom Datetime column name
  data_custom_dt <- tibble::tibble(
    MyTime = lubridate::as_datetime(0) + lubridate::days(c(0, 2))
  )
  expect_true(has_gaps(data_custom_dt, Datetime.colname = MyTime, epoch = "1 day"))
})

test_that("has_gaps works with grouped data", {
  # No gaps within groups
  data_grouped_no_gaps <- tibble::tibble(
    Id = rep(c("A", "B"), each = 3),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(c(0:2, 0:2))
  ) %>% dplyr::group_by(Id)
  expect_false(has_gaps(data_grouped_no_gaps, epoch = "1 min"))
  
  # Gaps in one group
  data_grouped_gaps_one <- tibble::tibble(
    Id = rep(c("A", "B"), each = 3),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(c(0, 1, 3, 0, 1, 2)) # gap in A
  ) %>% dplyr::group_by(Id)
  expect_true(has_gaps(data_grouped_gaps_one, epoch = "1 min"))
  
  # Gaps in all groups
  data_grouped_gaps_all <- tibble::tibble(
    Id = rep(c("A", "B"), each = 4),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(c(0, 2, 4, 8, 10, 12, 14, 18)) # gaps in A and B
  ) %>% dplyr::group_by(Id)
  expect_true(has_gaps(data_grouped_gaps_all, epoch = "2 mins"))
  
  
  # Groups span unconnected parts - considered a gap by default epoch
  data_unconnected_groups <- tibble::tibble(
    State = c("X", "X", "Y", "Y", "X", "X"),
    Datetime = lubridate::as_datetime(0) + lubridate::hours(c(0, 1, 10, 11, 20, 21))
  ) %>% dplyr::group_by(State)
  # dominant epoch for X will be 1 hour, but there's a large jump
  # dominant epoch for Y will be 1 hour.
  expect_true(has_gaps(data_unconnected_groups))
  
  
  # Single observation in a group
  dataset_single_obs_group <- tibble::tibble(
    Id = c("A", "A", "B"),
    Datetime = lubridate::as_datetime(0) + lubridate::days(c(0, 1, 0))
  ) %>% dplyr::group_by(Id)
  # Group B has only one observation, gap_handler filters it out, so no gaps reported for it.
  # Group A has no gaps.
  expect_false(has_gaps(dataset_single_obs_group, epoch = "1 day"))
  
  dataset_single_obs_group_with_gap <- tibble::tibble(
    Id = c("A", "A", "B", "A"),
    Datetime = lubridate::as_datetime(0) + lubridate::days(c(0, 2, 0, 3)) # A: 0,2,3 (gap); B: 0
  ) %>% dplyr::group_by(Id)
  expect_true(has_gaps(dataset_single_obs_group_with_gap, epoch = "1 day"))
  
})

test_that("has_gaps handles epoch correctly", {
  data <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::days(c(0, 2, 3)) # gap if epoch is 1 day
  )
  expect_true(has_gaps(data, epoch = "1 day"))
  expect_true(has_gaps(data, epoch = "dominant.epoch"))
  
  data2 <- tibble::tibble(Datetime = lubridate::as_datetime(0) + lubridate::hours(c(0,2,4)))
  expect_false(has_gaps(data2, epoch = "2 hours"))
  expect_true(has_gaps(data2, epoch = "1 hour"))
  expect_false(has_gaps(data2, epoch = lubridate::duration(2, "hours")))
})

test_that("has_gaps error handling", {
  expect_error(has_gaps(list()), "dataset is not a dataframe")
  data <- tibble::tibble(D = lubridate::as_datetime(0))
  expect_error(has_gaps(data, Datetime.colname = NotDatetime), "Datetime.colname must be part of the dataset")
  data_wrong_type <- tibble::tibble(Datetime = 1:3)
  expect_error(has_gaps(data_wrong_type), "Datetime.colname must be a Datetime")
  data_ok <- tibble::tibble(Datetime = lubridate::as_datetime(0))
  expect_error(has_gaps(data_ok, epoch = 123), "epoch must either be a duration or a string")
})

# Tests for has_irregulars

test_that("has_irregulars works with ungrouped data", {
  # No irregulars
  data_no_irregulars <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:5)
  )
  expect_false(has_irregulars(data_no_irregulars, epoch = "1 hour"))
  
  # With irregulars
  data_with_irregulars <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::dhours(c(0, 1.5, 2, 3.1, 4))
  )
  expect_true(has_irregulars(data_with_irregulars, epoch = "1 hour"))
  
  # Custom Datetime column name
  data_custom_dt <- tibble::tibble(
    MyTime = lubridate::as_datetime(0) + lubridate::ddays(c(0, 1.2, 2))
  )
  expect_true(has_irregulars(data_custom_dt, Datetime.colname = MyTime, epoch = "1 day"))
})

test_that("has_irregulars works with grouped data", {
  # No irregulars within groups
  data_grouped_no_irregulars <- tibble::tibble(
    Id = rep(c("A", "B"), each = 3),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(c(0:2, 10:12))
  ) %>% dplyr::group_by(Id)
  expect_false(has_irregulars(data_grouped_no_irregulars, epoch = "1 min"))
  
  # Irregulars in one group
  data_grouped_irregulars_one <- tibble::tibble(
    Id = rep(c("A", "B"), each = 3),
    Datetime = lubridate::as_datetime(0) + lubridate::dminutes(c(0, 1.5, 2, 10, 11, 12)) # irregular in A
  ) %>% dplyr::group_by(Id)
  expect_true(has_irregulars(data_grouped_irregulars_one, epoch = "1 min"))
  
  # Irregulars in all groups
  data_grouped_irregulars_all <- tibble::tibble(
    Id = rep(c("A", "B"), each = 3),
    Datetime = lubridate::as_datetime(0) + lubridate::dminutes(c(0, 2.2, 4, 10.1, 12, 14.9))
  ) %>% dplyr::group_by(Id)
  expect_true(has_irregulars(data_grouped_irregulars_all, epoch = "2 mins"))
  
  # Single observation in a group (cannot be irregular by itself with gapless_Datetimes current logic)
  dataset_single_obs_group <- tibble::tibble(
    Id = c("A", "A", "B"),
    Datetime = lubridate::as_datetime(0) + lubridate::ddays(c(0, 1, 0.5)) # B has 0.5
  ) %>% dplyr::group_by(Id)
  # Group B will be filtered by gapless_Datetimes, but remains in the output of gap_handler. Group A is regular.
  expect_true(has_irregulars(dataset_single_obs_group, epoch = "1 day"))
  
  
  dataset_single_obs_group_irreg <- tibble::tibble(
    Id = c("A", "A", "B", "A"),
    Datetime = lubridate::as_datetime(0) + lubridate::ddays(c(0, 1.5, 0, 2)) # A: 0, 1.5, 2 (1.5 is irreg); B: 0
  ) %>% dplyr::group_by(Id)
  # Group B is filtered by gapless_Datetimes. Group A has 1.5 which is irregular.
  expect_true(has_irregulars(dataset_single_obs_group_irreg, epoch = "1 day"))
})


test_that("has_irregulars handles epoch correctly", {
  data <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::dhours(c(0, 1, 2.5, 4)) # 2.5 is irregular if epoch is 1 hour
  )
  expect_true(has_irregulars(data, epoch = "1 hour"))
  expect_false(has_irregulars(data, epoch = "30 mins")) # 0,1,2.5,4 -> 0, 60, 150, 240 mins. All are multiples of 30min
  expect_true(has_irregulars(data, epoch = lubridate::duration(1, "hour")))
  
  data2 <- tibble::tibble(Datetime = lubridate::as_datetime(0) + lubridate::dhours(c(0,2,4.1)))
  expect_true(has_irregulars(data2, epoch = "2 hours")) # 4.1 is irregular
  expect_true(has_irregulars(data2, epoch = "1 hour")) # 4.1 is irregular
})

test_that("has_irregulars error handling", {
  expect_error(has_irregulars(list()), "dataset is not a dataframe")
  data <- tibble::tibble(D = lubridate::as_datetime(0))
  expect_error(has_irregulars(data, Datetime.colname = NotDatetime), "Datetime.colname must be part of the dataset")
  data_wrong_type <- tibble::tibble(Datetime = 1:3)
  expect_error(has_irregulars(data_wrong_type), "Datetime.colname must be a Datetime")
  data_ok <- tibble::tibble(Datetime = lubridate::as_datetime(0))
  expect_error(has_irregulars(data_ok, epoch = 123), "epoch must either be a duration or a string")
})

# Tests for extract_gaps

test_that("extract_gaps handles implicit gaps correctly (no Variable.colname)", {
  # No gaps
  data_no_gaps <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:2)
  )
  expect_message(result_no_gaps <- extract_gaps(data_no_gaps, epoch = "1 hour", full.days = FALSE), "No implicit gaps found")
  expect_equal(nrow(result_no_gaps), 0)
  
  # One gap
  data_one_gap <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::hours(c(0, 2)) # gap at 1 hour
  )
  result_one_gap <- extract_gaps(data_one_gap, epoch = "1 hour", full.days = FALSE)
  expect_equal(nrow(result_one_gap), 1)
  expect_equal(result_one_gap$gap.id, 1)
  expect_equal(result_one_gap$start, lubridate::as_datetime(0) + lubridate::hours(1) - lubridate::duration(30, "minutes"))
  expect_equal(result_one_gap$end, lubridate::as_datetime(0) + lubridate::hours(1) + lubridate::duration(30, "minutes"))
  expect_equal(result_one_gap$duration, lubridate::duration(1, "hour"))
  
  # Multiple gaps
  data_multi_gaps <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::hours(c(0, 3, 4, 7)) # gaps at 1,2 and 5,6
  )
  result_multi_gaps <- extract_gaps(data_multi_gaps, epoch = "1 hour", full.days = FALSE)
  expect_equal(nrow(result_multi_gaps), 2)
  expect_equal(result_multi_gaps$gap.id, c(1, 2))
  expect_equal(result_multi_gaps$start[1], lubridate::as_datetime(0) + lubridate::hours(1) - lubridate::duration(30, "minutes"))
  expect_equal(result_multi_gaps$end[1], lubridate::as_datetime(0) + lubridate::hours(2) + lubridate::duration(30, "minutes"))
  expect_equal(result_multi_gaps$duration[1], lubridate::duration(2, "hours"))
  expect_equal(result_multi_gaps$start[2], lubridate::as_datetime(0) + lubridate::hours(5) - lubridate::duration(30, "minutes"))
  expect_equal(result_multi_gaps$end[2], lubridate::as_datetime(0) + lubridate::hours(6) + lubridate::duration(30, "minutes"))
  expect_equal(result_multi_gaps$duration[2], lubridate::duration(2, "hours"))
  
  # full.days = TRUE
  data_fd <- tibble::tibble(Datetime = lubridate::as_datetime("2023-01-01 10:00:00") + lubridate::days(c(0,2))) # gap on 2023-01-02
  res_fd <- extract_gaps(data_fd, epoch="10 hours", full.days = TRUE)
  expect_equal(nrow(res_fd), 3)
  expect_equal(res_fd$start[1], lubridate::as_datetime("2023-01-01 00:00:00") - lubridate::duration(5, "hours"))
  expect_equal(res_fd$end[1],lubridate::as_datetime("2023-01-01 00:00:00") + lubridate::duration(5, "hours")) # seq creates end of day, then + epoch/2
  
})

test_that("extract_gaps handles explicit gaps (with Variable.colname)", {
  # No NA, no implicit gaps
  data_no_na_no_implicit <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:2),
    Value = c(10, 20, 30)
  )
  expect_message(result_no_na <- extract_gaps(data_no_na_no_implicit, Value, epoch = "1 hour", include.implicit.gaps = FALSE, full.days=FALSE), "No gaps found")
  expect_equal(nrow(result_no_na), 0)
  
  # Has NA, no implicit gaps
  data_has_na_no_implicit <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:3),
    Value = c(10, NA, NA, 40)
  )
  expect_warning( # due to include.implicit.gaps = FALSE and has_gaps being false
    result_has_na <- extract_gaps(data_has_na_no_implicit, Value, epoch = "1 hour", include.implicit.gaps = FALSE, full.days=FALSE),
    NA # No warning because no implicit gaps
  )
  
  expect_equal(nrow(result_has_na), 1)
  expect_equal(result_has_na$gap.id, 1)
  expect_equal(result_has_na$start, lubridate::as_datetime(0) + lubridate::hours(1) - lubridate::duration(30, "minutes")) # epoch/2 before first NA
  expect_equal(result_has_na$end, lubridate::as_datetime(0) + lubridate::hours(2) + lubridate::duration(30, "minutes"))   # epoch/2 after last NA
  expect_equal(result_has_na$duration, lubridate::duration(2, "hours"))
  
  # No NA, but has implicit gaps, include.implicit.gaps = TRUE
  data_no_na_has_implicit_true <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::hours(c(0, 2)), # implicit gap at 1h
    Value = c(10, 30)
  )
  result_implicit_true <- extract_gaps(data_no_na_has_implicit_true, Value, epoch = "1 hour", include.implicit.gaps = TRUE, full.days=FALSE)
  # gap_handler adds Datetime at 1h, Value is NA. This is one gap.
  expect_equal(nrow(result_implicit_true), 1)
  expect_equal(result_implicit_true$gap.id, 1)
  expect_equal(result_implicit_true$start, lubridate::as_datetime(0) + lubridate::hours(1) - lubridate::duration(30, "minutes"))
  expect_equal(result_implicit_true$end, lubridate::as_datetime(0) + lubridate::hours(1) + lubridate::duration(30, "minutes"))
  expect_equal(result_implicit_true$duration, lubridate::duration(1, "hour"))
  
  # No NA, but has implicit gaps, include.implicit.gaps = FALSE
  data_no_na_has_implicit_false <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::hours(c(0, 2)), # implicit gap at 1h
    Value = c(10, 30)
  )
  expect_warning(
    expect_message(
    result_implicit_false <- extract_gaps(data_no_na_has_implicit_false, Value, epoch = "1 hour", include.implicit.gaps = FALSE, full.days=FALSE)),
    "There are implicit gaps in the dataset that will not be part of the extracted summary"
  )
  expect_equal(nrow(result_implicit_false), 0)
  
  
  # Has NA AND implicit gaps, include.implicit.gaps = TRUE
  data_na_and_implicit_true <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::hours(c(0, 1, 3, 5)), # implicit gap at 2h, 4h
    Value = c(10, NA, 30, NA) # NA at 1h (explicit), NA at 5h (explicit)
  ) # After gap_handler: DTs: 0,1,2,3,4,5; Vals: 10,NA,NA(implicit),30,NA(implicit),NA
  # Gaps: (1h, 2h), (4h, 5h)
  result_na_implicit_true <- extract_gaps(data_na_and_implicit_true, Value, epoch = "1 hour", include.implicit.gaps = TRUE, full.days=FALSE)
  expect_equal(nrow(result_na_implicit_true), 2)
  expect_equal(result_na_implicit_true$gap.id, c(1,2))
  # Gap 1: Dts 1h (NA), 2h (implicit NA)
  expect_equal(result_na_implicit_true$start[1], lubridate::as_datetime(0) + lubridate::hours(1) - lubridate::duration(30, "minutes"))
  expect_equal(result_na_implicit_true$end[1], lubridate::as_datetime(0) + lubridate::hours(2) + lubridate::duration(30, "minutes"))
  expect_equal(result_na_implicit_true$duration[1], lubridate::duration(2, "hours"))
  # Gap 2: Dts 4h (implicit NA), 5h (NA)
  expect_equal(result_na_implicit_true$start[2], lubridate::as_datetime(0) + lubridate::hours(4) - lubridate::duration(30, "minutes"))
  expect_equal(result_na_implicit_true$end[2], lubridate::as_datetime(0) + lubridate::hours(5) + lubridate::duration(30, "minutes"))
  expect_equal(result_na_implicit_true$duration[2], lubridate::duration(2, "hours"))
  
  
  # Has NA AND implicit gaps, include.implicit.gaps = FALSE
  data_na_and_implicit_false <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::hours(c(0, 1, 3, 5)), # implicit gap at 2h, 4h
    Value = c(10, NA, 30, NA) # NA at 1h, NA at 5h
  )
  expect_warning(
    result_na_implicit_false <- extract_gaps(data_na_and_implicit_false, Value, epoch = "1 hour", include.implicit.gaps = FALSE, full.days=FALSE),
    "There are implicit gaps in the dataset that will not be part of the extracted summary"
  )
  # Only considers original NAs at 1h and 5h. These are separate gaps as per extract_states logic on original data.
  expect_equal(nrow(result_na_implicit_false), 2)
  expect_equal(result_na_implicit_false$gap.id, c(1,2))
  expect_equal(result_na_implicit_false$start[1], lubridate::as_datetime(0) + lubridate::hours(1) - lubridate::duration(30, "minutes"))
  expect_equal(result_na_implicit_false$end[1], lubridate::as_datetime(0) + lubridate::hours(1) + lubridate::duration(30, "minutes"))
  expect_equal(result_na_implicit_false$duration[1], lubridate::duration(1, "hour"))
  expect_equal(result_na_implicit_false$start[2], lubridate::as_datetime(0) + lubridate::hours(5) - lubridate::duration(30, "minutes"))
  expect_equal(result_na_implicit_false$end[2], lubridate::as_datetime(0) + lubridate::hours(5) + lubridate::duration(30, "minutes"))
  expect_equal(result_na_implicit_false$duration[2], lubridate::duration(1, "hour"))
})


test_that("extract_gaps works with grouped data", {
  # Implicit gaps, grouped
  data_grouped_implicit <- tibble::tibble(
    Id = rep(c("A", "B"), each = 2),
    Datetime = lubridate::as_datetime(0) + lubridate::hours(c(0, 2, 10, 12)) # gap at 1h for A, 11h for B
  ) %>% dplyr::group_by(Id)
  result_grp_implicit <- extract_gaps(data_grouped_implicit, epoch = "1 hour", full.days = FALSE)
  expect_equal(nrow(result_grp_implicit), 2)
  expect_equal(result_grp_implicit$Id, c("A", "B"))
  expect_equal(result_grp_implicit$gap.id, c(1, 1)) # gap.id is per group here
  expect_equal(result_grp_implicit$start[result_grp_implicit$Id=="A"], lubridate::as_datetime(0) + lubridate::hours(1) - lubridate::duration(30, "minutes"))
  expect_equal(result_grp_implicit$duration[result_grp_implicit$Id=="A"], lubridate::duration(1, "hour"))
  expect_equal(result_grp_implicit$start[result_grp_implicit$Id=="B"], lubridate::as_datetime(0) + lubridate::hours(11) - lubridate::duration(30, "minutes"))
  expect_equal(result_grp_implicit$duration[result_grp_implicit$Id=="B"], lubridate::duration(1, "hour"))
  
  # Explicit gaps, grouped, include.implicit.gaps = TRUE
  data_grouped_explicit_implicit_T <- tibble::tibble(
    Id = rep(c("A", "B"), each = 3),
    Datetime = lubridate::as_datetime(0) + lubridate::hours(c(0, 1, 3, 10, 12, 13)), # A implicit at 2h; B implicit at 11h
    Value = c(1, NA, 3, 10, NA, 30) # A explicit NA at 1h; B explicit NA at 12h
  ) %>% dplyr::group_by(Id)
  # For A: gap_handler DTs: 0,1,2,3; Vals: 1,NA,NA(impl),3 -> Gaps at 1h,2h
  # For B: gap_handler DTs: 10,11,12,13; Vals: 10,NA(impl),NA,30 -> Gaps at 11h,12h
  result_grp_exp_impl_T <- extract_gaps(data_grouped_explicit_implicit_T, Value, epoch = "1 hour", include.implicit.gaps = TRUE, full.days = FALSE)
  expect_equal(nrow(result_grp_exp_impl_T), 2) # One combined gap per group
  expect_equal(result_grp_exp_impl_T$Id, c("A", "B"))
  expect_equal(result_grp_exp_impl_T$gap.id, c(1,1))
  
  # Check A's gap (1h explicit, 2h implicit)
  res_A <- dplyr::filter(result_grp_exp_impl_T, Id == "A")
  expect_equal(res_A$start, lubridate::as_datetime(0) + lubridate::hours(1) - lubridate::duration(30, "minutes"))
  expect_equal(res_A$end, lubridate::as_datetime(0) + lubridate::hours(2) + lubridate::duration(30, "minutes"))
  expect_equal(res_A$duration, lubridate::duration(2, "hours"))
  
  # Check B's gap (11h implicit, 12h explicit)
  res_B <- dplyr::filter(result_grp_exp_impl_T, Id == "B")
  expect_equal(res_B$start, lubridate::as_datetime(0) + lubridate::hours(11) - lubridate::duration(30, "minutes"))
  expect_equal(res_B$end, lubridate::as_datetime(0) + lubridate::hours(12) + lubridate::duration(30, "minutes"))
  expect_equal(res_B$duration, lubridate::duration(2, "hours"))
  
  
  # Explicit gaps, grouped, include.implicit.gaps = FALSE
  data_grouped_explicit_implicit_F <- tibble::tibble(
    Id = rep(c("A", "B"), each = 3),
    Datetime = lubridate::as_datetime(0) + lubridate::hours(c(0, 1, 3, 10, 12, 13)), # A implicit at 2h; B implicit at 11h
    Value = c(1, NA, 3, 10, NA, 30) # A explicit NA at 1h; B explicit NA at 12h
  ) %>% dplyr::group_by(Id)
  
  expect_warning(
    result_grp_exp_impl_F <- extract_gaps(data_grouped_explicit_implicit_F, Value, epoch = "1 hour", include.implicit.gaps = FALSE, full.days = FALSE),
    "There are implicit gaps in the dataset that will not be part of the extracted summary"
  )
  # Only original NAs considered.
  # For A: NA at 1h
  # For B: NA at 12h
  expect_equal(nrow(result_grp_exp_impl_F), 2)
  expect_equal(result_grp_exp_impl_F$Id, c("A", "B"))
  expect_equal(result_grp_exp_impl_F$gap.id, c(1,1)) # gap.id is per group here
  
  res_A_F <- dplyr::filter(result_grp_exp_impl_F, Id == "A")
  expect_equal(res_A_F$start, lubridate::as_datetime(0) + lubridate::hours(1) - lubridate::duration(30, "minutes"))
  expect_equal(res_A_F$end, lubridate::as_datetime(0) + lubridate::hours(1) + lubridate::duration(30, "minutes"))
  expect_equal(res_A_F$duration, lubridate::duration(1, "hour"))
  
  res_B_F <- dplyr::filter(result_grp_exp_impl_F, Id == "B")
  expect_equal(res_B_F$start, lubridate::as_datetime(0) + lubridate::hours(12) - lubridate::duration(30, "minutes"))
  expect_equal(res_B_F$end, lubridate::as_datetime(0) + lubridate::hours(12) + lubridate::duration(30, "minutes"))
  expect_equal(res_B_F$duration, lubridate::duration(1, "hour"))
  
  
  # Single observation in one group (filtered out by gap_finder if no Variable.colname)
  dataset_single_obs_group <- tibble::tibble(
    Id = c("A", "A", "B", "A"),
    Datetime = lubridate::as_datetime(0) + lubridate::days(c(0, 2, 0, 3)), # A: 0,2,3 (gap at day 1); B: 0
    Value = c(1,2,10,3)
  ) %>% dplyr::group_by(Id)
  
  res_single_implicit <- extract_gaps(dataset_single_obs_group, epoch = "1 day", full.days=FALSE) # No Variable.colname
  # Group B is filtered by gapless_Datetimes.
  # Group A has gap at day 1.
  expect_equal(nrow(res_single_implicit), 1)
  expect_equal(res_single_implicit$Id, "A")
  expect_equal(res_single_implicit$start, lubridate::as_datetime(0) + lubridate::days(1) - lubridate::duration(12, "hours"))
  
  res_single_explicit <- extract_gaps(dataset_single_obs_group, Value, epoch = "1 day", include.implicit.gaps = TRUE, full.days=FALSE)
  # gap_handler for A: DTs 0,1,2,3; Vals 1,NA,2,3. Gap for A at day 1.
  # gap_handler for B: DT 0; Val 10. No gaps for B.
  expect_equal(nrow(res_single_explicit), 1)
  expect_equal(res_single_explicit$Id, "A")
  expect_equal(res_single_explicit$start, lubridate::as_datetime(0) + lubridate::days(1) - lubridate::duration(12, "hours"))
  expect_equal(res_single_explicit$duration, lubridate::duration(1, "day"))
  
})

test_that("extract_gaps error handling", {
  expect_error(extract_gaps(list()), "dataset is not a dataframe")
  data <- tibble::tibble(D = lubridate::as_datetime(0), V = 1)
  expect_error(extract_gaps(data, Datetime.colname = NotDatetime), "Datetime.colname must be part of the dataset")
  data_wrong_type <- tibble::tibble(Datetime = 1:3, V=1)
  expect_error(extract_gaps(data_wrong_type), "Datetime.colname must be a Datetime")
  data_ok <- tibble::tibble(Datetime = lubridate::as_datetime(0), V=1)
  expect_error(extract_gaps(data_ok, epoch = 123), "epoch must either be a duration or a string")
  expect_error(extract_gaps(data_ok, Variable.colname = NotValue), "Variable.colname must be part of the dataset")
})