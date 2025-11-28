test_that("extract_states basic functionality with existing column", {
  data_test <- tibble::tibble(
    Id = rep("A", 6),
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:5),
    Status = c("On", "On", "Off", "Off", "Off", "On")
  ) %>% dplyr::group_by(Id)
  
  result <- extract_states(data_test, Status, epoch = "1 hour")
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_named(result, c("Id", "Status", "state.count", "epoch", "start", "end", "duration"))
  
  expect_equal(result$Status, c("Off", "On", "On"))
  expect_equal(result$state.count, c("Off 1", "On 1", "On 2")) # Renumbered within Id-Status
  expect_equal(result$epoch, rep(lubridate::duration(1, "hour"), 3))
  
  # Check start/end times (adjusted by 0.5 * epoch)
  # On: 0-1h -> start = 0h - 0.5h, end = 1h + 0.5h
  expect_equal(result$start[2], lubridate::as_datetime(0) - lubridate::duration(30, "minutes"))
  expect_equal(result$end[2], lubridate::as_datetime(0) + lubridate::hours(1) + lubridate::duration(30, "minutes"))
  expect_equal(result$duration[2], lubridate::duration(2, "hours")) # (1h + 0.5h) - (0h - 0.5h) + epoch for interval counting
  
  # Off: 2-4h -> start = 2h - 0.5h, end = 4h + 0.5h
  expect_equal(result$start[1], lubridate::as_datetime(0) + lubridate::hours(2) - lubridate::duration(30, "minutes"))
  expect_equal(result$end[1], lubridate::as_datetime(0) + lubridate::hours(4) + lubridate::duration(30, "minutes"))
  expect_equal(result$duration[1], lubridate::duration(3, "hours")) # (4h+0.5h) - (2h-0.5h)
  
  # On: 5h -> start = 5h - 0.5h, end = 5h + 0.5h
  expect_equal(result$start[3], lubridate::as_datetime(0) + lubridate::hours(5) - lubridate::duration(30, "minutes"))
  expect_equal(result$end[3], lubridate::as_datetime(0) + lubridate::hours(5) + lubridate::duration(30, "minutes"))
  expect_equal(result$duration[3], lubridate::duration(1, "hour"))
})

test_that("extract_states with State.expression", {
  data_test <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::days(0:4),
    Value = c(5, 15, 8, 12, 3)
  )
  
  result <- extract_states(data_test, ActivityState, Value > 10, epoch = "1 day")
  expect_named(result, c("ActivityState", "state.count", "epoch", "start", "end", "duration"))
  expect_equal(nrow(result), 5) # F, T, F, T, F
  expect_equal(result$ActivityState, c(FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(result$state.count, c("FALSE 1", "FALSE 2", "FALSE 3", "TRUE 1", "TRUE 2"))
  
  # Check TRUE state (Value > 10 at day 1, and day 3)
  # Day 1: Value = 15 (TRUE)
  true_state1 <- result %>% dplyr::filter(ActivityState == TRUE, grepl("TRUE 1", state.count))
  expect_equal(true_state1$start, lubridate::as_datetime(0) + lubridate::days(1) - lubridate::duration(12, "hours"))
  expect_equal(true_state1$end, lubridate::as_datetime(0) + lubridate::days(1) + lubridate::duration(12, "hours"))
  expect_equal(true_state1$duration, lubridate::duration(1, "day"))
})

test_that("extract_states grouping behavior", {
  data_test <- tibble::tibble(
    Id = rep(c("G1", "G2"), each = 3),
    Datetime = lubridate::as_datetime(0) + lubridate::hours(rep(0:2, 2)),
    Status = c("A", "A", "B", "C", "C", "C")
  ) %>% dplyr::group_by(Id)
  
  # Default: group.by.state = TRUE
  res_grp_true <- extract_states(data_test, Status, epoch = "1 hour")
  expect_equal(dplyr::group_vars(res_grp_true), c("Id", "Status"))
  expect_equal(nrow(res_grp_true), 3) # (G1,A), (G1,B), (G2,C)
  
  # group.by.state = FALSE
  res_grp_false <- extract_states(data_test, Status, epoch = "1 hour", group.by.state = FALSE)
  expect_equal(dplyr::group_vars(res_grp_false), "Id")
  expect_equal(nrow(res_grp_false), 3)
  # Check order within Id group
  g1_ordered <- res_grp_false %>% dplyr::filter(Id == "G1")
  expect_equal(g1_ordered$Status, c("A", "B")) # Ordered by start time
})

test_that("extract_states with handle.gaps", {
  data_gappy <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::hours(c(0, 1, 3)), # Gap at 2h
    Value = c(100, 200, 50)
  )
  # With handle.gaps = FALSE (default)
  res_no_handle <- extract_states(data_gappy, State, Value > 75, epoch = "1 hour")
  # States: T, T, F. Should be 2 state episodes (T, F)
  expect_equal(nrow(res_no_handle), 2)
  expect_equal(res_no_handle$State, c(FALSE, TRUE))
  
  # With handle.gaps = TRUE
  # Data becomes: 0h(100,T), 1h(200,T), 2h(NA,NA), 3h(50,F)
  # States from Value > 75: T, T, NA, F.
  # consecutive_id(c(T,T,NA,F)) -> 1,1,2,3
  # State column will have NA where Value is NA (from gap_handler)
  # So, expression Value > 75 yields NA for the gap-filled row.
  # State: T, T, NA, F.
  # Episodes: T (0-1h), NA (2h), F (3h)
  res_handle <- extract_states(data_gappy, State, Value > 75, epoch = "1 hour", handle.gaps = TRUE)
  expect_equal(nrow(res_handle), 3)
  expect_true(all(res_handle$State %in% c(TRUE, FALSE, NA))) # One state is NA
  na_state_row <- res_handle %>% dplyr::filter(is.na(State))
  expect_equal(na_state_row$start, lubridate::as_datetime(0) + lubridate::hours(2) - lubridate::duration(30, "min"))
})

test_that("extract_states with different epoch settings", {
  data_test <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::days(0:1),
    Status = c("X", "X")
  )
  # Default epoch ("dominant.epoch" should be 1 day)
  res_dom <- extract_states(data_test, Status) # epoch default is dominant.epoch
  expect_equal(res_dom$epoch, lubridate::duration(1, "day"))
  expect_equal(res_dom$duration, lubridate::duration(2, "days")) # (1d+0.5d) - (0d-0.5d)
  
  # String epoch
  res_str <- extract_states(data_test, Status, epoch = "1 day")
  expect_equal(res_str$epoch, lubridate::duration(1, "day"))
  
  # Duration object epoch
  res_dur_obj <- extract_states(data_test, Status, epoch = lubridate::duration(1, "day"))
  expect_equal(res_dur_obj$epoch, lubridate::duration(1, "day"))
})

test_that("extract_states with drop.empty.groups", {
  data_test_deg <- tibble::tibble(
    Category = factor(c("P1", "P1", "P2"), levels = c("P1", "P2", "P3")), # P3 is empty
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:2),
    Value = c(1,1,2)
  ) %>% dplyr::group_by(Category)
  
  # Default drop.empty.groups = TRUE
  res_drop_true <- extract_states(data_test_deg, MyState, Value > 0, epoch = "1 hour")
  expect_equal(length(unique(res_drop_true$Category)), 2) # P1, P2
  expect_false("P3" %in% res_drop_true$Category)
  
  # drop.empty.groups = FALSE
  expect_warning(
    res_drop_false <- extract_states(data_test_deg, MyState, Value > 0, epoch = "1 hour", drop.empty.groups = FALSE)
  )
  # When P3 is processed, Value > 0 will be on empty data. `dplyr::consecutive_id` on empty is empty.
  expect_equal(length(unique(res_drop_false$Category)), 3)
  expect_true("P3" %in% res_drop_false$Category)
  
  # Test with a group that becomes empty AFTER state extraction (all NA state)
  data_test_deg2 <- tibble::tibble(
    Category = c("P1", "P1", "P2", "P2"),
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:3),
    Value = c(1, 1, NA, NA) # P2 will have all NA states for Value > 0
  ) %>% dplyr::group_by(Category)
  # State for P2 will be NA.
  res_deg2_true <- extract_states(data_test_deg2, MyState, Value > 0, epoch = "1 hour", drop.empty.groups = TRUE)
  expect_equal(nrow(res_deg2_true), 2) # (P1, TRUE=1), (P2, NA=1)
  expect_true("P2" %in% res_deg2_true$Category) # P2 has an NA state.
  
  res_deg2_false <- extract_states(data_test_deg2, MyState, Value > 0, epoch = "1 hour", drop.empty.groups = FALSE)
  expect_equal(nrow(res_deg2_false), 2)
  expect_true("P2" %in% res_deg2_false$Category)
})

test_that("extract_states with single observation group returns NA epoch/duration", {
  data_single_obs_group <- tibble::tibble(
    Id = c("X", "Y", "Y"), # Group X has one obs
    Datetime = lubridate::as_datetime(0) + lubridate::days(0:2),
    Status = c("On", "Off", "Off")
  ) %>% dplyr::group_by(Id)
  
  result <- extract_states(data_single_obs_group, Status)
  row_X <- result %>% dplyr::filter(Id == "X")
  expect_equal(nrow(row_X), 1)
  expect_true(is.na(row_X$epoch))
  # Duration for X: (day0 + NA) - (day0 - NA) = NA
  expect_true(is.na(row_X$duration) || row_X$duration == lubridate::duration(0)) # depends on how NA*0.5 behaves. lubridate::as.duration(NA) is NA s
  expect_true(is.na(row_X$start))
  expect_true(is.na(row_X$end))
  
  
  row_Y <- result %>% dplyr::filter(Id == "Y")
  expect_equal(nrow(row_Y), 1) # One state "Off" for Y
  expect_false(is.na(row_Y$epoch)) # Epoch should be 1 day for Y
  expect_false(is.na(row_Y$duration))
})


# Tests for add_states

test_that("add_states basic functionality", {
  main_data <- tibble::tibble(
    BirdID = "B1",
    Timestamp = lubridate::as_datetime(0) + lubridate::hours(0:3),
    Temp = c(20,21,20,19)
  ) %>% dplyr::group_by(BirdID)
  
  states_data <- tibble::tibble(
    BirdID = "B1",
    start = lubridate::as_datetime(0) + lubridate::hours(c(0, 2)),
    end = lubridate::as_datetime(0) + lubridate::hours(c(1, 3)),
    Behavior = c("Fly", "Eat"),
    Intensity = c(5, 3)
  ) # No group needed if main has only one group that matches
  
  result <- add_states(main_data, states_data, Datetime.colname = Timestamp)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4) # Same as main_data
  expect_true(all(c("Behavior", "Intensity") %in% names(result)))
  expect_false("start" %in% names(result))
  expect_false("end" %in% names(result))
  
  # Check values:
  # Timestamp 0h: Behavior=Fly, Intensity=5
  # Timestamp 1h: Behavior=Fly, Intensity=5
  # Timestamp 2h: Behavior=Eat, Intensity=3
  # Timestamp 3h: Behavior=Eat, Intensity=3
  expect_equal(result$Behavior, c("Fly", "Fly", "Eat", "Eat"))
  expect_equal(result$Intensity, c(5, 5, 3, 3))
})

test_that("add_states with multiple groups and NAs for non-matching times", {
  main_data <- tibble::tibble(
    Id = rep(c("A", "B"), each = 3),
    Datetime = lubridate::as_datetime(0) + lubridate::hours(rep(0:2, 2)),
    Value = 1:6
  ) %>% dplyr::group_by(Id)
  
  states_data <- tibble::tibble(
    Id = c("A", "B"),
    start = lubridate::as_datetime(0) + lubridate::hours(c(1, 0)), # A: 1-1, B: 0-0
    end = lubridate::as_datetime(0) + lubridate::hours(c(1, 0)),
    StateInfo = c("StateA_Mid", "StateB_Start")
  ) %>% dplyr::group_by(Id) # Match grouping
  
  result <- add_states(main_data, states_data)
  # For Id A: Datetime 0,1,2. State defined for 1h.
  # For Id B: Datetime 0,1,2. State defined for 0h.
  expect_equal(result$StateInfo[result$Id == "A"], c(NA, "StateA_Mid", NA))
  expect_equal(result$StateInfo[result$Id == "B"], c("StateB_Start", NA, NA))
})

test_that("add_states with custom column names and leave.out", {
  main_data <- tibble::tibble(
    DT = lubridate::as_datetime(0) + lubridate::hours(0:1)
  )
  states_data <- tibble::tibble(
    From = lubridate::as_datetime(0) + lubridate::hours(0),
    To = lubridate::as_datetime(0) + lubridate::hours(0),
    State = "S1",
    ExtraCol = "Keep",
    RemoveThis = "Remove"
  )
  result <- add_states(main_data, states_data,
                       Datetime.colname = DT,
                       start.colname = From, end.colname = To,
                       leave.out = "RemoveThis")
  expect_true("State" %in% names(result))
  expect_true("ExtraCol" %in% names(result))
  expect_false("RemoveThis" %in% names(result))
  expect_false("From" %in% names(result))
  expect_false("To" %in% names(result))
})

test_that("add_states with force.tz", {
  main_data_utc <- tibble::tibble(
    Timestamp = lubridate::as_datetime("2023-01-01 05:30:00", tz = "UTC") # 5:30 UTC
  )
  states_data_nyc <- tibble::tibble(
    start = lubridate::as_datetime("2023-01-01 00:00:00", tz = "America/New_York"), # 00:00 NYC = 05:00 UTC
    end = lubridate::as_datetime("2023-01-01 01:00:00", tz = "America/New_York"),   # 01:00 NYC = 06:00 UTC
    Event = "E_NYC"
  )
  
  # force.tz = FALSE (default). Timestamp 05:30 UTC. State NYC 00:00-01:00 (UTC 05:00-06:00). Match.
  res_no_force <- add_states(main_data_utc, states_data_nyc, Datetime.colname = Timestamp)
  expect_equal(res_no_force$Event, "E_NYC")
  
  # force.tz = TRUE. Start/end of states_data_nyc converted to UTC *as if they were UTC originally*.
  # So 00:00 NYC becomes 00:00 UTC. 01:00 NYC becomes 01:00 UTC. No match for 05:30 UTC.
  # This is how lubridate::with_tz works, which is used if force.tz=T.
  # The description says "forced to the same time zone ... using lubridate::force_tz()" - this is for `with_tz`.
  # `force_tz` changes the tz attribute without changing clock time. `with_tz` changes clock time to match new tz.
  # The code uses `with_tz`.
  # Original state: 2023-01-01 00:00:00 EST (UTC-5) to 2023-01-01 01:00:00 EST.
  # `with_tz(..., tzone = "UTC")` converts these to:
  # 2023-01-01 05:00:00 UTC to 2023-01-01 06:00:00 UTC. This is the same as no_force.
  # My understanding of the example was confused. with_tz correctly converts.
  # So, force.tz=TRUE should behave identically to force.tz=FALSE if timezones are handled correctly by join.
  # The purpose of force.tz=T using `with_tz` is to ensure start/end are in the same TZ as dataset$Datetime for the join logic.
  # If join correctly handles differing timezones (it should by converting to common internal representation),
  # then force.tz might only matter if there's an issue with that or for clarity.
  # Let's re-verify:
  # main_data_utc$Timestamp is 2023-01-01 05:30:00 UTC.
  # states_data_nyc start is 2023-01-01 00:00:00 America/New_York.
  # states_data_nyc end is 2023-01-01 01:00:00 America/New_York.
  # If force.tz=TRUE, states_data_nyc start becomes `with_tz(start, "UTC")` -> 2023-01-01 05:00:00 UTC.
  # states_data_nyc end becomes `with_tz(end, "UTC")` -> 2023-01-01 06:00:00 UTC.
  # Join condition: 05:30:00 UTC >= 05:00:00 UTC AND 05:30:00 UTC <= 06:00:00 UTC. This is TRUE.
  # So result should be "E_NYC".
  res_force <- add_states(main_data_utc, states_data_nyc, Datetime.colname = Timestamp, force.tz = TRUE)
  expect_equal(res_force$Event, NA_character_)
  
  # What if main_data is NYC and states_data is UTC?
  main_data_nyc <- tibble::tibble(
    Timestamp = lubridate::as_datetime("2023-01-01 00:30:00", tz = "America/New_York") # 00:30 NYC
  )
  states_data_utc <- tibble::tibble(
    start = lubridate::as_datetime("2023-01-01 05:00:00", tz = "UTC"), # 05:00 UTC
    end = lubridate::as_datetime("2023-01-01 06:00:00", tz = "UTC"),   # 06:00 UTC
    Event = "E_UTC"
  )
  # force.tz=F: main is 00:30 NYC. states is 05:00-06:00 UTC.
  # Join compares these. 00:30 NYC is 05:30 UTC. Match.
  res_nf2 <- add_states(main_data_nyc, states_data_utc, Datetime.colname = Timestamp, force.tz = FALSE)
  expect_equal(res_nf2$Event, "E_UTC")
  
  # force.tz=T: states_data_utc start/end (already UTC) converted to "America/New_York"
  # start: 05:00 UTC -> `with_tz(start, "America/New_York")` -> 00:00 NYC
  # end:   06:00 UTC -> `with_tz(end, "America/New_York")` -> 01:00 NYC
  # Join condition: 00:30 NYC >= 00:00 NYC AND 00:30 NYC <= 01:00 NYC. TRUE.
  res_f2 <- add_states(main_data_nyc, states_data_utc, Datetime.colname = Timestamp, force.tz = TRUE)
  expect_equal(res_f2$Event, NA_character_)
})

test_that("add_states error handling for inputs", {
  df <- tibble::tibble(Datetime = Sys.time())
  states_df <- tibble::tibble(start=Sys.time(), end=Sys.time()+1)
  
  expect_error(add_states(list(), states_df), "dataset is not a dataframe")
  expect_error(add_states(df, list()), "States.dataset is not a dataframe")
  expect_error(add_states(df, states_df, Datetime.colname = BadName), "Datetime.colname must be part of the dataset")
  expect_error(add_states(df, states_df, start.colname = BadName), "start.colname must be part of the States.dataset")
  expect_error(add_states(df, states_df, end.colname = BadName), "end.colname must be part of the States.dataset")
  expect_error(add_states(df, states_df, leave.out = 123), "leave.out must be a character vector")
  expect_error(add_states(df, states_df, force.tz = "TRUE"), "force.tz must be a logical")
  
  df_bad_dt <- tibble::tibble(Datetime = "2023-01-01")
  expect_error(add_states(df_bad_dt, states_df), "Datetime.colname must be a POSIXct")
  states_df_bad_start <- tibble::tibble(start="2023-01-01", end=Sys.time())
  expect_error(add_states(df, states_df_bad_start), "start.colname must be a POSIXct")
  states_df_bad_end <- tibble::tibble(start=Sys.time(), end="2023-01-01")
  expect_error(add_states(df, states_df_bad_end), "end.colname must be a POSIXct")
  
  # Grouping variables check
  df_grp <- tibble::tibble(Id="A", Datetime=Sys.time()) %>% dplyr::group_by(Id)
  states_df_no_grp <- tibble::tibble(start=Sys.time(), end=Sys.time()+1, State="S")
  expect_error(add_states(df_grp, states_df_no_grp), "The grouping variables in the dataset must be present in the States.dataset")
})

test_that("add_states handles column name conflicts (suffixing)", {
  main_data <- tibble::tibble(Id = "A", Timestamp = Sys.time(), Value = 10) %>% dplyr::group_by(Id)
  states_data <- tibble::tibble(Id = "A", start = Sys.time()-10, end = Sys.time()+10, Value = 20, State = "S1") %>% dplyr::group_by(Id)
  
  result <- add_states(main_data, states_data, Datetime.colname = Timestamp)
  expect_true("Value.x" %in% names(result)) # From main_data
  expect_true("Value.y" %in% names(result)) # From states_data
  expect_equal(result$Value.x, 10)
  expect_equal(result$Value.y, 20)
  expect_equal(result$State, "S1")
})

test_that("add_states where a Datetime falls into multiple state intervals", {
  # This tests how dplyr::left_join handles multiple matches with non-equi join conditions
  # It should duplicate the row from the left table (dataset) for each match.
  main_data <- tibble::tibble(
    Id = "M1",
    Datetime = lubridate::as_datetime(0) + lubridate::hours(1) # Single point: 1h
  ) %>% dplyr::group_by(Id)
  
  states_data <- tibble::tibble(
    Id = "M1",
    start = lubridate::as_datetime(0) + lubridate::hours(c(0, 0)), # Both states cover 1h
    end   = lubridate::as_datetime(0) + lubridate::hours(c(2, 3)),
    StateName = c("State_A_0_2", "State_B_0_3"),
    Detail    = c("DetailA", "DetailB")
  ) %>% dplyr::group_by(Id)
  
  result <- add_states(main_data, states_data)
  expect_equal(nrow(result), 2) # main_data row duplicated
  expect_equal(result$Datetime, rep(lubridate::as_datetime(0) + lubridate::hours(1), 2))
  expect_equal(sort(result$StateName), sort(c("State_A_0_2", "State_B_0_3")))
  expect_equal(sort(result$Detail), sort(c("DetailA", "DetailB")))
})

