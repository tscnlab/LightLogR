test_that("calculation works", {
  MEDI = c(10,20,30,0,0,50,60)
  Time = lubridate::dminutes(1:7)
  expect_equal(
    pulses_above_threshold(MEDI,Time,"above",10,"1 mins", "2 mins", 0, as.df = TRUE),
    tibble::tibble(
      "n_pulses_above_10" = 2,
      "mean_level_pulses_above_10" = 37.5,
      "mean_duration_pulses_above_10" = lubridate::dminutes(2.5),
      "total_duration_pulses_above_10" = lubridate::dminutes(5),
      "mean_onset_pulses_above_10" = lubridate::dminutes(3.5),
      "mean_midpoint_pulses_above_10" = lubridate::dminutes(4.25),
      "mean_offset_pulses_above_10" = lubridate::dminutes(5),
    )
  )
  expect_equal(
    pulses_above_threshold(MEDI,Time,"above",10,"3 mins", "1 mins", 0, as.df = TRUE),
    tibble::tibble(
      "n_pulses_above_10" = 1,
      "mean_level_pulses_above_10" = 20,
      "mean_duration_pulses_above_10" = lubridate::dminutes(3),
      "total_duration_pulses_above_10" = lubridate::dminutes(3),
      "mean_onset_pulses_above_10" = lubridate::dminutes(1),
      "mean_midpoint_pulses_above_10" = lubridate::dminutes(2),
      "mean_offset_pulses_above_10" = lubridate::dminutes(3),
    )
  )
  expect_equal(
    pulses_above_threshold(MEDI,Time,"above",10,"2 mins", "2 mins", 0.4, as.df = TRUE),
    tibble::tibble(
      "n_pulses_above_10" = 1,
      "mean_level_pulses_above_10" = mean(MEDI),
      "mean_duration_pulses_above_10" = lubridate::dminutes(7),
      "total_duration_pulses_above_10" = lubridate::dminutes(7),
      "mean_onset_pulses_above_10" = lubridate::dminutes(1),
      "mean_midpoint_pulses_above_10" = lubridate::dminutes(4),
      "mean_offset_pulses_above_10" = lubridate::dminutes(7),
    )
  )
  expect_equal(
    pulses_above_threshold(MEDI,Time,"below",20,"1 mins", "1 mins", 0, as.df = TRUE),
    tibble::tibble(
      "n_pulses_below_20" = 2,
      "mean_level_pulses_below_20" = 7.5,
      "mean_duration_pulses_below_20" = lubridate::dminutes(2),
      "total_duration_pulses_below_20" = lubridate::dminutes(4),
      "mean_onset_pulses_below_20" = lubridate::dminutes(2.5),
      "mean_midpoint_pulses_below_20" = lubridate::dminutes(3),
      "mean_offset_pulses_below_20" = lubridate::dminutes(3.5),
    )
  )
  expect_equal(
    pulses_above_threshold(MEDI,Time,"above",c(10,30),"1 mins", "1 mins", 0, as.df = TRUE),
    tibble::tibble(
      "n_pulses_within_10-30" = 1,
      "mean_level_pulses_within_10-30" = 15,
      "mean_duration_pulses_within_10-30" = lubridate::dminutes(2),
      "total_duration_pulses_within_10-30" = lubridate::dminutes(2),
      "mean_onset_pulses_within_10-30" = lubridate::dminutes(1),
      "mean_midpoint_pulses_within_10-30" = lubridate::dminutes(1.5),
      "mean_offset_pulses_within_10-30" = lubridate::dminutes(2),
    )
  )
})

test_that("Ouput works for different time variables", {
  MEDI = c(10,20,30)
  Datetime = lubridate::as_datetime(lubridate::dminutes(1:3))
  HMS = hms::as_hms(Datetime)
  Duration = lubridate::as.duration(HMS)
  Difftime = Datetime-Datetime[1]
  x = pulses_above_threshold(MEDI,Datetime,"above",10,"1 mins","1 mins",0,as.df = TRUE)
  
  
  expect_equal(
    pulses_above_threshold(MEDI,Datetime,"above",10,"1 mins","1 mins",0,as.df = TRUE),
    tibble::tibble(
      "n_pulses_above_10" = 1,
      "mean_level_pulses_above_10" = 20,
      "mean_duration_pulses_above_10" = lubridate::dminutes(3),
      "total_duration_pulses_above_10" = lubridate::dminutes(3),
      "mean_onset_pulses_above_10" = lubridate::as_datetime(1*60),
      "mean_midpoint_pulses_above_10" = lubridate::as_datetime(2*60),
      "mean_offset_pulses_above_10" = lubridate::as_datetime(3*60)
    )
  )
  
  expect_equal(
    pulses_above_threshold(MEDI,HMS,"above",10,"1 mins", "1 mins", 0, as.df = TRUE),
    tibble::tibble(
      "n_pulses_above_10" = 1,
      "mean_level_pulses_above_10" = 20,
      "mean_duration_pulses_above_10" = lubridate::dminutes(3),
      "total_duration_pulses_above_10" = lubridate::dminutes(3),
      "mean_onset_pulses_above_10" = hms::as_hms(1*60),
      "mean_midpoint_pulses_above_10" = hms::as_hms(2*60),
      "mean_offset_pulses_above_10" = hms::as_hms(3*60)
    )
  )
  
  expect_equal(
    pulses_above_threshold(MEDI,Duration,"above",10,"1 mins", "1 mins", 0, as.df = TRUE),
    tibble::tibble(
      "n_pulses_above_10" = 1,
      "mean_level_pulses_above_10" = 20,
      "mean_duration_pulses_above_10" = lubridate::dminutes(3),
      "total_duration_pulses_above_10" = lubridate::dminutes(3),
      "mean_onset_pulses_above_10" = lubridate::dminutes(1),
      "mean_midpoint_pulses_above_10" = lubridate::dminutes(2),
      "mean_offset_pulses_above_10" = lubridate::dminutes(3),
    )
  )
  
  expect_equal(
    pulses_above_threshold(MEDI,Difftime,"above",10,"1 mins", "1 mins", 0, as.df = TRUE),
    tibble::tibble(
      "n_pulses_above_10" = 1,
      "mean_level_pulses_above_10" = 20,
      "mean_duration_pulses_above_10" = lubridate::dminutes(3),
      "total_duration_pulses_above_10" = lubridate::dminutes(3),
      "mean_onset_pulses_above_10" = lubridate::as.difftime(lubridate::dminutes(0)),
      "mean_midpoint_pulses_above_10" = lubridate::as.difftime(lubridate::dminutes(1)),
      "mean_offset_pulses_above_10" = lubridate::as.difftime(lubridate::dminutes(2))
    )
  )
})

test_that("calculation works with missing values", {
  MEDI = c(10,20,30,NA,NA,50,60)
  Time = lubridate::dminutes(1:7)
  expect_equal(
    pulses_above_threshold(MEDI,Time,"above",10,"1 mins", "1 mins", 0, as.df = TRUE),
    tibble::tibble(
      "n_pulses_above_10" = 2,
      "mean_level_pulses_above_10" = 37.5,
      "mean_duration_pulses_above_10" = lubridate::dminutes(2.5),
      "total_duration_pulses_above_10" = lubridate::dminutes(5),
      "mean_onset_pulses_above_10" = lubridate::dminutes(3.5),
      "mean_midpoint_pulses_above_10" = lubridate::dminutes(4.25),
      "mean_offset_pulses_above_10" = lubridate::dminutes(5),
    )
  )
  expect_equal(
    pulses_above_threshold(MEDI,Time,"below",30,"1 mins", "1 mins", 0, as.df = TRUE),
    tibble::tibble(
      "n_pulses_below_30" = 1,
      "mean_level_pulses_below_30" = 20,
      "mean_duration_pulses_below_30" = lubridate::dminutes(3),
      "total_duration_pulses_below_30" = lubridate::dminutes(3),
      "mean_onset_pulses_below_30" = lubridate::dminutes(1),
      "mean_midpoint_pulses_below_30" = lubridate::dminutes(2),
      "mean_offset_pulses_below_30" = lubridate::dminutes(3),
    )
  )
  expect_equal(
    pulses_above_threshold(MEDI,Time,"below",60,"1 mins", "2 mins", 1, as.df = TRUE),
    tibble::tibble(
      "n_pulses_below_60" = 1,
      "mean_level_pulses_below_60" = as.double(NA),
      "mean_duration_pulses_below_60" = lubridate::dminutes(7),
      "total_duration_pulses_below_60" = lubridate::dminutes(7),
      "mean_onset_pulses_below_60" = lubridate::dminutes(1),
      "mean_midpoint_pulses_below_60" = lubridate::dminutes(4),
      "mean_offset_pulses_below_60" = lubridate::dminutes(7),
    )
  )
  expect_equal(
    pulses_above_threshold(MEDI,Time,"below",60,"1 mins", "2 mins", 1, na.rm=TRUE, as.df = TRUE),
    tibble::tibble(
      "n_pulses_below_60" = 1,
      "mean_level_pulses_below_60" = 34,
      "mean_duration_pulses_below_60" = lubridate::dminutes(7),
      "total_duration_pulses_below_60" = lubridate::dminutes(7),
      "mean_onset_pulses_below_60" = lubridate::dminutes(1),
      "mean_midpoint_pulses_below_60" = lubridate::dminutes(4),
      "mean_offset_pulses_below_60" = lubridate::dminutes(7),
    )
  )
})
