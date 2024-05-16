#dst_change_handler

test_that("DST jumps work, when a DST change is present", {
  
  #setup
  tz = "Europe/Berlin"
  
  ST_to_DST <-
    tibble::tibble(
      Datetime = seq.POSIXt(from = as.POSIXct("2023-03-26 00:30:00", tz = tz),
                            to = as.POSIXct("2023-03-26 03:00:00", tz = tz),
                            by = "30 mins"),
      Value = 1)
  
  DST_to_ST <-
    tibble::tibble(
      Datetime = seq.POSIXt(from = as.POSIXct("2023-10-29 01:30:00", tz = tz),
                            to = as.POSIXct("2023-10-29 03:00:00", tz = tz),
                            by = "30 mins"),
      Value = 1)
  
  #make a jump forward
  expect_equal(
    ST_to_DST %>% dst_change_handler() %>% .$Datetime %>% .[4],
    as.POSIXct(c("2023-03-26 04:00:00"), tz = tz)
  )
  
  #make a jump backward
  expect_equal(
    DST_to_ST %>% dst_change_handler() %>% .$Datetime %>% .[6],
    as.POSIXct(c("2023-10-29 01:00:00"), tz = "UTC") %>% lubridate::with_tz(tzone = tz)
  )
  
  #make a jump forward with grouping
  expect_equal(
    ST_to_DST %>% dplyr::group_by(Value) %>% dst_change_handler() %>% 
      .$Datetime %>% .[4],
    as.POSIXct(c("2023-03-26 04:00:00"), tz = tz)
  )
  
  #make no jump when there is no DST change
  expect_equal(
    DST_to_ST[1:3,] %>% dst_change_handler(),
    DST_to_ST[1:3,]
  )
  #again the other way round
  expect_equal(
    ST_to_DST[1:3,] %>% dst_change_handler(),
    ST_to_DST[1:3,]
  )
  #again with grouping
  expect_equal(
    ST_to_DST[1:3,] %>% dplyr::group_by(Value) %>% dst_change_handler(),
    ST_to_DST[1:3,] %>% dplyr::group_by(Value)
  )
  
})

test_that("giving wrong input types gives an error", {
  
  #setup
  tz = "Europe/Berlin"
  
  DST_to_ST <-
    tibble::tibble(
      Datetime = seq.POSIXt(from = as.POSIXct("2023-10-29 01:30:00", tz = tz),
                            to = as.POSIXct("2023-10-29 03:00:00", tz = tz),
                            by = "30 mins"),
      Value = 1)
  
  #Dataset is not a dataframe
  expect_error(
    DST_to_ST %>% dplyr::pull(Datetime) %>% dst_change_handler(),
    "dataset is not a dataframe"
  )
  expect_error(
    DST_to_ST %>% dplyr::pull(Datetime) %>% dst_change_summary(),
    "dataset is not a dataframe"
  )
  
  #Datetime is not part of the function
  expect_error(
    DST_to_ST %>% dst_change_handler(Datetime.colname = "Value"),
    "Datetime.colname must be a Datetime"
  )
  
})

#dst_change_summary

test_that("DST summary works", {
  
  #setup
  tz = "Europe/Berlin"
  
  ST_to_DST <-
    tibble::tibble(
      Datetime = seq.POSIXt(from = as.POSIXct("2023-03-26 00:30:00", tz = tz),
                            to = as.POSIXct("2023-03-26 03:00:00", tz = tz),
                            by = "30 mins"),
      Value = 1)
  
  DST_to_ST <-
    tibble::tibble(
      Datetime = seq.POSIXt(from = as.POSIXct("2023-10-29 01:30:00", tz = tz),
                            to = as.POSIXct("2023-10-29 03:00:00", tz = tz),
                            by = "30 mins"),
      Value = 1)
  
  #detect a jump forward
  expect_true(DST_to_ST %>% dst_change_summary() %>% .[[1]])
  
  #detect a jump backward
  expect_false(ST_to_DST %>% dst_change_summary() %>% .[[1]])
  
  #provide a zero-length vector when no jump was detected
  expect_equal(
    DST_to_ST[1:3,] %>% dst_change_summary() %>%  .[[1]] %>% length(),
    0
  )
  #same thing 2nd time
  expect_equal(
    ST_to_DST[1:3,] %>% dst_change_summary() %>%  .[[1]] %>% length(),
    0
  )

})

