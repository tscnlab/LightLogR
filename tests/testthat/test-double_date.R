test_that("repeat_date and next date works", {
  #some example datetimes
  dates <- tibble::tibble(
    Datetime = as.POSIXct(
      c("2023-08-20 10:00:00", 
        "2023-08-20 10:15:00", 
        "2023-08-20 10:30:00", 
        "2023-08-20 10:45:00"
        )
      )
  )
  
  #repeat_date works
  expect_equal(nrow(repeat_date(dates)), 2*nrow(dates))
  expect_equal(repeat_date(dates) %>% 
                 dplyr::pull(Datetime) %>% 
                 lubridate::date() %>% 
                 unique() %>% as.character(), 
               c("2023-08-20", "2023-08-21"))
  expect_equal(repeat_date(dates) %>% length(), 2)
  expect_equal(repeat_date(dates) %>% dplyr::group_vars(), "Date.data")
  expect_true(repeat_date(dates) %>% 
                 create_Timedata() %>% 
                 dplyr::pull(Time.data) %>% {all(.[1:4] == .[5:8])})
  
  #next_date works
  next_dates <- next_date(repeat_date(dates) %>% dplyr::ungroup())
  expect_equal(nrow(next_dates), 3*nrow(dates))
  expect_equal(next_dates %>% 
                 dplyr::pull(Datetime) %>% 
                 lubridate::date() %>% unique() %>% as.character(), 
               c("2023-08-20", "2023-08-21"))
  expect_equal(next_dates %>% 
                 dplyr::pull(Datetime) %>% 
                 lubridate::date() %>% table() %>% unname() %>% as.numeric(), 
               c(4, 8))
  expect_equal(next_dates %>% length(), 2)
  expect_equal(next_dates %>% dplyr::group_vars(), "Date.data")
})

test_that("double_date works", {
  #some example datetimes
  dates <- tibble::tibble(
    Datetime = as.POSIXct(
      c("2023-08-20 10:00:00", 
        "2023-08-20 10:15:00", 
        "2023-08-20 10:30:00", 
        "2023-08-20 10:45:00"
      )
    )
  )
  next_dates <- next_date(repeat_date(dates) %>% dplyr::ungroup())

  #double_date correctly identifies the correct type of date
  expect_equal(double_date(dates), repeat_date(dates))
  expect_equal(double_date(repeat_date(dates) %>% dplyr::ungroup()), 
               next_dates)
})