test_that("Datetime_breaks works", {
  dataset <- c("2023-08-15", "2023-08-20")
  expect_equal(Datetime_breaks(dataset),
               c(1692100800, 1692187200, 1692273600, 1692360000, 1692446400) %>% 
               lubridate::as_datetime())
  expect_equal(Datetime_breaks(dataset, shift = 0), 
               c(1692057600, 
                 1692144000, 
                 1692230400, 
                 1692316800, 
                 1692403200, 
                 1692489600) %>% lubridate::as_datetime())
  expect_equal(Datetime_breaks(dataset, by = "12 hours"), 
               c(1692100800, 1692144000, 1692187200, 1692230400, 1692273600, 
                 1692316800, 1692360000, 1692403200, 1692446400, 1692489600) %>% 
                 lubridate::as_datetime())
})

test_that("Datetime_limits works", {
  dataset <- c("2023-08-15", "2023-08-20")
  breaks <- Datetime_breaks(dataset)
  expect_equal(Datetime_limits(breaks), 
               c("2023-08-15", "2023-08-20") %>% lubridate::as_datetime())
  expect_equal(Datetime_limits(breaks, start = lubridate::ddays(1)), 
               c("2023-08-16", "2023-08-20") %>% lubridate::as_datetime())
  expect_equal(Datetime_limits(breaks, length = lubridate::ddays(2)), 
               c("2023-08-15", "2023-08-18") %>% lubridate::as_datetime())
})
