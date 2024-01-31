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
