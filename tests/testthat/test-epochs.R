test_that("dominant_epoch finder works", {
  dataset <- 
    tibble::tibble(Id = c("A", "A", "A", "B", "B", "B"),
                   Datetime = lubridate::as_datetime(1) +
                     lubridate::days(c(0:2, 4, 6, 8)))
  
  expect_equal(dataset %>%
                 dplyr::group_by(Id) %>%
                 dominant_epoch() %>% .[[2]],
               lubridate::duration(c(1,2), unit = "days"))
  expect_equal(dataset %>%
                 dominant_epoch() %>% .[[1]],
               lubridate::duration(2, unit = "days"))
  
})

test_that("dominant_epoch throws appropriate errors", {
  dataset <- 
    tibble::tibble(Id = c("A", "A", "A", "B", "B", "B"),
                   Datetime = lubridate::as_datetime(1) +
                     lubridate::days(c(0:2, 4, 6, 8)))
  expect_error(dataset$Id %>% dominant_epoch(),
               "dataset is not a dataframe")
  expect_error(dataset %>% dominant_epoch(Datetime.colname = Status),
               "Datetime.colname must be part of the dataset")
  expect_error(dataset %>% dominant_epoch(Datetime.colname = Id),
               "Datetime.colname must be a Datetime")
})