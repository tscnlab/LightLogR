test_that("sc2interval works as expected", {
  sample <- tibble::tibble(Datetime = c("2023-08-15 6:00:00",
                                         "2023-08-15 23:00:00"),
                            State = c("wake", "sleep"),
                            Id = "Participant")
  expect_snapshot(sc2interval(sample))
  expect_setequal(names(sc2interval(sample)), c("Id", "State", "Interval"))
  expect_equal(sample %>% sc2interval() %>% nrow(), 3)
  expect_equal(sample %>% sc2interval(full = FALSE) %>% nrow(), 1)
}
)

test_that("sc2interval throws errors as expected", {
  sample <- tibble::tibble(Datetime = c("2023-08-15 6:00:00",
                                         "2023-08-15 23:00:00"),
                            State = c("wake", "sleep"),
                            Id = "Participant")
  expect_error(sc2interval(sample, full = "FALSE"), "full must be a logical")
  expect_error(sc2interval(sample, full = 1), "full must be a logical")
  expect_error(sc2interval(sample, full = 0), "full must be a logical")
  expect_error(sc2interval(sample, full = "TRUE"), "full must be a logical")
  expect_error(sc2interval(sample, Statechange.colname = Stata), 
               "Statechange.colname must be part of the dataset")
  expect_error(sc2interval(sample, Datetime.colname = Stata),
               "Datetime.colname must be part of the dataset")
})
  