test_that("sleep_int2Brown works as expected", {
  sample <- tibble::tibble(Datetime = c("2023-08-15 6:00:00",
                                            "2023-08-15 23:00:00",
                                            "2023-08-16 6:00:00",
                                            "2023-08-16 22:00:00"),
                            State = rep(c("wake", "sleep"), 2),
                            Id = "Participant")
  sample <- sample %>% sc2interval()
  output <- c(NA, "day", "evening", "night", "day", "evening", "night")
  expect_equal(sample %>% 
                 sleep_int2Brown() %>% 
                 dplyr::pull(State.Brown), output)
  expect_equal(sample %>% 
                 sleep_int2Brown(output.dataset = FALSE), output)
  
})

test_that("sleep_int2Brown throws errors as expected", {
  sample <- tibble::tibble(Datetime = c("2023-08-15 6:00:00",
                                        "2023-08-15 23:00:00",
                                        "2023-08-16 6:00:00",
                                        "2023-08-16 22:00:00"),
                           State = rep(c("wake", "sleep"), 2),
                           Id = "Participant")
  sample <- sample %>% sc2interval()
  expect_error(sleep_int2Brown(sample, output.dataset = "TRUE"), "must be a logical")
  expect_error(sleep_int2Brown(sample, Interval.colname = Datetime), "must be part of the dataset")
  expect_error(sleep_int2Brown(sample, Brown.night = c("night", "day")), "must be scalars")
}
)
