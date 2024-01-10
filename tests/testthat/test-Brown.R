test_that("Brown_rec works as expected", {
  expect_equal(Brown_rec("day"), 250)
  expect_equal(Brown_rec("evening"), 10)
  expect_equal(Brown_rec("night", Brown.night.th = 2), 2)
  expect_equal(Brown_rec("nighttime", Brown.night = "nighttime"), 1)
  expect_equal(Brown_rec("night"), 1)
  expect_equal(Brown_rec(c("day", "evening", "night")), c(250, 10, 1))
  expect_equal(Brown_rec(c("day", "evening", "night", "nighttime")), c(250, 10, 1, NA))
}
)

test_that("Brown_rec throws errors as expected", {
  expect_error(Brown_rec("day", Brown.day.th = "250"), "Thresholds need to be numeric")
  expect_error(Brown_rec("day", Brown.day = c("day", "evening", "night")), "States need to be scalars")
}
)

test_that("Brown_check works as expected", {
  expect_equal(Brown_check(100, "day"), FALSE)
  expect_equal(Brown_check(100, "day", Brown.day.th = 100), TRUE)
  expect_equal(Brown_check(100, "evening"), FALSE)
  expect_equal(Brown_check(100, "evening", Brown.evening.th = 100), TRUE)
  expect_equal(Brown_check(100, "night"), FALSE)
  expect_equal(Brown_check(100, "night", Brown.night.th = 100), TRUE)
  expect_equal(Brown_check(100, "nighttime", Brown.night = "nighttime"), FALSE)
  expect_equal(Brown_check(100, "nighttime"), NA)
  expect_equal(Brown_check(100, "nighttime", Brown.night = "nighttime", Brown.night.th = 100), TRUE)
}
)

test_that("Brown_check throws errors as expected", {
  expect_error(Brown_check(100, "day", Brown.day.th = "250"), "Thresholds need to be numeric")
  expect_error(Brown_check(100, "day", Brown.day = c("day", "evening", "night")), "States need to be scalars")
  expect_error(Brown_check(100, c("day", "evening", "night")), "state needs to be a character vector with the same length as value")
}
)

test_that("Brown2reference works as expected", {
  testdata <- tibble::tibble(MEDI = c(100, 10, 1, 300),
                    State.Brown = c("day", "evening", "night", "day"))
  expect_equal(Brown2reference(testdata) %>% 
                 dplyr::pull(Reference), c(250, 10, 1, 250))
  expect_equal(Brown2reference(testdata) %>% 
                 dplyr::pull(Reference.check), c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(Brown2reference(testdata, Brown.day.th = 100) %>% 
                 dplyr::pull(Reference), c(100, 10, 1, 100))
  expect_equal(Brown2reference(testdata, Brown.day.th = 100) %>% 
                 dplyr::pull(Reference.check), c(TRUE, TRUE, TRUE, TRUE))
}
)

test_that("Brown2reference throws errors as expected", {
  testdata <- tibble::tibble(MEDI = c(100, 10, 1, 300),
                             State.Brown = c("day", "evening", "night", "day"))
  expect_error(Brown2reference(
    testdata, Brown.day.th = "250"), 
    "Thresholds need to be numeric")
  expect_error(Brown2reference(
    testdata, 
    Brown.day = c("day", "evening", "night")), 
    "States need to be scalars")
}
)
