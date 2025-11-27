test_that("format_coordinates builds readable labels", {
  expect_equal(format_coordinates(c(48.52, 9.06)), "48.5°N, 9.1°E")
  expect_equal(format_coordinates(c(-48.52, -9.06)), "48.5°S, 9.1°W")
  expect_equal(format_coordinates(c(0, 0)), "0°N, 0°E")
  expect_equal(format_coordinates(c(48.52, 9.06), digits = 2), "48.52°N, 9.06°E")
})

test_that("format_coordinates validates inputs", {
  expect_error(format_coordinates(c(48, 9, 10)))
  expect_error(format_coordinates(c(NA_real_, 9)))
  expect_error(format_coordinates("not numeric"))
  expect_error(format_coordinates(c(48, 9), digits = -1))
  expect_error(format_coordinates(c(48, 9), digits = c(1, 2)))
})
