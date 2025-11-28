test_that("sample_groups validates inputs", {
  expect_error(sample_groups(mtcars), "grouped")
  grouped <- dplyr::group_by(mtcars, cyl)
  expect_error(sample_groups(grouped, n = 0), "positive integer")
  expect_error(sample_groups(grouped, n = c(1, 2)), "single positive integer")
  expect_error(sample_groups(grouped, sample = "condition"), "one of")
  expect_error(sample_groups(grouped, sample = c(-1, 2)), "positive integers")
})

test_that("sample_groups handles ordering and conditions", {
  toy_data <- tibble::tibble(
    Id = c("A", "A", "B", "B", "C", "C"),
    Datetime = as.POSIXct("2023-01-01", tz = "UTC") + 1:6,
    value = c(1, 2, 6, 5, 3, 4)
  ) |>
    dplyr::group_by(Id)

  top_group <- toy_data |>
    sample_groups(n = 1, sample = "top", order.by = mean(value)) |>
    dplyr::distinct(Id) |>
    dplyr::pull(Id)

  bottom_group <- toy_data |>
    sample_groups(n = 1, sample = "bottom", order.by = mean(value)) |>
    dplyr::distinct(Id) |>
    dplyr::pull(Id)

  conditioned <- toy_data |>
    sample_groups(condition = mean(value) > 2) |>
    dplyr::distinct(Id) |>
    dplyr::arrange(Id) |>
    dplyr::pull(Id)

  positioned <- toy_data |>
    sample_groups(sample = 2:3, order.by = mean(value)) |>
    dplyr::distinct(Id) |>
    dplyr::arrange(Id) |>
    dplyr::pull(Id)

  numeric_overrides_n <- toy_data |>
    sample_groups(sample = 2, n = 10, order.by = mean(value)) |>
    dplyr::distinct(Id) |>
    dplyr::pull(Id)

  ignored_sample <- toy_data |>
    sample_groups(sample = "bottom", condition = mean(value) > 2) |>
    dplyr::distinct(Id) |>
    dplyr::arrange(Id) |>
    dplyr::pull(Id)

  default_order_top <- toy_data |>
    sample_groups(n = 1, sample = "top") |>
    dplyr::distinct(Id) |>
    dplyr::pull(Id)

  expect_equal(top_group, "B")
  expect_equal(bottom_group, "A")
  expect_equal(conditioned, c("B", "C"))
  expect_equal(positioned, c("B", "C"))
  expect_equal(ignored_sample, c("B", "C"))
  expect_equal(default_order_top, "C")
  expect_equal(numeric_overrides_n, "C")
})

test_that("sample_groups can sample a random group", {
  toy_data <- tibble::tibble(
    Id = rep(LETTERS[1:4], each = 2),
    Datetime = as.POSIXct("2023-01-01", tz = "UTC") + 1:8,
    value = 1:8
  ) |>
    dplyr::group_by(Id)

  set.seed(123)
  random_sample <- toy_data |>
    sample_groups(n = 2, sample = "random") |>
    dplyr::distinct(Id) |>
    dplyr::arrange(Id) |>
    dplyr::pull(Id)

  expect_equal(length(random_sample), 2L)
  expect_setequal(random_sample, unique(random_sample))
})
