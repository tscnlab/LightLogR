test_that("sleep_int2Brown works as expected", {
  sample <- tibble::tibble(
    Datetime = c(
      "2023-08-15 6:00:00",
      "2023-08-15 23:00:00",
      "2023-08-16 6:00:00",
      "2023-08-16 22:00:00"
    ),
    State = rep(c("wake", "sleep"), 2),
    Id = "Participant"
  )
  sample <- sample %>% sc2interval()
  output <- c(NA, "day", "evening", "night", "day", "evening", "night")
  expect_silent(dataset <- sleep_int2Brown(sample))
  expect_identical(dataset$State.Brown, output)
  expect_silent(states <- sleep_int2Brown(sample, output.dataset = FALSE))
  expect_identical(states, output)
})

test_that("sleep_int2Brown throws errors as expected", {
  sample <- tibble::tibble(
    Datetime = c(
      "2023-08-15 6:00:00",
      "2023-08-15 23:00:00",
      "2023-08-16 6:00:00",
      "2023-08-16 22:00:00"
    ),
    State = rep(c("wake", "sleep"), 2),
    Id = "Participant"
  )
  sample <- sample %>% sc2interval()
  expect_error(
    sleep_int2Brown(sample, output.dataset = "TRUE"),
    "must be a logical"
  )
  expect_error(
    sleep_int2Brown(sample, Interval.colname = Datetime),
    "must be part of the dataset"
  )
  expect_error(
    sleep_int2Brown(sample, Brown.night = c("night", "day")),
    "must be scalars"
  )
})

test_that("sleep_int2Brown preserves unmatched states and custom mappings", {
  origin <- as.POSIXct("2026-01-01 00:00:00", tz = "Europe/Berlin")
  sample <- tibble::tibble(
    Span = lubridate::interval(
      origin + c(0, 4, 12, 16, 20) * 3600,
      origin + c(4, 12, 16, 20, 24) * 3600
    ),
    Status = c(NA, "awake", "nonwear", "asleep", "awake")
  )

  for (use_factor in c(FALSE, TRUE)) {
    input <- sample
    if (use_factor) input$Status <- factor(input$Status)
    expect_silent(
      result <- sleep_int2Brown(
        input,
        Interval.colname = Span,
        Sleep.colname = Status,
        wake.state = "awake",
        sleep.state = "asleep",
        Brown.day = "D",
        Brown.evening = "E",
        Brown.night = "N",
        evening.length = 3600,
        Brown.state.colname = Phase
      )
    )
    expected_starts <- origin + c(0, 4, 12, 15, 16, 20) * 3600
    expected_ends <- origin + c(4, 12, 15, 16, 20, NA) * 3600
    expect_identical(result$Phase, c(NA, "D", "nonwear", "E", "N", "D"))
    expect_identical(lubridate::int_start(result$Span), expected_starts)
    expect_identical(lubridate::int_end(result$Span), expected_ends)
  }
})

test_that("sleep_int2Brown keeps grouping and fills metadata within each group", {
  origin <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  sample <- tibble::tibble(
    Id = rep(c("A", "B"), each = 4),
    Interval = rep(
      lubridate::interval(
        origin + c(6, 23, 30, 46) * 3600,
        origin + c(23, 30, 46, 54) * 3600
      ),
      2
    ),
    State = rep(c("wake", "sleep"), 4),
    Visit = c(1L, 2L, 3L, 4L, NA_integer_, 6L, 7L, 8L)
  ) |>
    dplyr::group_by(Id)

  # Selecting the interval/state columns reports the retained grouping column.
  expect_silent(result <- suppressMessages(sleep_int2Brown(sample)))
  expect_identical(dplyr::group_vars(result), "Id")
  expect_identical(result$Id, rep(c("A", "B"), each = 6))
  expect_identical(result$State.Brown, rep(c("day", "evening", "night"), 4))
  expect_identical(
    result$Visit,
    c(1L, 1L, 2L, 3L, 3L, 4L, NA, NA, 6L, 7L, 7L, 8L)
  )
})
