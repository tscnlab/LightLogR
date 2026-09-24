lys_pro_fixture <- function() {
  tibble::tibble(
    Email = "person@example.test/TL-001",
    `Timestamp (UTC)` = c(
      "2026-09-21 08:26:27+00:00",
      "2026-09-21 08:26:42+00:00",
      "2026-09-21 08:26:57+00:00",
      "2026-09-21 08:27:12+00:00"
    ),
    Clear = c(1642.5, 0, 2583, NA),
    F1 = c(104.5, 0, 167, NA),
    F2 = 153,
    F3 = 232,
    F4 = 313,
    F5 = 374,
    F6 = 402,
    F7 = 504,
    F8 = 560,
    NIR = 687,
    Flicker = c("None", "100", "None", "120"),
    Movement = c(6, 0, 0, 0),
    Lux = c(286, 0, 452, NA),
    mEDI = c(223, 0, 357, NA),
    CCT = c(4685, 4672, 4770, NA)
  )
}

lys_legacy_fixture <- function() {
  tibble::tibble(
    timestamp = c(
      "21-06-2023 00.00.12",
      "21-06-2023 00.00.27",
      "21-06-2023 00.00.42",
      "21-06-2023 00.00.57"
    ),
    sensor = c("B", "A", "B", "A"),
    lux = c(12, 15, 17, NA),
    kelvin = 4000,
    rgbR = 0.12,
    rgbG = 0.15,
    rgbB = 0.1,
    rgbIR = 0,
    movement = 0,
    mEDI = c(10, 12, 14, NA),
    `R'` = 1,
    `G'` = 2,
    `B'` = 3
  )
}

write_lys_fixture <- function(path, data = lys_pro_fixture()) {
  readr::write_csv(data, path, na = "")
}

test_that("LYS preserves PRO measurements and missing flicker values", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  write_lys_fixture(path)

  expect_silent(data <- import$LYS(path, silent = TRUE, guess_max = 1))
  expect_s3_class(data, "grouped_df")
  expect_identical(dim(data), c(4L, 19L))
  expect_identical(dplyr::group_vars(data), "Id")
  expect_identical(lubridate::tz(data$Datetime), "UTC")
  expect_identical(
    data$Datetime,
    as.POSIXct("2026-09-21 08:26:27", tz = "UTC") + c(0, 15, 30, 45)
  )
  expect_identical(data$Email, rep("person@example.test/TL-001", 4))
  expect_identical(data$MEDI, c(223, 0, 357, NA_real_))
  expect_identical(data$Lux, c(286, 0, 452, NA_real_))
  expect_identical(data$Clear, c(1642.5, 0, 2583, NA_real_))
  expect_identical(data$F1, c(104.5, 0, 167, NA_real_))
  expect_identical(data$F8, rep(560, 4))
  expect_identical(data$Movement, c(6, 0, 0, 0))
  expect_identical(data$Flicker, c(NA_real_, 100, NA_real_, 120))
})

test_that("LYS preserves legacy timestamps, factor levels, and numeric types", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  write_lys_fixture(path, lys_legacy_fixture())

  expect_silent(data <- import$LYS(path, tz = "Europe/Berlin", silent = TRUE))
  expect_identical(dim(data), c(4L, 15L))
  expect_identical(
    data$sensor,
    factor(c("B", "A", "B", "A"), levels = c("B", "A"))
  )
  expect_identical(
    data$Datetime,
    as.POSIXct("2023-06-21 02:00:12", tz = "Europe/Berlin") + c(0, 15, 30, 45)
  )
  expect_identical(data$MEDI, c(10, 12, 14, NA_real_))
  expect_identical(data$lux, c(12, 15, 17, NA_real_))
  expect_identical(data$rgbIR, rep(0, 4))
  expect_identical(data$R., rep(1, 4))
})

test_that("LYS is shared by all import entry points", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  write_lys_fixture(path)

  data <- import$LYS(path, silent = TRUE)
  expect_true("LYS" %in% supported_devices())
  expect_identical(import_Dataset("LYS", path, silent = TRUE), data)
  adjusted <- import_adjustment(ll_import_expr())
  expect_identical(adjusted$LYS(path, silent = TRUE), data)
})

test_that("LYS honors UTC offsets and converts to the requested timezone", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  fixture <- lys_pro_fixture()
  fixture$`Timestamp (UTC)` <- c(
    "2026-09-21 10:26:27+02:00",
    "2026-09-21T08:26:42Z",
    "2026-09-21 03:26:57-05:00",
    "2026-09-21 08:27:12"
  )
  write_lys_fixture(path, fixture)

  expect_silent(data <- import$LYS(path, tz = "Europe/Berlin", silent = TRUE))
  expect_identical(
    data$Datetime,
    as.POSIXct("2026-09-21 10:26:27", tz = "Europe/Berlin") + c(0, 15, 30, 45)
  )
})

test_that("LYS selects date order explicitly and retains fractional seconds", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  fixture <- lys_legacy_fixture()
  fixture$timestamp <- c(
    "01-09-2026 08.26.27",
    "2026-09-01 08:26:42+00:00",
    "01/09/2026 08:26:57.125",
    "2026-09-01 08:27:12.5+00:00"
  )
  write_lys_fixture(path, fixture)

  expect_silent(data <- import$LYS(path, silent = TRUE))
  expect_identical(
    data$Datetime,
    as.POSIXct("2026-09-01 08:26:27", tz = "UTC") + c(0, 15, 30.125, 45.5)
  )
})

test_that("LYS accepts minimal and reordered optional columns", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  fixture <- lys_pro_fixture()
  layouts <- list(
    c("Timestamp (UTC)", "mEDI"),
    c("mEDI", "Flicker", "Timestamp (UTC)", "Lux"),
    c("Lux", "mEDI", "Email", "Timestamp (UTC)")
  )

  for (columns in layouts) {
    write_lys_fixture(path, fixture[columns])
    expect_silent(data <- import$LYS(path, silent = TRUE))
    expected_names <- columns
    expected_names[expected_names == "Timestamp (UTC)"] <- "Datetime"
    expected_names[expected_names == "mEDI"] <- "MEDI"
    expect_named(data, c("Id", "file.name", expected_names))
    expect_identical(data$MEDI, c(223, 0, 357, NA_real_))
  }
})

test_that("LYS accepts timestamp suffixes without relying on column position", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  fixture <- lys_legacy_fixture()[c("sensor", "mEDI", "timestamp")]
  names(fixture)[3] <- "TIMESTAMP_UTC"
  write_lys_fixture(path, fixture)

  expect_silent(data <- import$LYS(path, silent = TRUE))
  expect_named(data, c("Id", "file.name", "sensor", "MEDI", "Datetime"))
  expect_s3_class(data$sensor, "factor")
  expect_identical(data$MEDI, c(10, 12, 14, NA_real_))
})

test_that("omitting optional LYS fields leaves the other columns unchanged", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  fixture <- lys_pro_fixture()
  write_lys_fixture(path, fixture)
  full <- import$LYS(path, silent = TRUE)

  fixture$Email <- NULL
  fixture$Flicker <- NULL
  write_lys_fixture(path, fixture)
  expect_silent(reduced <- import$LYS(path, silent = TRUE))
  expect_identical(reduced, dplyr::select(full, -Email, -Flicker))
})

test_that("LYS keeps known empty measurements numeric and infers additional fields", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  fixture <- lys_pro_fixture()
  fixture$mEDI <- NA_real_
  fixture$F1 <- NA_real_
  fixture$Flicker <- "None"
  fixture$Email <- NA_character_
  fixture$Comment <- c("morning", "outside", "inside", "evening")
  fixture$Extra <- c(1.5, 2.5, 3.5, NA)
  write_lys_fixture(path, fixture)

  expect_silent(data <- import$LYS(path, silent = TRUE))
  expect_identical(data$MEDI, rep(NA_real_, 4))
  expect_identical(data$F1, rep(NA_real_, 4))
  expect_identical(data$Flicker, rep(NA_real_, 4))
  expect_identical(data$Email, rep(NA_character_, 4))
  expect_identical(data$Comment, fixture$Comment)
  expect_identical(data$Extra, fixture$Extra)
})

test_that("LYS applies the first file's reduced layout to a batch", {
  paths <- c(tempfile(fileext = ".csv"), tempfile(fileext = ".csv"))
  on.exit(unlink(paths), add = TRUE)
  fixture <- lys_pro_fixture()[c("Timestamp (UTC)", "mEDI")]
  write_lys_fixture(paths[1], fixture)
  fixture$mEDI <- c(100, 200, 300, 400)
  write_lys_fixture(paths[2], fixture)

  expect_silent(data <- import$LYS(paths, silent = TRUE))
  expect_identical(dim(data), c(8L, 4L))
  expect_identical(as.integer(table(data$Id)), c(4L, 4L))
  second <- dplyr::filter(
    data,
    .data$Id == tools::file_path_sans_ext(basename(paths[2]))
  )
  expect_identical(second$MEDI, c(100, 200, 300, 400))
})

test_that("LYS forwards paths, limits, ID, type, and missing-value overrides", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  write_lys_fixture(path)

  expect_silent(
    data <- import$LYS(
      basename(path),
      path = dirname(path),
      n_max = 3,
      manual.id = "PRO",
      silent = TRUE,
      col_types = readr::cols(.default = readr::col_character()),
      na = c("", "NA")
    )
  )
  expect_identical(nrow(data), 3L)
  expect_identical(as.character(data$Id), rep("PRO", 3))
  expect_identical(data$MEDI, c("223", "0", "357"))
  expect_identical(data$Flicker, c("None", "100", "None"))
})

test_that("LYS honors column selection and explicit type inference", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  write_lys_fixture(path)

  expect_silent(
    selected <- import$LYS(
      path,
      silent = TRUE,
      col_select = c("Timestamp..UTC.", "mEDI", "Lux")
    )
  )
  expect_named(selected, c("Id", "file.name", "Datetime", "MEDI", "Lux"))
  inferred <- import$LYS(path, silent = TRUE, col_types = NULL)
  expect_identical(
    inferred,
    import$LYS(path, silent = TRUE, col_types = readr::cols())
  )
  expect_identical(inferred$Datetime, selected$Datetime)
})

test_that("LYS requires mEDI and one unambiguous timestamp column", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  fixture <- lys_pro_fixture()
  write_lys_fixture(path, fixture[setdiff(names(fixture), "mEDI")])
  expect_error(import$LYS(path, silent = TRUE), "mEDI")

  write_lys_fixture(path, fixture[setdiff(names(fixture), "Timestamp (UTC)")])
  expect_error(import$LYS(path, silent = TRUE), "exactly one timestamp column")

  fixture$timestamp_local <- fixture$`Timestamp (UTC)`
  write_lys_fixture(path, fixture)
  expect_error(import$LYS(path, silent = TRUE), "exactly one timestamp column")
})
