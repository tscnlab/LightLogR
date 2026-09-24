actlumus_fixture <- function() {
  # Synthetic measurements with the column layout of the supplied AL0102 log.
  tibble::tibble(
    `DATE/TIME` = c(
      "22/09/2026 14:16:48",
      "22/09/2026 14:17:48",
      "22/09/2026 14:18:48",
      "22/09/2026 14:19:48"
    ),
    EVENT = c(0, 0, 0, 1),
    `SLEEP EVENT` = c(0, 0, 0, 1),
    `SLEEP EVENT DESC` = c(NA, NA, NA, "EVENT"),
    TEMPERATURE = c(23, 24, 25, 26),
    ORIENTATION = 16,
    `ORIENTATION DESC` = "Z Plus",
    PIM = c(25, 0, 10, 5),
    PIMn = c(25, 0, 10, 5),
    TAT = 0,
    TATn = 0,
    ZCM = c(3, 0, 2, 1),
    ZCMn = c(3, 0, 2, 1),
    LIGHT = c(15, 0, 30, NA),
    `IR LIGHT` = 0.001,
    CAP_SENS_1 = 200,
    CAP_SENS_2 = 160,
    F1 = 0.001,
    F2 = 0.002,
    F3 = 0.003,
    F4 = 0.004,
    F5 = 0.005,
    F6 = 0.006,
    F7 = 0.007,
    F8 = 0.008,
    `MELANOPIC EDI` = c(10, 0, 20, NA),
    CLEAR = 0.01,
    `S CONE OPIC EDI` = c(5, 0, 10, NA),
    `M CONE OPIC EDI` = c(12, 0, 24, NA),
    `L CONE OPIC EDI` = c(14, 0, 28, NA),
    `RHODOPIC EDI` = c(11, 0, 22, NA),
    `Z LIGHT` = 0.01,
    `Y LIGHT` = 0.02,
    `X LIGHT` = 0.03,
    `FD LIGHT` = 0,
    CLA2 = c(13, 0, 26, NA),
    CS = c(0.02, 0, 0.04, NA),
    `INITIAL STATE` = c(4, 0, 0, 0),
    STATE = c(4, 0, 0, 0),
    `STATE DESC` = c("OffWrist", "Awake", "Awake", "Awake"),
    MODEL = "AL0102"
  )
}

write_actlumus_fixture <- function(path, data = actlumus_fixture()) {
  readr::write_lines(
    c(
      "#ActLogModel=2.0.0",
      "LOG_FILE_VERSION : 1.0.8",
      "DEVICE_MODEL : AL0102"
    ),
    path
  )
  readr::write_delim(
    data,
    path,
    delim = ";",
    na = "",
    append = TRUE,
    col_names = TRUE
  )
}

test_that("ActLumus preserves Plus measurements and sparse descriptions", {
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path), add = TRUE)
  write_actlumus_fixture(path)

  expect_silent(
    data <- import$ActLumus(
      path,
      tz = "Europe/Berlin",
      manual.id = "plus",
      silent = TRUE,
      guess_max = 1
    )
  )

  expect_s3_class(data, "grouped_df")
  expect_identical(dim(data), c(4L, 43L))
  expect_identical(dplyr::group_vars(data), "Id")
  expect_identical(as.character(data$Id), rep("plus", 4))
  expect_identical(lubridate::tz(data$Datetime), "Europe/Berlin")
  expect_equal(
    data$Datetime,
    as.POSIXct("2026-09-22 14:16:48", tz = "Europe/Berlin") + c(0, 60, 120, 180)
  )
  expect_identical(data$MEDI, c(10, 0, 20, NA_real_))
  expect_identical(data$LIGHT, c(15, 0, 30, NA_real_))
  expect_identical(data$S.CONE.OPIC.EDI, c(5, 0, 10, NA_real_))
  expect_identical(data$M.CONE.OPIC.EDI, c(12, 0, 24, NA_real_))
  expect_identical(data$L.CONE.OPIC.EDI, c(14, 0, 28, NA_real_))
  expect_identical(data$RHODOPIC.EDI, c(11, 0, 22, NA_real_))
  expect_identical(data$SLEEP.EVENT.DESC, c(NA, NA, NA, "EVENT"))
  expect_identical(data$STATE.DESC, c("OffWrist", "Awake", "Awake", "Awake"))
  expect_identical(data$ORIENTATION.DESC, rep("Z Plus", 4))
  expect_identical(data$MODEL, rep("AL0102", 4))
  expect_identical(data$F1, rep(0.001, 4))
  expect_identical(data$F8, rep(0.008, 4))
  expect_identical(data$CS, c(0.02, 0, 0.04, NA_real_))
})

test_that("ActLumus is available through all import entry points", {
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path), add = TRUE)
  write_actlumus_fixture(path)

  expect_true("ActLumus" %in% supported_devices())
  expect_identical(supported_versions("ActLumus")$Version, "initial")
  direct <- import$ActLumus(path, silent = TRUE)
  expect_identical(import_Dataset("ActLumus", path, silent = TRUE), direct)
  adjusted <- import_adjustment(ll_import_expr())
  expect_identical(adjusted$ActLumus(path, silent = TRUE), direct)
})

test_that("ActLumus forwards limits, paths, and readr overrides", {
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path), add = TRUE)
  write_actlumus_fixture(path)

  expect_silent(
    data <- import$ActLumus(
      basename(path),
      path = dirname(path),
      n_max = 3,
      silent = TRUE,
      col_types = readr::cols(.default = readr::col_character()),
      na = c("", "NA", "20")
    )
  )
  expect_identical(nrow(data), 3L)
  expect_identical(data$MEDI, c("10", "0", NA_character_))
  expect_identical(data$PIM, c("25", "0", "10"))
  expect_identical(data$SLEEP.EVENT.DESC, rep(NA_character_, 3))
  expect_identical(lubridate::tz(data$Datetime), "UTC")
})

test_that("ActLumus retains separate IDs for multiple files", {
  paths <- c(tempfile(fileext = ".txt"), tempfile(fileext = ".txt"))
  on.exit(unlink(paths), add = TRUE)
  write_actlumus_fixture(paths[1])
  write_actlumus_fixture(paths[2])

  expect_silent(data <- import$ActLumus(paths, silent = TRUE))
  expect_identical(nrow(data), 8L)
  expect_setequal(
    as.character(data$Id),
    tools::file_path_sans_ext(basename(paths))
  )
  expect_identical(as.integer(table(data$Id)), c(4L, 4L))
  expect_identical(sum(data$SLEEP.EVENT.DESC == "EVENT", na.rm = TRUE), 2L)
})

test_that("ActLumus rejects files without its table header", {
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path), add = TRUE)
  readr::write_lines(c("DEVICE_MODEL : AL0102", "not a data table"), path)

  expect_error(
    import$ActLumus(path, silent = TRUE),
    "Could not find a line with this order of column names"
  )
})

test_that("omitting a description leaves the other ActLumus columns unchanged", {
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path), add = TRUE)
  fixture <- actlumus_fixture()
  write_actlumus_fixture(path, fixture)
  full <- import$ActLumus(path, silent = TRUE)

  fixture$`SLEEP EVENT DESC` <- NULL
  write_actlumus_fixture(path, fixture)
  expect_silent(reduced <- import$ActLumus(path, silent = TRUE))
  expect_identical(reduced, dplyr::select(full, -"SLEEP.EVENT.DESC"))
})

test_that("ActLumus accepts minimal and reordered export layouts", {
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path), add = TRUE)
  fixture <- actlumus_fixture()
  layouts <- list(
    c("DATE/TIME", "MELANOPIC EDI"),
    c("DATE/TIME", "MELANOPIC EDI", "STATE DESC", "LIGHT"),
    c("DATE/TIME", "LIGHT", "TEMPERATURE", "MELANOPIC EDI", "MODEL")
  )

  for (columns in layouts) {
    write_actlumus_fixture(path, fixture[columns])
    expect_silent(data <- import$ActLumus(path, silent = TRUE))
    expected_names <- make.names(columns)
    expected_names[expected_names == "DATE.TIME"] <- "Datetime"
    expected_names[expected_names == "MELANOPIC.EDI"] <- "MEDI"
    expect_named(data, c("Id", "file.name", expected_names))
    expect_identical(data$MEDI, c(10, 0, 20, NA_real_))
    expect_identical(nrow(data), 4L)
  }
})

test_that("ActLumus infers types for additional export columns", {
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path), add = TRUE)
  fixture <- actlumus_fixture()
  fixture$COMMENT <- c("morning", "indoors", "outside", "evening")
  fixture$`EXTRA MEASUREMENT` <- c(2.5, 3.5, NA_real_, 4.5)
  write_actlumus_fixture(path, fixture)

  expect_silent(data <- import$ActLumus(path, silent = TRUE))
  expect_identical(data$COMMENT, fixture$COMMENT)
  expect_identical(data$EXTRA.MEASUREMENT, fixture$`EXTRA MEASUREMENT`)
  expect_identical(data$SLEEP.EVENT.DESC, c(NA, NA, NA, "EVENT"))
})

test_that("known ActLumus columns keep their types when entirely missing", {
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path), add = TRUE)
  fixture <- actlumus_fixture()
  fixture$`MELANOPIC EDI` <- NA_real_
  fixture$LIGHT <- NA_real_
  fixture$`SLEEP EVENT DESC` <- NA_character_
  fixture$MODEL <- NA_character_
  write_actlumus_fixture(path, fixture)

  expect_silent(data <- import$ActLumus(path, silent = TRUE, guess_max = 1))
  expect_identical(data$MEDI, rep(NA_real_, 4))
  expect_identical(data$LIGHT, rep(NA_real_, 4))
  expect_identical(data$SLEEP.EVENT.DESC, rep(NA_character_, 4))
  expect_identical(data$MODEL, rep(NA_character_, 4))
})

test_that("ActLumus applies a reduced first-file layout to the entire batch", {
  paths <- c(tempfile(fileext = ".txt"), tempfile(fileext = ".txt"))
  on.exit(unlink(paths), add = TRUE)
  fixture <- actlumus_fixture()[c("DATE/TIME", "MELANOPIC EDI")]
  fixture$COMMENT <- c("a", "b", "c", "d")
  write_actlumus_fixture(paths[1], fixture)
  fixture$`MELANOPIC EDI` <- c(40, 50, 60, 70)
  fixture$COMMENT <- c("e", "f", "g", "h")
  write_actlumus_fixture(paths[2], fixture)

  expect_silent(data <- import$ActLumus(paths, silent = TRUE))
  expect_identical(nrow(data), 8L)
  expect_named(data, c("Id", "file.name", "Datetime", "MEDI", "COMMENT"))
  second_id <- tools::file_path_sans_ext(basename(paths[2]))
  second_file <- dplyr::filter(data, .data$Id == second_id)
  expect_identical(second_file$MEDI, c(40, 50, 60, 70))
  expect_identical(second_file$COMMENT, c("e", "f", "g", "h"))
})

test_that("ActLumus honors column selection and explicit type inference", {
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path), add = TRUE)
  write_actlumus_fixture(path)

  expect_silent(
    selected <- import$ActLumus(
      path,
      silent = TRUE,
      col_select = c("DATE.TIME", "MELANOPIC.EDI", "MODEL")
    )
  )
  expect_named(selected, c("Id", "file.name", "Datetime", "MEDI", "MODEL"))
  expect_identical(selected$MEDI, c(10, 0, 20, NA_real_))
  inferred <- import$ActLumus(path, silent = TRUE, col_types = NULL)
  expect_identical(
    inferred,
    import$ActLumus(path, silent = TRUE, col_types = readr::cols())
  )
})

test_that("ActLumus still requires the melanopic measurement column", {
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path), add = TRUE)
  fixture <- actlumus_fixture()[c("DATE/TIME", "LIGHT")]
  write_actlumus_fixture(path, fixture)

  expect_error(import$ActLumus(path, silent = TRUE), "MELANOPIC.EDI")
})
