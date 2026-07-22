test_that("import works", {
  path <- 
    system.file(
      "extdata", 
      package = "LightLogR"
      )
  filename <- "205_actlumus_Log_1020_20230904101707532.txt.zip"
  tz <- "UTC"
  pattern <- "^(\\d{3})"
  data <- import$ActLumus(filename, path, tz = tz, auto.id = pattern, silent = TRUE)
  import_cols <- c("Id",
                   "file.name",
                   "Datetime",
                   "MS",
                   "EVENT",
                   "TEMPERATURE",
                   "EXT.TEMPERATURE",
                   "ORIENTATION",
                   "PIM",
                   "PIMn",
                   "TAT",
                   "TATn",
                   "ZCM",
                   "ZCMn",
                   "LIGHT",
                   "AMB.LIGHT",
                   "RED.LIGHT",
                   "GREEN.LIGHT",
                   "BLUE.LIGHT",
                   "IR.LIGHT",
                   "UVA.LIGHT",
                   "UVB.LIGHT",
                   "STATE",
                   "CAP_SENS_1",
                   "CAP_SENS_2",
                   "F1",
                   "F2",
                   "F3",
                   "F4",
                   "F5",
                   "F6",
                   "F7",
                   "F8",
                   "MEDI",
                   "CLEAR")
  #check if the data has the correct dimensions
  expect_equal(dim(data), c(61016, 35))
  #check if the data has the correct column names
  expect_equal(names(data), import_cols)
  #check if the function extracted the correct name from the filepath and whether there is only one
  expect_equal(unique(data$Id) %>% as.character(), "205")
})

test_that("VEET imports TOF with typed columns", {
  filename <- tempfile(fileext = ".csv")
  writeLines(
    c(
      paste(c(1719835200, "TOF", seq_len(256)), collapse = ","),
      paste(c(1719835260, "TOF", seq_len(256) + 1), collapse = ",")
    ),
    filename
  )

  data <- import$VEET(
    filename,
    modality = "TOF",
    auto.plot = FALSE,
    silent = TRUE
  )

  expect_equal(nrow(data), 2)
  expect_true(is.numeric(data$time_stamp))
  expect_true(is.numeric(data$conf1_0))
  expect_equal(data$dist2_63, c(256, 257))
})

test_that("VEET TOF auto plot runs without errors", {
  filename <- tempfile(fileext = ".csv")
  writeLines(
    c(
      paste(c(1719835200, "TOF", seq_len(256)), collapse = ","),
      paste(c(1719835260, "TOF", seq_len(256) + 1), collapse = ",")
    ),
    filename
  )

  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  on.exit(grDevices::dev.off(), add = TRUE)

  data <- NULL
  expect_output(
    data <- import$VEET(
      filename,
      modality = "TOF",
      auto.plot = TRUE,
      silent = FALSE
    ),
    "Successfully read in"
  )

  expect_equal(nrow(data), 2)
})
