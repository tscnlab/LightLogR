test_that("count_difftime works", {
  filepath <- system.file("extdata/sample_data_LYS.csv", package = "LightLogR")
  dataset <- import$LYS(filepath, silent = TRUE)
  dataset <- count_difftime(dataset)
  dataset <- dataset$difftime
  expect_equal(
    dataset, c("15s", "16s", "17s", "18s") %>% lubridate::as.duration()
    )
})
