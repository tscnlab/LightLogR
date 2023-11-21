test_that("gg_overview works", {
  Plot <- sample.data.environment %>% gg_overview()
  expect_snapshot(Plot$data)
})
