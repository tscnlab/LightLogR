test_that("gg_overview works", {
  Plot <- sample.data.environment %>% gg_overview(Id.colname = Source)
  expect_snapshot(Plot$data)
})
