test_that("summarize_numeric() produces correct summaries", {
  test_data <- tibble::tibble(
    Datetime = lubridate::ymd_hm("2023-01-01 00:00") + lubridate::minutes(0:59),
    lux = c(rep(0, 20), rep(3000, 20), rep(0, 20))
  ) |> dplyr::group_by(Id = 1)
  
  clusters <- extract_clusters(test_data, lux > 2000, cluster.duration = "20 mins")
  summary <- summarize_numeric(clusters)
  
  expect_equal(summary$mean_duration, lubridate::dminutes(20))
  expect_equal(summary$episodes, 1)
})
