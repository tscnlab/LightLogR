test_that("extract_metric() calculates metrics correctly", {
  test_data <- tibble::tibble(
    Datetime = lubridate::ymd_hm("2023-01-01 00:00") + lubridate::minutes(0:59),
    lux = c(rep(100, 30), rep(2000, 30)),
    temp = runif(60, 20, 25)
  ) |> dplyr::group_by(Id = 1)
  
  clusters <- extract_clusters(test_data, lux > 1500)
  clusters_metrics <- extract_metric(clusters, test_data,
                                     mean_lux = mean(lux),
                                     max_temp = max(temp))
  
  expect_equal(clusters_metrics$mean_lux, 2000)
  expect_true(clusters_metrics$max_temp > 20)
})
