test_that("extract_clusters() works with basic cases", {
  # Create minimal test data
  test_data <- tibble::tibble(
    Datetime = lubridate::ymd_hm("2023-01-01 00:00") + lubridate::minutes(0:59),
    lux = c(rep(0, 15), rep(1500, 30), rep(0, 15))
  ) |> dplyr::group_by(Id = 1)
  
  # Test minimum duration requirement
  clusters <- extract_clusters(test_data, lux > 1000, cluster.duration = lubridate::dminutes(20))
  expect_equal(nrow(clusters), 1)
  expect_equal(clusters$duration, lubridate::dminutes(30))
  
  # Test maximum duration requirement
  clusters_max <- extract_clusters(test_data, lux > 1000, 
                                  cluster.duration = lubridate::dminutes(40), 
                                  duration.type = "max")
  expect_equal(nrow(clusters_max), 1)
})

test_that("extract_clusters() handles interruptions correctly", {
  test_data <- tibble::tibble(
    Datetime = lubridate::ymd_hm("2023-01-01 00:00") + lubridate::minutes(0:59),
    lux = c(rep(1500, 10), rep(0, 5), rep(1500, 15), rep(0, 30))
  ) |> dplyr::group_by(Id = 1)
  
  # Allow 5 minute interruptions
  clusters <- extract_clusters(test_data, lux > 1000, 
                              cluster.duration = lubridate::dminutes(20),
                              interruption.duration = lubridate::dminutes(5))
  expect_equal(nrow(clusters), 1)
  expect_equal(clusters$duration, lubridate::dminutes(30))
})

test_that("add_clusters() correctly joins cluster info", {
  test_data <- tibble::tibble(
    Datetime = lubridate::ymd_hm("2023-01-01 00:00") + lubridate::minutes(seq(0, 85, by = 5)),
    lux = rep(c(0, 1500), each = 9) %>% head(19)
  ) |> dplyr::group_by(Id = 1)
  
  data_with_clusters <- add_clusters(test_data, lux > 1000, 
                                     cluster.duration = lubridate::dminutes(30))
  
  expect_true("state" %in% names(data_with_clusters))
  expect_equal(sum(!is.na(data_with_clusters$state)), 9)
  expect_equal(nrow(dplyr::distinct(data_with_clusters, state)), 2)
})

test_that("Functions handle empty cases correctly", {
  empty_data <- tibble::tibble(
    Datetime = lubridate::ymd_hm("2023-01-01 00:00"),
    lux = 0
  ) |> dplyr::group_by(Id = 1)
  
  # Test extract_clusters
  expect_warning(
  expect_message(extract_clusters(empty_data, lux > 1000), 
                 "No clusters of condition: lux > 1000 found")
  )
  
  # Test add_clusters
  clusters_added <- 
    suppressWarnings(
      suppressMessages(
        add_clusters(empty_data, lux > 1000)
        )
    )
  expect_warning(clusters_added$cluster)
})

test_that("Grouped data handling works", {
  test_data <- tibble::tibble(
    Id = rep(c("A", "B"), each = 30),
    Datetime = rep(lubridate::ymd_hm("2023-01-01 00:00") + lubridate::minutes(0:29), 2),
    lux = rep(c(rep(1500, 20), rep(0, 10)), 2)
  )
  
  clusters <- test_data %>%
    dplyr::group_by(Id) %>%
    extract_clusters(lux > 1000, cluster.duration = "20 mins")
  
  expect_equal(nrow(clusters), 2)
  expect_setequal(clusters$Id, c("A", "B"))
})

test_that("Duration calculations are precise", {
  precise_data <- tibble::tibble(
    Datetime = lubridate::ymd_hm("2023-01-01 12:00") + lubridate::seconds(seq(0, 300*17, by = 300)),
    lux = c(rep(0, 6), rep(2000, 6), rep(0, 6))
  ) |> dplyr::group_by(Id = 1)
  
  clusters <- extract_clusters(precise_data, lux > 1000, cluster.duration = "1500 secs")
  expected_duration <- lubridate::dseconds(6*300) # 6 intervals = 1800 seconds
  
  expect_equal(clusters$duration, expected_duration)
})

