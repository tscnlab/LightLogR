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
  expect_message(extract_clusters(empty_data, lux > 1000), 
                 "No clusters of condition: lux>1000|d≥30mins found")
  
  # Test add_clusters
  expect_message(add_clusters(empty_data, lux > 1000),
                 "No clusters of condition: lux>1000|d≥30mins found")
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

#-------
test_that("add_clusters basic functionality", {
  data_test <- tibble::tibble(
    Id = "A",
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(seq(0, 50, by = 10)), # 0, 10, 20, 30, 40, 50
    Value = c(100, 200, 50, 300, 400, 50) # Condition Value > 150
    # T: 10 (200), 30 (300), 40 (400)
    # F: 0 (100), 20 (50), 50 (50)
  ) %>% dplyr::group_by(Id)
  
  # Condition: Value > 150. Cluster duration: min 20 minutes (2 intervals of 10min)
  # Epoch is 10 min.
  # States for Value > 150: F (0), T (10), F (20), T (30), T (40), F (50)
  # Episodes from extract_clusters:
  #   1. T at 10min. Duration 10min. Not a cluster.
  #   2. T at 30-40min. Duration 20min. IS a cluster.
  #     start = 30m - 5m = 25m. end = 40m + 5m = 45m.
  result <- add_clusters(data_test, Value > 150, cluster.duration = "20 mins")
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 6) # Same as input data
  expect_true("state" %in% names(result)) # Default cluster.colname
  
  # Check cluster assignment:
  # Datetimes: 0, 10, 20, 30, 40, 50
  # Expected state column: NA, NA, NA, "1", "1", NA
  # Cluster 1 (Value > 150 for >=20min) is for DT 30 and 40.
  expect_equal(result$state, c(NA, NA, NA, "1", "1", NA))
})

test_that("add_clusters with multiple groups", {
  data_test_multi <- tibble::tibble(
    Id = rep(c("G1", "G2"), each = 4),
    Datetime = lubridate::as_datetime(0) + lubridate::hours(rep(0:3, 2)),
    Reading = c(5, 15, 12, 8,  # G1: T at 1h, 2h
                20, 8, 22, 25) # G2: T at 0h, 2h, 3h
  ) %>% dplyr::group_by(Id)
  
  # Condition: Reading > 10. Cluster duration: min 2 hours (2 intervals of 1h)
  # G1: Reading > 10 at 1h (15), 2h (12). This is a 2h cluster.
  #   start = 1h-0.5h=0.5h. end = 2h+0.5h=2.5h. Affects DT 1h, 2h.
  # G2: Reading > 10 at 0h (20), 2h (22), 3h (25).
  #   Episode 1: 0h (20). Duration 1h. Not cluster.
  #   Episode 2: 2h (22), 3h (25). Duration 2h. IS cluster.
  #   start = 2h-0.5h=1.5h. end = 3h+0.5h=3.5h. Affects DT 2h, 3h.
  
  result <- add_clusters(data_test_multi, Reading > 10, cluster.duration = "2 hours")
  
  g1_states <- result %>% dplyr::filter(Id == "G1") %>% dplyr::pull(state)
  g2_states <- result %>% dplyr::filter(Id == "G2") %>% dplyr::pull(state)
  
  expect_equal(g1_states, c(NA, "1", "1", NA)) # Cluster "1" specific to G1
  expect_equal(g2_states, c(NA, NA, "1", "1")) # Cluster "1" specific to G2 (renumbered)
})

test_that("add_clusters with interruptions allowed", {
  data_interrupt <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(seq(0, 60, by = 10)), # 0 to 60
    Value = c(100, 80, 120, # T F T (0,10,20) - Interruption at 10 if Value > 90
              70,           # F (30) - Value at 30
              150, 95, 180) # T F T (40,50,60) - Interruption at 50 if Value > 90
  )
  # Condition: Value > 90. Epoch 10 min.
  # Value > 90: T (0), F (10), T (20), F (30), T (40), F (50), T (60)
  # Cluster duration: min 30 mins. Interruption: max 10 mins.
  # Sequence of (Value > 90): T, F, T, F, T, F, T
  # extract_clusters logic:
  #   Initial episodes (type, duration): (T,10m), (F,10m), (T,10m), (F,10m), (T,10m), (F,10m), (T,10m)
  #   Interruption handling:
  #     (F,10m) at DT=10 (idx 2) is between T,T. duration 10m <= interruption.duration 10m. Becomes T.
  #     (F,10m) at DT=30 (idx 4) is between T,T. Becomes T.
  #     (F,10m) at DT=50 (idx 6) is between T,T. Becomes T.
  #   New sequence of types for consecutive_id: T,T,T,T,T,T,T. One long episode.
  #   Summarized episode: start=0-5m, end=60+5m. Duration 70min.
  #   Is cluster? 70min >= 30min. Yes.
  expect_warning(
  result <- add_clusters(data_interrupt, Value > 90,
                         cluster.duration = "30 mins",
                         interruption.duration = "10 mins")
  )
  expect_equal(result$state, rep("1", 7))
})

test_that("add_clusters no clusters found", {
  data_no_cluster <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:2),
    Value = c(10, 20, 15) # Condition Value > 100, will find no episodes > 0s
  )
  expect_warning(
  # Expect message from extract_clusters, then add_clusters returns original data
  expect_message(
    result <- add_clusters(data_no_cluster, Value > 100, cluster.duration = "1 hour"),
    "No clusters of condition: Value>100|d≥1hour found"
  )
  )
  expect_equal(result, data_no_cluster) # Should not have 'state' column or start/end
  expect_false("state" %in% names(result))
  
  # Case where episodes exist, but none meet cluster.duration
  data_short_episodes <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:2),
    Value = c(110, 120, 50) # Value > 100 for 0h,1h (20min episode if epoch=10min, or 2h if epoch=1h)
  )
  # extract_clusters with epoch=1h: Value > 100 -> T, T, F. Episode is 2h long.
  # If cluster.duration = "3 hours", this is not a cluster.
  expect_warning(
  expect_message(
    result2 <- add_clusters(data_short_episodes, Value > 100, cluster.duration = "3 hours"),
    "No clusters of condition: Value>100|d≥3hours found"
  )
  )
  expect_equal(result2, data_short_episodes)
  expect_false("state" %in% names(result2))
})

test_that("add_clusters custom column names", {
  data_custom <- tibble::tibble(
    MyDT = lubridate::as_datetime(0) + lubridate::hours(0:1),
    Activity = c(TRUE, TRUE)
  )
  expect_warning(
  result <- add_clusters(data_custom, Activity,
                         Datetime.colname = MyDT,
                         cluster.colname = ActivityCluster,
                         cluster.duration = "1 hour") # 2h episode, cluster ok
  )
  expect_true("ActivityCluster" %in% names(result))
  expect_equal(result$ActivityCluster, c("1", "1"))
})

test_that("add_clusters handle.gaps functionality", {
  data_gappy_ac <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::hours(c(0, 2, 3)), # Gap at 1h
    Value = c(TRUE, TRUE, TRUE) # All TRUE for condition Value
  )
  # Default handle.gaps = FALSE.
  # extract_clusters sees: T(0h), T(2h), T(3h). Epoch guessed likely 1h.
  # Episodes: (T,1h at 0h), (T,2h at 2-3h).
  # If cluster.duration="1 hour": Both are clusters.
  # Cluster 1: DT 0. state "1"
  # Cluster 2: DT 2,3. state "2" (renumbered)
  expect_warning(
    res_no_gap_handle <- add_clusters(data_gappy_ac, Value, cluster.duration = "1 hour")
  )
  expect_equal(res_no_gap_handle$state, c("1", "1", "1"))
  
  # handle.gaps = TRUE.
  # Data becomes: 0h(T), 1h(NA->F), 2h(T), 3h(T)
  # Episodes (Value): (T,1h at 0h), (F,1h at 1h), (T,2h at 2-3h)
  # If cluster.duration="1 hour": first and third are clusters.
  # Cluster 1: DT 0. state "1"
  # Cluster 2: DT 2,3. state "2" (renumbered)
  expect_warning(
    res_gap_handle <- add_clusters(data_gappy_ac, Value, cluster.duration = "1 hour", handle.gaps = TRUE)
  )
  expect_equal(res_gap_handle$state, c("1", "2", "2")) # NA for the filled gap
})

test_that("add_clusters interaction with return.only.clusters=FALSE in extract_clusters", {
  # add_clusters internally calls extract_clusters with return.only.clusters = TRUE (default).
  # This test ensures that this internal detail is consistent.
  data_roc <- tibble::tibble(
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:3),
    Value = c(TRUE, FALSE, TRUE, TRUE)
  )
  # extract_clusters with Value, duration="1 hour":
  # Episodes: T(0h), F(1h), T(2-3h, 2h duration)
  # Cluster is only the 2-3h one.
  expect_warning(
  result <- add_clusters(data_roc, Value, cluster.duration = "1 hour")
  )
  expect_equal(result$state, c("1", NA, "2", "2"))
})

