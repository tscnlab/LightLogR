test_that("count_difftime works", {
  dataset <- count_difftime(sample.data.irregular)
  dataset <- dataset$difftime
  expect_equal(
    dataset, c("15s", "16s", "17s", "18s") %>% lubridate::as.duration()
    )
})

test_that("find_clusters works", {
  data <- as.logical(c(1,1,1,0,0,1,0,1,1,0,1,1))
  reference.1 <- tibble::tibble(
    row_idx = c(1,2,3,8,9,11,12),
    is_cluster = c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE),
    cluster_idx = c(1,1,1,2,2,3,3),
    cluster_start = c(1,1,1,8,8,11,11),
    cluster_end = c(3,3,3,9,9,12,12)
  )
  reference.2 <- tibble::tibble(
    row_idx = c(1,2,3,8,9,10,11,12),
    is_cluster = c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE),
    cluster_idx = c(1,1,1,2,2,2,2,2),
    cluster_start = c(1,1,1,8,8,8,8,8),
    cluster_end = c(3,3,3,12,12,12,12,12)
  )
  expect_equal(find_clusters(data, min.length = 2), reference.1)
  expect_equal(find_clusters(data, min.length = 2, max.interrupt = 1), reference.2)
  expect_equal(find_clusters(data, min.length = 2, max.interrupt = 1, prop.interrupt = 0.1), reference.1)
  expect_equal(find_clusters(data, min.length = 2, max.interrupt = 1, prop.interrupt = 0.25), reference.2)
})
