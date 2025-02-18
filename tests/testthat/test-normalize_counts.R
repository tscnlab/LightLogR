test_that("normalize_counts works", {
  example.table <-
  tibble::tibble(
  uvGain = c(4096, 1024, 2),
  visGain = c(4096, 4096, 4096),
  irGain = c(2,2,2),
  uvValue = c(692, 709, 658),
  visValue = c(128369, 129657, 128609),
  irValue = c(122193, 127113, 124837))

  gain.columns = c("uvGain", "visGain", "irGain")
  count.columns = c("uvValue", "visValue", "irValue")
  
  normalized_counts <- 
    example.table |> 
    normalize_counts(gain.columns, count.columns, gain.ratio.tables$TSL2585)
  
  expect_equal(
    normalized_counts |> length(), 9)
  expect_equal(normalized_counts |> nrow(), 3)
  expect_true(normalized_counts |> purrr::map_lgl(is.numeric) |> all())
})
