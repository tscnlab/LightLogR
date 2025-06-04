test_that("log_zero_inflated works with default parameters", {
  input_vec <- c(0, 1, 10, 100, 1000)
  expected_vec <- log10(input_vec + 0.1)
  expect_equal(log_zero_inflated(input_vec), expected_vec)
})

test_that("log_zero_inflated handles zero correctly", {
  expect_equal(log_zero_inflated(0), log10(0.1))
  expect_equal(log_zero_inflated(c(0, 0, 0)), rep(log10(0.1), 3))
})

test_that("log_zero_inflated works with custom offset", {
  input_vec <- c(0, 1, 10)
  custom_offset <- 0.5
  expected_vec <- log10(input_vec + custom_offset)
  expect_equal(log_zero_inflated(input_vec, offset = custom_offset), expected_vec)
})

test_that("log_zero_inflated works with custom base", {
  input_vec <- c(0, 1, 10)
  custom_base <- 2
  expected_vec <- log(input_vec + 0.1, base = custom_base)
  expect_equal(log_zero_inflated(input_vec, base = custom_base), expected_vec)
  
  # Test with natural logarithm
  expected_vec_ln <- log(input_vec + 0.1, base = exp(1))
  expect_equal(log_zero_inflated(input_vec, base = exp(1)), expected_vec_ln)
})

test_that("log_zero_inflated handles NA values", {
  input_vec_na <- c(0, NA, 10)
  expected_vec_na <- log10(input_vec_na + 0.1) # log10(NA+0.1) is NA
  expect_equal(log_zero_inflated(input_vec_na), expected_vec_na)
})

test_that("log_zero_inflated handles Inf and -Inf", {
  expect_equal(log_zero_inflated(Inf), Inf) # log10(Inf+0.1) is Inf
  # log10(-Inf+0.1) is NaN because -Inf+0.1 is -Inf, log10 of negative is NaN
  expect_warning(
  expect_true(is.nan(log_zero_inflated(-Inf)))
  )
})

test_that("log_zero_inflated handles empty vector", {
  expect_equal(log_zero_inflated(numeric(0)), numeric(0))
})

test_that("log_zero_inflated throws error for non-numeric input", {
  expect_error(log_zero_inflated("a"), "x must be numeric")
  expect_error(log_zero_inflated(TRUE), "x must be numeric")
  expect_error(log_zero_inflated(as.factor("b")), "x must be numeric")
  expect_error(log_zero_inflated(list(1,2)), "x must be numeric")
})


# Tests for exp_zero_inflated

test_that("exp_zero_inflated works with default parameters", {
  input_vec <- c(-1, 0, 1, 2, 3) # Results of log10(c(0,1,10,100,1000)+0.1) approx
  expected_vec <- 10^(input_vec) - 0.1
  expect_equal(exp_zero_inflated(input_vec), expected_vec)
})

test_that("exp_zero_inflated is inverse of log_zero_inflated (default params)", {
  original_vec <- c(0, 1, 10, 100, 1000, 0.005, 123.45)
  transformed_vec <- log_zero_inflated(original_vec)
  reversed_vec <- exp_zero_inflated(transformed_vec)
  expect_equal(reversed_vec, original_vec, tolerance = 1e-9) # Added tolerance for floating point
})

test_that("exp_zero_inflated works with custom offset", {
  input_vec <- c(-1, 0, 1)
  custom_offset <- 0.5
  expected_vec <- 10^(input_vec) - custom_offset
  expect_equal(exp_zero_inflated(input_vec, offset = custom_offset), expected_vec)
})

test_that("exp_zero_inflated works with custom base", {
  input_vec <- c(-1, 0, 1)
  custom_base <- 2
  expected_vec <- custom_base^(input_vec) - 0.1
  expect_equal(exp_zero_inflated(input_vec, base = custom_base), expected_vec)
  
  # Test with natural logarithm base
  original_vec_ln <- c(0, 1, 10)
  log_transformed_ln <- log_zero_inflated(original_vec_ln, base = exp(1))
  exp_transformed_ln <- exp_zero_inflated(log_transformed_ln, base = exp(1))
  expect_equal(exp_transformed_ln, original_vec_ln, tolerance = 1e-9)
})

test_that("exp_zero_inflated handles NA values", {
  input_vec_na <- c(-1, NA, 1)
  expected_vec_na <- 10^(input_vec_na) - 0.1 # 10^NA is NA, NA - 0.1 is NA
  expect_equal(exp_zero_inflated(input_vec_na), expected_vec_na)
})

test_that("exp_zero_inflated handles Inf and -Inf", {
  expect_equal(exp_zero_inflated(Inf), Inf)     # 10^Inf - 0.1 is Inf
  expect_equal(exp_zero_inflated(-Inf), -0.1) # 10^(-Inf) is 0, 0 - 0.1 is -0.1
})

test_that("exp_zero_inflated handles empty vector", {
  expect_equal(exp_zero_inflated(numeric(0)), numeric(0))
})

test_that("exp_zero_inflated throws error for non-numeric input", {
  expect_error(exp_zero_inflated("a"), "x must be numeric")
  expect_error(exp_zero_inflated(TRUE), "x must be numeric")
  expect_error(exp_zero_inflated(as.factor("b")), "x must be numeric")
  expect_error(exp_zero_inflated(list(1,2)), "x must be numeric")
})

test_that("log_zero_inflated and exp_zero_inflated are inverses with custom parameters", {
  original_vec <- c(0, 5, 50, 500)
  custom_offset <- 0.01
  custom_base <- exp(1)
  
  log_t <- log_zero_inflated(original_vec, offset = custom_offset, base = custom_base)
  exp_t <- exp_zero_inflated(log_t, offset = custom_offset, base = custom_base)
  expect_equal(exp_t, original_vec, tolerance = 1e-9)
  
  custom_offset_2 <- 1
  custom_base_2 <- 5
  log_t2 <- log_zero_inflated(original_vec, offset = custom_offset_2, base = custom_base_2)
  exp_t2 <- exp_zero_inflated(log_t2, offset = custom_offset_2, base = custom_base_2)
  expect_equal(exp_t2, original_vec, tolerance = 1e-9)
})