# tests/testthat/test-spectral_reconstruction.R

test_that("Valid inputs produce correct outputs", {
  # Create 3-channel calibration matrix (R, G, B)
  calib <- matrix(
    c(0.1, 0.2, 0.3, 0.4, 0.5,
      0.6, 0.7, 0.8, 0.9, 1.0,
      1.1, 1.2, 1.3, 1.4, 1.5),
    ncol = 3,
    dimnames = list(400:404, c("R", "G", "B"))
  )
  
  # Test named vector input
  sensor_vec <- c(R = 1, G = 2, B = 3)
  expect_silent(
    result_vec <- spectral_reconstruction(sensor_vec, calib)
  )
  expected <- calib[,"R"]*1 + calib[,"G"]*2 + calib[,"B"]*3
  expect_equal(result_vec$irradiance, as.numeric(expected))
  
  # Test dataframe input with tidyselect
  df <- data.frame(R = 1, G = 2, B = 3, other = "x")
  expect_silent(
    result_df <- spectral_reconstruction(dplyr::select(df, R:B), calib)
  )
  expect_equal(result_df, result_vec)
  
  # Test wide format
  wide_result <- spectral_reconstruction(df, calib, format = "wide")
  expect_equal(nrow(wide_result), 1)
  expect_equal(colnames(wide_result), as.character(400:404))
  expect_equal(as.numeric(wide_result[1,]), as.numeric(expected))
})

test_that("Multi-row inputs return correct formats", {
  # 4 wavelengths x 2 sensors calibration
  calib <- matrix(
    1:8, ncol = 2,
    dimnames = list(400:403, c("S1", "S2"))
  )
  
  multi_df <- data.frame(S1 = 1:3, S2 = 4:6)
  
  # Long format (list of tibbles)
  long_result <- spectral_reconstruction(multi_df, calib)
  expect_type(long_result, "list")
  expect_length(long_result, 3)
  
  # Verify second spectrum calculation
  expected_2 <- calib[,"S1"]*2 + calib[,"S2"]*5
  expect_equal(long_result[[2]]$irradiance, as.numeric(expected_2))
  
  # Wide format (dataframe)
  wide_result <- spectral_reconstruction(multi_df, calib, "wide")
  expect_equal(nrow(wide_result), 3)
  expect_equal(
    as.numeric(wide_result[3,]),
    as.numeric(calib[,"S1"]*3 + calib[,"S2"]*6)
  )
})

test_that("Input validation works correctly", {
  # Proper 3x3 calibration matrix
  good_calib <- matrix(
    1:9, ncol = 3, 
    dimnames = list(400:402, c("R", "G", "B"))
  )
  
  # Test calibration matrix without column names
  bad_calib <- matrix(1:6, ncol = 2, dimnames = list(NULL, NULL))
  expect_error(
    spectral_reconstruction(c(R=1), bad_calib),
    "Sensor names mismatch with calibration columns",
    fixed = TRUE
  )
  
  # Test calibration matrix with wrong column names
  misnamed_calib <- matrix(1:6, ncol = 2, dimnames = list(NULL, c("X", "Y")))
  expect_error(
    spectral_reconstruction(c(R=1), misnamed_calib),
    "Sensor names mismatch with calibration columns",
    fixed = TRUE
  )
})


test_that("Edge cases are handled properly", {
  # Single wavelength calibration
  calib_single <- matrix(
    1:3, ncol = 3, nrow = 1,
    dimnames = list(400, c("R", "G", "B"))
  )
  expect_silent(
    spectral_reconstruction(c(R=1, G=2, B=3), calib_single)
  )
  
  # Zero values
  calib_zero <- matrix(
    0, ncol = 3, nrow = 3,
    dimnames = list(400:402, c("R", "G", "B"))
  )
  result_zero <- spectral_reconstruction(c(R=1, G=2, B=3), calib_zero)
  expect_equal(result_zero$irradiance, rep(0,3))
})
