test_that("Basic functionality works", {
  spd <- data.frame(wl = 360:830, values = 1)
  
  # Test auto-weighting
  expect_silent(
    res <- spectral_integration(spd, action.spectrum = "photopic", 
                                general.weight = "auto")
  )
  expect_equal(res, 72983.3, tolerance = 1e-4)
  
  # Test custom action spectrum
  custom_act <- data.frame(wavelength = 400:700, weight = 0.5)
  expect_silent(
    res <- spectral_integration(spd, wavelength.range = c(400,700),
                                action.spectrum = custom_act,
                                general.weight = 2)
  )
  expect_equal(res, 300*2*0.5, tolerance = 1)
})

test_that("Input validation works", {
  # Invalid spectrum
  expect_error(spectral_integration(data.frame(a=1)), "at least 2 columns")
  
  # Invalid action spectrum name
  expect_error(spectral_integration(data.frame(1:2,1:2), 
                                    action.spectrum = "invalid"),
               "Invalid built-in")
  
  # Auto-weight without built-in
  custom_act <- data.frame(wavelength=1:10, weight=1)
  expect_error(spectral_integration(data.frame(1:2,1:2), 
                                    action.spectrum = custom_act,
                                    general.weight = "auto"),
               "only works with built-in")
})

test_that("Column order independence", {
  # Reverse column order
  spd <- data.frame(spd=1:10, wavelength=380:389)
  expect_silent(spectral_integration(spd))
})

