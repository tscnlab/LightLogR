test_that("pulse calculations preserve the caller's dplyr option", {
  original <- options("dplyr.summarise.inform")
  on.exit(options(original), add = TRUE)

  for (value in list(TRUE, FALSE, NULL)) {
    options(dplyr.summarise.inform = value)
    expect_silent(
      pulses_above_threshold(
        c(10, 20, 30, 0, 0, 50, 60),
        lubridate::dminutes(1:7),
        threshold = 10,
        min.length = "1 min",
        max.interrupt = "2 mins",
        prop.interrupt = 0,
        as.df = TRUE
      )
    )
    expect_identical(getOption("dplyr.summarise.inform"), value)
  }
})
