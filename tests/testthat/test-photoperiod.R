test_that("photoperiod works", {
  coordinates <- c(20, 20)
  dates <- c("2023-06-01", "2025-08-23")
  tz <- "Europe/Zurich"
  testcase <- photoperiod(coordinates, dates, tz)
  
  #correct dawn calculation
  expect_equal(testcase$dawn, 
               as.POSIXct(c("2023-06-01 05:35:47", "2025-08-23 05:59:38"),
                          tz = tz
                          )
               )
  #correct dusk calculation
  expect_equal(testcase$dusk, 
               as.POSIXct(c("2023-06-01 19:39:58", "2025-08-23 19:25:14"),
                          tz = tz
               )
  )
  #correct dates
  expect_equal(testcase$date, 
               lubridate::as_date(dates)
               )
  #correct timezones
  expect_equal(testcase$tz, 
               c(tz, tz)
               )  
  #provide latitude and longitude
  expect_equal(c(testcase$lat, testcase$lon),
               rep(coordinates, 2)
               )
  #provide solar angle
  expect_equal(testcase$solar.angle, c(-6,-6))
  #correct photoperiod calculation
  expect_equal(testcase$photoperiod, 
               difftime(testcase$dusk, testcase$dawn, units = "hours")
               )
  #correct total numbers of columns
  expect_equal(length(testcase), 8)
})


test_that("photoperiod throws errors", {
  coordinates <- c(20, 20)
  dates <- c("2023-06-01", "2025-08-23")
  tz <- "Europe/Zurich"

  #require correct time zone
  expect_error(photoperiod(coordinates, dates, tz = "no time zone"))
  #require correct format of coordinates
  expect_error(photoperiod(c(coordinates, coordinates), dates, tz = tz))
  expect_error(photoperiod(c(coordinates, 30), dates, tz = tz))
  expect_error(photoperiod(c(20, NA), dates, tz = tz))
  expect_error(photoperiod(c(20, NaN), dates, tz = tz))
  #require correct format of dates
  expect_error(
    suppressWarnings(
      photoperiod(coordinates, "no real date", tz = tz)
      )
  )
})

test_that("extract_photoperiod works", {
  coordinates <- c(20,20)
  expect_equal(extract_photoperiod(sample.data.environment, coordinates) |> 
                 nrow(),
               6)
  
})

test_that("add_photoperiod works", {
  coordinates <- c(20,20)
  new_names <- c("dusk", "dawn", "photoperiod", "photoperiod.state")
  added_photoperiod <- 
    add_photoperiod(sample.data.environment, coordinates)
  expect_true(all(new_names %in% names(added_photoperiod)
               ))
  expect_equal(nrow(added_photoperiod), nrow(sample.data.environment))
  expect_equal(length(added_photoperiod), length(sample.data.environment)+4)
  expect_true(lubridate::is.POSIXct(added_photoperiod$dawn))
  expect_true(lubridate::is.POSIXct(added_photoperiod$dusk))
  expect_true(lubridate::is.difftime(added_photoperiod$photoperiod))
})

test_that("gg_photoperiod works", {
  coordinates <- c(20,20)
  Plot <- sample.data.environment |> gg_day() |> gg_photoperiod(coordinates)
  Plot2 <- sample.data.environment |> gg_days() |> gg_photoperiod(coordinates)
  Plot3 <- sample.data.environment |> gg_doubleplot() |> gg_photoperiod(coordinates)
  Plot4 <- sample.data.environment |> add_photoperiod(coordinates)|> gg_day() |> gg_photoperiod()
  Plot5 <- sample.data.environment |> gg_doubleplot(type = "repeat") |> gg_photoperiod(coordinates)
  Plot6 <- sample.data.environment |> add_photoperiod(coordinates) |> gg_doubleplot(type = "repeat") |> gg_photoperiod()
  expect_snapshot(Plot$data)
  expect_snapshot(Plot2$data)
  expect_snapshot(Plot3$data)
  expect_snapshot(Plot4$data)
  expect_false(identical(Plot5$data, Plot6$data))
})

test_that("gg_photoperiod throws errors", {
  coordinates <- c(20,20)
  expect_error(sample.data.environment |> gg_day() |> gg_photoperiod())
})
