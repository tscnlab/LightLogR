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
  #correct total numbers of columns
  expect_equal(length(testcase), 7)
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
