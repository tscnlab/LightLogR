test_that("add_Date_col adds Date column correctly with default settings", {
  test_data <- tibble::tibble(
    ID = 1,
    Datetime = lubridate::as_datetime(c("2023-01-01 10:00:00", "2023-01-01 22:00:00", "2023-01-02 08:00:00"))
  )
  result <- add_Date_col(test_data)
  expect_true("Date" %in% names(result))
  expect_s3_class(result$Date, "Date")
  expect_equal(result$Date, lubridate::as_date(c("2023-01-01", "2023-01-01", "2023-01-02")))
  expect_equal(dplyr::group_vars(result), character(0)) # No grouping by default
})

test_that("add_Date_col uses custom Date.colname and Datetime.colname", {
  test_data <- tibble::tibble(
    MyTimestamp = lubridate::as_datetime("2023-03-15 12:00:00")
  )
  result <- add_Date_col(test_data, Date.colname = MyDate, Datetime.colname = MyTimestamp)
  expect_true("MyDate" %in% names(result))
  expect_s3_class(result$MyDate, "Date")
  expect_equal(result$MyDate, lubridate::as_date("2023-03-15"))
})

test_that("add_Date_col with as.wday = TRUE creates weekday factor column", {
  test_data <- tibble::tibble(
    Datetime = lubridate::as_datetime(c("2023-01-02 10:00:00",  # Monday
                                        "2023-01-03 12:00:00",  # Tuesday
                                        "2023-01-08 14:00:00")) # Sunday
  )
  result <- add_Date_col(test_data, Date.colname = WeekDay, as.wday = TRUE)
  expect_true("WeekDay" %in% names(result))
  expect_s3_class(result$WeekDay, "factor")
  expected_wday <- factor(c("Mon", "Tue", "Sun"),
                          levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                          ordered = TRUE) # lubridate::wday with label=T returns ordered factor
  expect_equal(result$WeekDay, expected_wday)
})

test_that("add_Date_col with group.by = TRUE adds grouping", {
  test_data <- tibble::tibble(
    ID = c("A", "A", "B"),
    Datetime = lubridate::as_datetime(c("2023-01-01 10:00:00", "2023-01-02 10:00:00", "2023-01-01 12:00:00"))
  ) %>% dplyr::group_by(ID)
  
  result_date_grouped <- add_Date_col(test_data, group.by = TRUE)
  expect_equal(dplyr::group_vars(result_date_grouped), c("ID", "Date"))
  
  result_wday_grouped <- add_Date_col(test_data, as.wday = TRUE, group.by = TRUE, Date.colname = DayOfWeek)
  expect_equal(dplyr::group_vars(result_wday_grouped), c("ID", "DayOfWeek"))
})

test_that("add_Date_col handles existing groups correctly", {
  test_data <- tibble::tibble(
    Group = "G1",
    Datetime = lubridate::as_datetime("2023-01-01 10:00:00")
  ) %>% dplyr::group_by(Group)
  
  result_no_group_by <- add_Date_col(test_data) # group.by = FALSE
  expect_equal(dplyr::group_vars(result_no_group_by), "Group")
  expect_equal(as.character(result_no_group_by$Date), "2023-01-01")
  
  result_group_by <- add_Date_col(test_data, group.by = TRUE)
  expect_equal(dplyr::group_vars(result_group_by), c("Group", "Date"))
})

test_that("add_Date_col input validation errors", {
  expect_error(add_Date_col(list(a = 1)), "dataset is not a dataframe")
  test_df <- tibble::tibble(DT = lubridate::now())
  expect_error(add_Date_col(test_df, Datetime.colname = NonExistent), "Datetime.colname must be part of the dataset")
  test_df_bad_dt <- tibble::tibble(Datetime = "2023-01-01")
  expect_error(add_Date_col(test_df_bad_dt), "Datetime.colname must be a Datetime")
  expect_error(add_Date_col(test_df, Datetime.colname = DT, as.wday = "TRUE"), "as.wday has to be a logical")
  expect_error(add_Date_col(test_df, Datetime.colname = DT, group.by = "FALSE"), "group.by has to be a logical")
})

test_that("add_Date_col overwrites existing column with the same name", {
  test_data <- tibble::tibble(
    Datetime = lubridate::as_datetime("2023-01-01 10:00:00"),
    Date = "Old Value" # Existing column with default name
  )
  result <- add_Date_col(test_data) # Should overwrite 'Date'
  expect_s3_class(result$Date, "Date")
  expect_equal(result$Date, lubridate::as_date("2023-01-01"))
  
  test_data_wday <- tibble::tibble(
    Datetime = lubridate::as_datetime("2023-01-02 10:00:00"), # Monday
    MyWday = 123 # Existing column with custom name
  )
  result_wday <- add_Date_col(test_data_wday, Date.colname = MyWday, as.wday = TRUE)
  expect_s3_class(result_wday$MyWday, "factor")
  expect_equal(as.character(result_wday$MyWday), "Mon")
})

