#test for import_Statechanges
test_that("imports a wide and long dataset", {
  
  path <- system.file("extdata/",
                      package = "LightLogR")
  file.sleep <- "205_sleepdiary_all_20230904.csv"
  
  tz <- "Europe/Berlin"
  
  #wide data
  expect_snapshot({
  import_Statechanges(file.sleep, path,
  Datetime.format = "dmyHM",
  State.colnames = c("sleep", "offset"),
  State.encoding = c("sleep", "wake"),
  Id.colname = record_id,
  sep = ";",
  dec = ",",
  tz = tz,
  silent = TRUE)
  }
  )
  
  #long data
  expect_snapshot(
    {
      import_Statechanges(file.sleep, path,
                          Datetime.format = "dmyHM",
                          State.colnames = "comments",
                          Datetime.column = sleep,
                          Id.colname = record_id,
                          sep = ";",
                          dec = ",", structure = "long",
                          tz = tz, silent = TRUE)
    }
  )
}
)

test_that("expect snapshot needs certain info",{
  #no State.colnames
  expect_error({
    path <- system.file("extdata/",
                        package = "LightLogR")
    file.sleep <- "205_sleepdiary_all_20230904.csv"
    import_Statechanges(file.sleep, path)
    })
})
