test_that("data2reference works as expected", {
  #create a small test dataset
  testdata <- 
    tibble::tibble(
      Id = rep(c("Participant1", "Participant2"), each = 5),
      MEDI = 1:10,
      Datetime= 
        rep(
          seq(
            from = as.POSIXct("2023-08-15 6:00:00"), 
            by = "hour", 
            length.out = 5
            ),
          2)
    ) %>% dplyr::group_by(Id)
  
  expect_equal(
    testdata %>% data2reference() %>% dplyr::pull(Reference), rep(NA_real_, 10))
  
  expect_equal(
    testdata %>% data2reference(length.restriction.seconds = 60*60*24) %>% 
      dplyr::pull(Reference), 
    1:10)
  
  expect_equal(
    testdata %>% 
      data2reference(
        length.restriction.seconds = 60*60*24,
        across.id = TRUE,
        filter.expression.reference = Id == "Participant1"
        ) %>% 
      dplyr::pull(Reference), 
               rep(1:5,2))  
  })
