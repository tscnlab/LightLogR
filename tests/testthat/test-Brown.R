test_that("Brown_rec works as expected", {
  expect_equal(Brown_rec("day"), 250)
  expect_equal(Brown_rec("evening"), 10)
  expect_equal(Brown_rec("night", Brown.night.th = 2), 2)
  expect_equal(Brown_rec("nighttime", Brown.night = "nighttime"), 1)
  expect_equal(Brown_rec("night"), 1)
  expect_equal(Brown_rec(c("day", "evening", "night")), c(250, 10, 1))
  expect_equal(Brown_rec(c("day", "evening", "night", "nighttime")), c(250, 10, 1, NA))
}
)

test_that("Brown_rec throws errors as expected", {
  expect_error(Brown_rec("day", Brown.day.th = "250"), "Thresholds need to be numeric")
  expect_error(Brown_rec("day", Brown.day = c("day", "evening", "night")), "States need to be scalars")
}
)

test_that("Brown_check works as expected", {
  expect_equal(Brown_check(100, "day"), FALSE)
  expect_equal(Brown_check(100, "day", Brown.day.th = 100), TRUE)
  expect_equal(Brown_check(100, "evening"), FALSE)
  expect_equal(Brown_check(100, "evening", Brown.evening.th = 100), TRUE)
  expect_equal(Brown_check(100, "night"), FALSE)
  expect_equal(Brown_check(100, "night", Brown.night.th = 100), TRUE)
  expect_equal(Brown_check(100, "nighttime", Brown.night = "nighttime"), FALSE)
  expect_equal(Brown_check(100, "nighttime"), NA)
  expect_equal(Brown_check(100, "nighttime", Brown.night = "nighttime", Brown.night.th = 100), TRUE)
}
)

test_that("Brown_check throws errors as expected", {
  expect_error(Brown_check(100, "day", Brown.day.th = "250"), "Thresholds need to be numeric")
  expect_error(Brown_check(100, "day", Brown.day = c("day", "evening", "night")), "States need to be scalars")
  expect_error(Brown_check(100, c("day", "evening", "night")), "state needs to be a character vector with the same length as value")
}
)

test_that("Brown2reference works as expected", {
  testdata <- tibble::tibble(MEDI = c(100, 10, 1, 300),
                    State.Brown = c("day", "evening", "night", "day"))
  expect_equal(Brown2reference(testdata) %>% 
                 dplyr::pull(Reference), c(250, 10, 1, 250))
  expect_equal(Brown2reference(testdata) %>% 
                 dplyr::pull(Reference.check), c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(Brown2reference(testdata, Brown.day.th = 100) %>% 
                 dplyr::pull(Reference), c(100, 10, 1, 100))
  expect_equal(Brown2reference(testdata, Brown.day.th = 100) %>% 
                 dplyr::pull(Reference.check), c(TRUE, TRUE, TRUE, TRUE))
}
)

test_that("Brown2reference throws errors as expected", {
  testdata <- tibble::tibble(MEDI = c(100, 10, 1, 300),
                             State.Brown = c("day", "evening", "night", "day"))
  expect_error(Brown2reference(
    testdata, Brown.day.th = "250"), 
    "Thresholds need to be numeric")
  expect_error(Brown2reference(
    testdata, 
    Brown.day = c("day", "evening", "night")), 
    "States need to be scalars")
}
)

#----
test_that("Brown_cut basic functionality", {
  data_test <- tibble::tibble(
    Id = "A",
    MEDI = c(0.5, 5, 100, 300, NA_real_)
  )
  
  # Default cuts and labels
  result_default <- Brown_cut(data_test)
  expect_s3_class(result_default, "tbl_df")
  expect_true("state" %in% names(result_default)) # Default New.state.colname
  # Default labels: c("\U{2264}1lx", "\U{2264}10lx", NA, "\U{2265}250lx")
  # Cuts: -Inf, 1, 10, 250, Inf
  # 0.5 -> "<=1lx"
  # 5   -> "<=10lx"
  # 100 -> NA (between 10 and 250, label is NA)
  # 300 -> ">=250lx"
  # NA  -> NA
  expected_states_default <- factor(
    c("\U{2264}1lx", "\U{2264}10lx", NA, "\U{2265}250lx", NA),
    levels = c("\U{2264}1lx", "\U{2264}10lx", NA, "\U{2265}250lx"), # cut adds NA to levels if present
    exclude = NULL # to ensure NA is a level
  )
  # Manually correct NA level if cut doesn't add it as expected.
  # `cut` by default does not include NA in levels unless `include.lowest=TRUE` and NA is produced
  # from a value falling into a bin labeled NA. The NA input value becomes NA factor level.
  # The factor levels will be the unique non-NA labels provided.
  # Let's check the actual factor levels `cut` produces.
  # For `vector_labels = c("L1", "L2", NA, "L4")`, `cut` creates levels c("L1", "L2", "L4").
  # The values that fall into the bin labeled NA become `NA_character_` which then becomes `NA` factor level.
  # So, comparing directly to character is safer if NA levels are tricky.
  expect_equal(as.character(result_default$state),
               c("\U{2264}1lx", "\U{2264}10lx", NA, "\U{2265}250lx", NA))
  
  
  # Custom column names
  result_custom_cols <- Brown_cut(data_test, MEDI.colname = MEDI, New.state.colname = LightCategory)
  expect_true("LightCategory" %in% names(result_custom_cols))
  expect_equal(as.character(result_custom_cols$LightCategory),
               c("\U{2264}1lx", "\U{2264}10lx", NA, "\U{2265}250lx", NA))
  
})

test_that("Brown_cut with custom cuts and labels", {
  data_test <- tibble::tibble(Value = c(5, 15, 25, 35))
  custom_cuts <- c(0, 10, 20, 30, 40)
  custom_labels <- c("Low", "Mid-Low", "Mid-High", "High")
  
  result <- Brown_cut(data_test, MEDI.colname = Value, New.state.colname = Category,
                      vector_cuts = custom_cuts, vector_labels = custom_labels)
  
  expect_true("Category" %in% names(result))
  expected_categories <- factor(c("Low", "Mid-Low", "Mid-High", "High"), levels = custom_labels)
  expect_equal(result$Category, expected_categories)
  
  # Edge cases for cuts (using include.lowest = TRUE, right = TRUE by default for cut)
  # Value = 10 should be "Low"
  # Value = 20 should be "Mid-Low"
  data_edge <- tibble::tibble(Value = c(10, 20, 30))
  result_edge <- Brown_cut(data_edge, MEDI.colname = Value, New.state.colname = Cat,
                           vector_cuts = custom_cuts, vector_labels = custom_labels)
  # cut default: (a,b] interval. include.lowest=F means first interval is (breaks[1], breaks[2]].
  # If include.lowest=T, it becomes [breaks[1], breaks[2]].
  # Default behavior of `cut` is `right = TRUE`, so intervals are (lo, hi].
  # First interval: (0, 10] -> "Low"
  # Second interval: (10, 20] -> "Mid-Low"
  # Third interval: (20, 30] -> "Mid-High"
  expect_equal(as.character(result_edge$Cat), c("Low", "Mid-Low", "Mid-High"))
})

test_that("Brown_cut overwrite behavior", {
  data_test_ow <- tibble::tibble(MEDI = c(5, 15), state = c("old1", "old2"))
  
  # overwrite = FALSE (default in function signature, but test sets to TRUE)
  # Test with explicit overwrite = FALSE and existing column
  expect_error(Brown_cut(data_test_ow, overwrite = FALSE))
  # overwrite = TRUE (default in function signature used in example)
  expect_warning(result_ow_true <- Brown_cut(data_test_ow, overwrite = TRUE))
  expect_true("state" %in% names(result_ow_true))
  # Default cuts: MEDI=5 -> "<=10lx", MEDI=15 -> NA
  expect_equal(as.character(result_ow_true$state), c("\U{2264}10lx", NA))
  
  # overwrite = TRUE, but column does not exist (no warning)
  data_no_state <- tibble::tibble(MEDI = c(5,15))
  expect_warning(result_no_warn <- Brown_cut(data_no_state, overwrite = TRUE), NA) # NA means no warning
  expect_equal(as.character(result_no_warn$state), c("\U{2264}10lx", NA))
})

test_that("Brown_cut input validation", {
  df <- tibble::tibble(Illuminance = c(10, 100))
  
  expect_error(Brown_cut(list()), "dataset is not a dataframe")
  expect_error(Brown_cut(df, MEDI.colname = BadName), "MEDI.colname must be part of the dataset")
  
  df_char_medi <- tibble::tibble(MEDI = c("low", "high"))
  expect_error(Brown_cut(df_char_medi), "MEDI.colname must be a numeric column")
  
  expect_error(Brown_cut(df, vector_cuts = "not numeric"), "vector_cuts need to be numeric")
  expect_error(Brown_cut(df, vector_labels = c("a","b"), vector_cuts = c(1,2)), # labels not shorter by 1
               "vector_labels need to be shorter by exactly one compared to vector_cuts")
  expect_error(Brown_cut(df, vector_labels = c("a"), vector_cuts = c(1,2,3,4)), # labels not shorter by 1
               "vector_labels need to be shorter by exactly one compared to vector_cuts")
  
  expect_error(Brown_cut(df, Illuminance, overwrite = "TRUE"), "overwrite must be a logical")
})

test_that("Brown_cut works with grouped data", {
  data_grouped <- tibble::tibble(
    Group = rep(c("X", "Y"), each = 2),
    Light = c(0.5, 50, 5, 300)
  ) %>% dplyr::group_by(Group)
  
  result <- Brown_cut(data_grouped, MEDI.colname = Light, New.state.colname = LightStates)
  expect_s3_class(result, "grouped_df")
  expect_equal(dplyr::group_vars(result), "Group")
  expect_true("LightStates" %in% names(result))
  
  expected_states_grp <- c("\U{2264}1lx", NA, "\U{2264}10lx", "\U{2265}250lx")
  expect_equal(as.character(result$LightStates), expected_states_grp)
})

