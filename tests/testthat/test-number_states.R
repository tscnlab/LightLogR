test_that("number_states works", {
  dataset <- tibble::tibble(
   Id = c("A", "B", "A", "A", "B", "B", "A", "B", "B", "A", "A", "B"),
   state =
   c("day", "day", "day", "night", "night", "day", "day", "night",
   "night", "night", "day", "night")
   )
  expect_equal(number_states(dataset, state) |> length(), 3)
  expect_equal(number_states(dataset, state) |> _[3] |> names(), "state.count")
  expect_equal(number_states(dataset, state) |> nrow(), 12)
  expect_equal(
    dataset |> 
      dplyr::group_by(Id) |> 
      number_states(state) |> 
      _[12,3] |> 
      tibble::deframe() , 
    "night 2"
    )
  expect_equal(dataset |> number_states(state) |> _[12,3] |> tibble::deframe() , "night 3")
})

test_that("number_states throws errors", {
  expect_error(number_states())
  expect_error(number_states(sample.data.environment))
  expect_error(number_states(sample.data.environment, "state"))
  expect_error(number_states(sample.data.environment, state))
  expect_error(number_states(c("day", "night"), state))
})