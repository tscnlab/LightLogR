#' Sample groups from a grouped dataset
#'
#' This helper selects a subset of groups from a grouped dataset. Groups can be
#' drawn randomly, by ordering groups from the top or bottom according to a
#' summary expression, or by filtering with a custom condition. The function is
#' designed to work with datasets that were grouped using [dplyr::group_by()].
#'
#' @param dataset A grouped dataset. Expects a data frame grouped with
#'   [dplyr::group_by()].
#' @param n Number of groups to return. Defaults to 1. Ignored when `condition`
#'   is supplied and `n` is `NULL`.
#' @param sample Sampling strategy. Must be one of `"random"`, `"top"` (the
#'   default), or `"bottom"`. Alternatively, a numeric vector can be provided to
#'   select group positions (using bottom ordering); when numeric, `n` is
#'   ignored. When `condition` is provided, the `sample` value is ignored and
#'   conditional filtering is applied instead.
#' @param order.by Expression used to order groups when `sample` is set to
#'   `"top"` or `"bottom"`. Evaluated in a one-row summary for each group.
#'   Defaults to [dplyr::cur_group_id()], i.e., the group number.
#' @param condition Logical expression used to filter the summarised groups.
#'   Evaluated in a one-row summary for each group, which includes an
#'   `.order_value` column derived from `order.by`.
#'
#' @return A grouped tibble containing only the sampled groups.
#' @export
#'
#' @examples
#' #gives one last group (highest group id)
#' sample.data.environment |>
#'   sample_groups() |>
#'   dplyr::group_keys()
#'
#' #gives one random group (highest group id)
#' sample.data.environment |>
#'   sample_groups(sample = "random") |>
#'   dplyr::group_keys()
#'
#' #gives the group with the highest average melanopic EDI
#' sample.data.environment |>
#'   sample_groups(order.by = mean(MEDI)) |>
#'   dplyr::group_keys()
#'
#' #gives the group with the lowest average melanopic EDI
#' sample.data.environment |>
#'   sample_groups(sample = "bottom", order.by = mean(MEDI)) |>
#'   dplyr::group_keys()
#'
#' # give only groups that have a median melanopic EDI > 1000 lx
#' sample.data.environment |>
#'   sample_groups(condition = median(MEDI, na.rm = TRUE) > 1000) |>
#'   dplyr::group_keys()
#'
#' # return only days with time above 250 lx mel EDI > 7 hours
#' sample.data.environment |>
#'   add_Date_col(group.by = TRUE) |>
#'   sample_groups(order.by = duration_above_threshold(MEDI, Datetime, threshold = 250),
#'                 condition = .order_value > 7*60*60) |>
#'   dplyr::group_keys()
#'   
#' # return the 5 days with the highest time above 250 lx mel EDI
#' sample.data.environment |>
#'   add_Date_col(group.by = TRUE) |>
#'   sample_groups(
#'     n = 5,
#'     order.by = duration_above_threshold(MEDI, Datetime, threshold = 250),
#'     ) |>
#'   dplyr::group_keys()
#'
#' # gives the first group
#' sample.data.environment |>
#'   sample_groups(sample = 1) |>
#'   dplyr::group_keys()
#'
#' # gives the second group
#' sample.data.environment |>
#'   sample_groups(sample = 2) |>
#'   dplyr::group_keys()
#'   
sample_groups <- function(dataset,
                          n = 1,
                          sample = c("top", "bottom", "random"),
                          order.by = dplyr::cur_group_id(),
                          condition = NULL) {
  order.by <- rlang::enquo(order.by)
  condition <- rlang::enquo(condition)
  has_condition <- !rlang::quo_is_null(condition)

  stopifnot(
    "`dataset` must be a data frame!" = is.data.frame(dataset),
    "`dataset` must be grouped!" = dplyr::is_grouped_df(dataset),
    "`n` must be NULL or a single positive integer!" =
      is.null(n) || (is.numeric(n) && length(n) == 1 && n %% 1 == 0 && n > 0)
  )

  sample_numeric <- is.numeric(sample)
  if (sample_numeric) {
    stopifnot(
      "Numeric `sample` must contain positive integers!" =
        all(sample %% 1 == 0 & sample > 0)
    )
    numeric_sample <- as.integer(sample)
  } else {
    sample <- match.arg(sample)
    numeric_sample <- NULL
  }

  sample_choice <- if (has_condition) "condition" else if (sample_numeric) "numeric" else sample

  if (!is.null(n)) {
    n <- as.integer(n)
  }

  if (!has_condition && !sample_numeric && is.null(n)) {
    stop("`n` must be supplied unless a `condition` is provided.")
  }

  group_vars <- dplyr::group_vars(dataset)

  group_summary <-
    if (!has_condition) {
      dataset |>
        dplyr::summarise(
          .order_value = !!order.by,
          .groups = "drop"
        )
    } else {
      dataset |>
        dplyr::summarise(
          .order_value = !!order.by,
          .condition_value = !!condition,
          .groups = "drop"
        )
    }

  selected <- switch(
    sample_choice,
    "random" = group_summary |>
      dplyr::slice_sample(n = n),
    "top" = group_summary |> 
      dplyr::arrange(dplyr::desc(.order_value)) |> 
      dplyr::slice_head(n = n),
    "bottom" = group_summary |>
      dplyr::arrange(dplyr::desc(.order_value)) |> 
      dplyr::slice_tail(n = n),
    "numeric" = group_summary |>
      dplyr::arrange(.order_value) |> 
      dplyr::slice(numeric_sample),
    "condition" = group_summary |>
      dplyr::arrange(dplyr::desc(.order_value)) |> 
        dplyr::filter(.condition_value)
  )

  selected <- selected |>
    dplyr::select(dplyr::all_of(group_vars))

  dataset |>
    dplyr::semi_join(selected, by = group_vars)
}
