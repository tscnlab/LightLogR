#' Sample groups from a grouped dataset
#'
#' This helper selects a subset of groups from a grouped dataset. Groups can be
#' drawn randomly, by ordering groups from the top or bottom according to a
#' summary expression, or by filtering with a custom condition. The function is
#' designed to work with datasets that were grouped using [dplyr::group_by()].
#'
#' @param dataset A grouped dataset. Expects a data frame grouped with
#'   [dplyr::group_by()].
#' @param n Number (or vector of positions) of groups to return. Defaults to 1.
#'   Ignored when `condition` is supplied and `n` is `NULL`.
#' @param sample Sampling strategy. Must be one of `"random"`, `"top"`, or
#'   `"bottom"`. When `condition` is provided, the `sample` value is ignored and
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
#' sample.data.environment |>
#'   dplyr::group_by(Id) |>
#'   sample_groups()
#'
#' sample.data.environment |>
#'   dplyr::group_by(Id) |>
#'   sample_groups(sample = "top", order.by = mean(MEDI))
#'
#' sample.data.environment |>
#'   dplyr::group_by(Id) |>
#'   sample_groups(condition = mean(MEDI, na.rm = TRUE) > 100)
sample_groups <- function(dataset,
                          n = 1,
                          sample = c("random", "top", "bottom"),
                          order.by = dplyr::cur_group_id(),
                          condition = NULL) {
  order.by <- rlang::enquo(order.by)
  condition <- rlang::enquo(condition)
  has_condition <- !rlang::quo_is_null(condition)

  stopifnot(
    "`dataset` must be a data frame!" = is.data.frame(dataset),
    "`dataset` must be grouped!" = dplyr::is_grouped_df(dataset),
    "`n` must be NULL or a positive integer vector!" =
      is.null(n) || (is.numeric(n) && all(n %% 1 == 0) && all(n > 0))
  )

  sample <- if (has_condition) "condition" else match.arg(sample)

  if (!is.null(n)) {
    n <- as.integer(n)
  }

  if (!has_condition && is.null(n)) {
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
    sample,
    "random" = group_summary |>
      dplyr::slice_sample(n = max(n)) |>
      dplyr::slice(n),
    "top" = group_summary |>
      dplyr::arrange(dplyr::desc(.order_value)) |>
      dplyr::slice(n),
    "bottom" = group_summary |>
      dplyr::arrange(.order_value) |>
      dplyr::slice(n),
    "condition" = {
      conditioned <- group_summary |>
        dplyr::filter(.condition_value)

      if (is.null(n)) {
        conditioned
      } else conditioned |>
        dplyr::slice(n)
    }
  )

  selected <- selected |>
    dplyr::select(dplyr::all_of(group_vars))

  dataset |>
    dplyr::semi_join(selected, by = group_vars)
}
