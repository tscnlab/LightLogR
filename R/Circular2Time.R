#' Convert circular time columns to hms
#'
#' @param dataset A data.frame with `circular` columns representing time of day.
#' @param cols The column names to convert. Expects a `symbol`. The default will
#'   convert all `circular` columns. If uncertain whether columns exist in the
#'   dataset, use [dplyr::any_of()].
#' @param silent Logical on whether no message shall be shown if input and
#'   output are identical. Defaults to `FALSE` (i.e., a message is shown).
#'
#' @returns The input dataset with converted circular columns as time (hms)
#'   columns. With the default settings, if no circular column exists, input and
#'   output will be identical.
#' @export
#'
#' @examples
#' times <- lubridate::as_datetime("2023-01-01 10:00:00") + lubridate::hours(0:2)
#' times
#' circular_times <- Datetime2Time(tibble::tibble(Timestamp = times), circular = TRUE)
#' circular_times
#' Circular2Time(circular_times)
#' 
#' #if times are not circular, then an averaging can be problematic across midnight:
#' selected_times <- 
#' sample.data.environment |> 
#' sample_groups() |> 
#' dplyr::slice(c(40:43, 51838:51840))
#' selected_times
#' 
#' #a simple averaging will lead to a nonsensical value, e.g. if this should 
#' #calculate average sleep timing: ~10:00 in the morning
#' selected_times |> summarize_numeric()
#' 
#' #by converting it to a circular beforehand, averaging works as expected: 
#' #~3 minutes after midnight
#' selected_times |> 
#' summarize_numeric(Datetime2Time.circular = TRUE) |> 
#' Circular2Time()

Circular2Time <- function(dataset,
                          cols = dplyr::where(circular::is.circular),
                          silent = FALSE) {
  stopifnot("dataset needs to be a data.frame" = is.data.frame(dataset),
            "silent needs to be a logical" = is.logical(silent))

  data <- dataset |>
    dplyr::mutate(
      dplyr::across({{ cols }}, circular_to_hms)
    )

  if(identical(data, dataset) & !silent){
    message("No columns were affected")
  }
  data
}

circular_to_hms <- function(x) {
  radians <- as.numeric(circular::conversion.circular(x, units = "radians")) %% (2 * pi)
  seconds <- radians / (2 * pi) * 24 * 3600
  hms::hms(seconds)
}
