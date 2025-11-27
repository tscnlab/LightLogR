#' Format coordinates as a readable string
#'
#' Create a concise latitude/longitude label (e.g., "48.5°N, 9.1°E") from a
#' two element coordinate vector.
#'
#' @param coordinates A numeric vector of length two with latitude as the first
#'   element and longitude as the second element.
#' @param digits Integerish scalar defining the number of decimal places used
#'   for rounding. Defaults to one decimal place.
#'
#' @return A character scalar with the formatted coordinate string.
#' @export
#'
#' @examples
#' # Coordinates for Tübingen, Germany
#' format_coordinates(c(48.5216, 9.0576))
#'
format_coordinates <- function(coordinates, digits = 1) {

  stopifnot(
    "`coordinates` must be numeric" = is.numeric(coordinates),
    "`coordinates` must have two elements" = length(coordinates) == 2,
    "`coordinates` must not contain NA or NaN" = !anyNA(coordinates),
    "`digits` must be integerish and non-negative" =
      rlang::is_integerish(digits, n = 1, finite = TRUE) && digits >= 0
  )

  digits <- digits |> as.integer()

  lat_string <-
    paste0(
      coordinates[1] |> round(digits) |> abs(),
      "°",
      dplyr::if_else(coordinates[1] >= 0, "N", "S")
    )

  lon_string <-
    paste0(
      coordinates[2] |> round(digits) |> abs(),
      "°",
      dplyr::if_else(coordinates[2] >= 0, "E", "W")
    )

  paste(lat_string, lon_string, sep = ", ")
}

