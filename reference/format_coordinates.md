# Format coordinates as a readable string

Create a concise latitude/longitude label (e.g., \\48.5^\circ N,
9.1^\circ E\\) from a two element coordinate vector.

## Usage

``` r
format_coordinates(coordinates, digits = 1)
```

## Arguments

- coordinates:

  A numeric vector of length two with latitude as the first element and
  longitude as the second element.

- digits:

  Integerish scalar defining the number of decimal places used for
  rounding. Defaults to one decimal place.

## Value

A character scalar with the formatted coordinate string.

## Examples

``` r
# Coordinates for Tuebingen, Germany
format_coordinates(c(48.5216, 9.0576))
#> [1] "48.5°N, 9.1°E"
```
