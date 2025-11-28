# Add photoperiods to gg_day() or gg_days() plots

`gg_photoperiod()` is a helper function to add photoperiod information
to plots generated with
[`gg_day()`](https://tscnlab.github.io/LightLogR/reference/gg_day.md) or
[`gg_days()`](https://tscnlab.github.io/LightLogR/reference/gg_days.md).
The function can either draw on the `dawn` and `dusk` columns of the
dataset or use the `coordinates` and `solarDep` arguments to calculate
the photoperiods.

## Usage

``` r
gg_photoperiod(
  ggplot_obj,
  coordinates = NULL,
  ymin = -Inf,
  ymax = Inf,
  alpha = 0.2,
  solarDep = 6,
  on.top = FALSE,
  Datetime.colname = Datetime,
  by.group = c(TRUE, FALSE),
  ...
)
```

## Arguments

- ggplot_obj:

  A `ggplot` object generated with
  [`gg_day()`](https://tscnlab.github.io/LightLogR/reference/gg_day.md)
  or
  [`gg_days()`](https://tscnlab.github.io/LightLogR/reference/gg_days.md)
  (or
  [`gg_doubleplot()`](https://tscnlab.github.io/LightLogR/reference/gg_doubleplot.md).

- coordinates:

  A two element numeric vector representing the latitude and longitude
  of the location. If `NULL`, the default, the function will look for
  the `dawn` and `dusk` columns in the dataset. If those are not
  present, (and in the `POSIXct` format), the function will stop with an
  error. Further, if `NULL`, the `solarDep` argument will be ignored.

- ymin, ymax:

  customize the height of the photoperiod rectangle. By default it will
  cover the whole vertical range (-Inf, Inf), but can be set to any
  value. If it is important to set the height conditionally, using
  [`gg_states()`](https://tscnlab.github.io/LightLogR/reference/gg_states.md)
  is recommended.

- alpha:

  A numerical value between 0 and 1 representing the transparency of the
  photoperiods. Default is 0.2.

- solarDep:

  A numerical value representing the solar depression angle between 90
  and -90. This means a value of 6 equals **-6** degrees above the
  horizon. Default is 6, equalling `Civil dawn/dusk`. Other common
  values are 12 degrees for `Nautical dawn/dusk`, 18 degrees for
  `Astronomical dawn/dusk`, and 0 degrees for `Sunrise/Sunset`. Note
  that the output columns will always be named `dawn` and `dusk`,
  regardless of the `solarDep` value.

- on.top:

  Logical scalar. If `TRUE`, the photoperiods will be plotted on top of
  the existing plot. If `FALSE`, the photoperiods will be plotted
  underneath the existing plot. Default is `FALSE`.

- Datetime.colname:

  Column name in the underlying dataset that contains the datetime.
  Defaults to `Datetime`. Is used to calculate photoperiod (if missing),
  and for grouping (only `POSIXct` axes).

- by.group:

  Logical that indicates whether the photoperiod to display is
  calculated within the groups of the dataset. By default this is`TRUE`
  for a `POSIXct` axis
  ([`gg_days()`](https://tscnlab.github.io/LightLogR/reference/gg_days.md))
  and `FALSE` for a `hms` axis
  ([`gg_day()`](https://tscnlab.github.io/LightLogR/reference/gg_day.md)).
  If provided as a length-2 vector, the first logical will be used for
  `POSIXct` and the second for `hms`. If a scalar is provided, it will
  be used for both conditions.

- ...:

  Additional arguments given to the
  [`ggplot2::geom_rect()`](https://ggplot2.tidyverse.org/reference/geom_tile.html)
  used to construct the photoperiod shading. Can be used to change the
  fill color or other aesthetic properties.

## Value

a modified `ggplot` object with the photoperiods added.

## Details

If used in combination with
[`gg_doubleplot()`](https://tscnlab.github.io/LightLogR/reference/gg_doubleplot.md),
with that function in the `type = "repeat"` setting (either manually
set, or because there is only one day of data per group present),
photoperiods need to be added separately through
[`add_photoperiod()`](https://tscnlab.github.io/LightLogR/reference/photoperiod.md),
or the second photoperiod in each panel will be off by one day. See the
examples for more information.

In general, if the photoperiod setup is more complex, it makes sense to
add it prior to plotting and make sure the photoperiods are correct.

## See also

Other photoperiod:
[`photoperiod()`](https://tscnlab.github.io/LightLogR/reference/photoperiod.md)

## Examples

``` r
coordinates <- c(48.521637, 9.057645)
#adding photoperiods to a ggplot
sample.data.environment |>
  gg_days() |>
  gg_photoperiod(coordinates)


#adding photoperiods prior to plotting
sample.data.environment |>
  add_photoperiod(coordinates, solarDep = 0) |>
  gg_days() |>
  gg_photoperiod()


#more examples that are not executed for computation time:
# \donttest{
#plotting photoperiods automatically works for both gg_day() and gg_days()
sample.data.environment |>
  gg_day() |>
  gg_photoperiod(coordinates)


#plotting for gg_doubleplot mostly works fine
sample.data.environment |>
  filter_Date(length = "2 days") |>
  gg_doubleplot() |>
  gg_photoperiod(coordinates)


#however, in cases where only one day of data per group is available, or the
#type = "repeat" setting is used, the photoperiods need to be added
#separately. Otherwise the second day will be off by one day in each panel.
#The visual difference is subtle, and might not be visible at all, as
#photoperiod only every changes by few minutes per day.

#WRONG
sample.data.environment |>
  filter_Date(length = "1 days") |>
  gg_doubleplot() |>
  gg_photoperiod(coordinates)


#CORRECT
sample.data.environment |>
  filter_Date(length = "1 days") |>
  add_photoperiod(coordinates) |>
  gg_doubleplot() |>
  gg_photoperiod()

  # }
```
