# Create a simple datetime plot of light logger data, faceted by group

`gg_days()` will create a simple ggplot along the timeline. The result
can further be manipulated like any ggplot. This will be sensible to
refine styling or guides. Through the `x.axis.limits` arguments, the
plot can be much refined to align several groups of differing datetime
ranges. It uses the
[`Datetime_limits()`](https://tscnlab.github.io/LightLogR/reference/Datetime_limits.md)
function to calculate the limits of the x-axis. Another notable
functions that are used are
[`Datetime_breaks()`](https://tscnlab.github.io/LightLogR/reference/Datetime_breaks.md)
to calculate the breaks of the x-axis.

## Usage

``` r
gg_days(
  dataset,
  y.axis = MEDI,
  geom = "line",
  x.axis = Datetime,
  aes_col = NULL,
  aes_fill = NULL,
  group = NULL,
  scales = c("free_x", "free_y", "fixed", "free"),
  x.axis.breaks = Datetime_breaks,
  y.axis.breaks = c(-10^(5:0), 0, 10^(0:5)),
  y.scale = "symlog",
  y.scale.sc = FALSE,
  x.axis.label = "Local date/time",
  y.axis.label = "Melanopic EDI (lx)",
  x.axis.limits = Datetime_limits,
  x.axis.format = "%a %D",
  title = NULL,
  subtitle = NULL,
  interactive = FALSE,
  facetting = TRUE,
  jco_color = TRUE,
  ...
)
```

## Arguments

- dataset:

  A light logger dataset. Expects a `dataframe`. If not imported by
  [LightLogR](https://tscnlab.github.io/LightLogR/reference/LightLogR-package.md),
  take care to choose a sensible variable for the `x.axis.`.

- geom:

  What geom should be used for visualization? Expects a `character`

  - `"point"` for
    [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)

  - `"line"` for
    [`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)

  - `"ribbon"` for
    [`ggplot2::geom_ribbon()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html)

  - as the value is just input into the `geom_` function from ggplot2,
    other variants work as well, but are not extensively tested.

- x.axis, y.axis:

  column name that contains the datetime (x, defaults to `"Datetime"`
  which is automatically correct for data imported with
  [LightLogR](https://tscnlab.github.io/LightLogR/reference/LightLogR-package.md))
  and the dependent variable (y, defaults to `"MEDI"`, or melanopic EDI,
  which is a standard measure of stimulus strength for the nonvisual
  effects of light). Expects a `symbol`. Needs to be part of the
  `dataset`.

- aes_col, aes_fill:

  optional input that defines separate sets and colors or fills them.
  Expects anything that works with the layer data
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).

- group:

  Optional column name that defines separate sets. Useful for certain
  geoms like `boxplot`.Expects anything that works with the layer data
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html)

- scales:

  For
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html),
  should scales be `"fixed"`, `"free"` or `"free"` in one dimension
  (`"free_x"` is the default). Expects a `character`.

- x.axis.breaks:

  The (major) breaks of the x-axis. Defaults to
  [`Datetime_breaks()`](https://tscnlab.github.io/LightLogR/reference/Datetime_breaks.md).
  The function has several options for adjustment. The default setting
  place a major break every 12 hours, starting at 12:00 of the first
  day.

- y.axis.breaks:

  Where should breaks occur on the y.axis? Expects a `numeric vector`
  with all the breaks or a function that calculates them based on the
  limits. If you want to activate the default behaviour of ggplot2, you
  need to put in
  [`ggplot2::waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html).

- y.scale:

  How should the y-axis be scaled?

  - Defaults to `"symlog"`, which is a logarithmic scale that can also
    handle negative values.

  - `"log10"` would be a straight logarithmic scale, but cannot handle
    negative values.

  - `"identity"` does nothing (continuous scaling).

  - a transforming function, such as
    [`symlog_trans()`](https://tscnlab.github.io/LightLogR/reference/symlog_trans.md)
    or
    [`scales::identity_trans()`](https://scales.r-lib.org/reference/transform_identity.html),
    which allow for more control.

- y.scale.sc:

  `logical` for whether scientific notation shall be used. Defaults to
  `FALSE`.

- x.axis.label, y.axis.label:

  labels for the x- and y-axis. Expects a `character`.

- x.axis.limits:

  The limits of the x-axis. Defaults to
  [`Datetime_limits()`](https://tscnlab.github.io/LightLogR/reference/Datetime_limits.md).
  Can and should be adjusted to shift the x-axis to align different
  groups of data.

- x.axis.format:

  The format of the x-axis labels. Defaults to `"%a %D"`, which is the
  weekday and date. See
  [`base::strptime()`](https://rdrr.io/r/base/strptime.html) for more
  options.

- title:

  Plot title. Expects a `character`.

- subtitle:

  Plot subtitle. Expects a `character`.

- interactive:

  Should the plot be interactive? Expects a `logical`. Defaults to
  `FALSE`.

- facetting:

  Should an automated facet by grouping be applied? Default is `TRUE`.

- jco_color:

  Should the
  [`ggsci::scale_color_jco()`](https://nanx.me/ggsci/reference/scale_jco.html)
  color palette be used? Defaults to `TRUE`.

- ...:

  Other options that get passed to the main geom function. Can be used
  to adjust to adjust size, linewidth, or linetype.

## Value

A ggplot object

## Details

The default scaling of the y-axis is a `symlog` scale, which is a
logarithmic scale that only starts scaling after a given threshold
(default = 0). This enables values of 0 in the plot, which are common in
light logger data, and even enables negative values, which might be
sensible for non-light data. See
[`symlog_trans()`](https://tscnlab.github.io/LightLogR/reference/symlog_trans.md)
for details on tweaking this scale. The scale can also be changed to a
normal or logarithmic scale - see the y.scale argument for more.

## Examples

``` r
dataset <-
sample.data.environment %>%
aggregate_Datetime(unit = "5 mins")

dataset %>% gg_days()

#restrict the x-axis to 3 days
dataset %>%
gg_days(
x.axis.limits = \(x) Datetime_limits(x, length = lubridate::ddays(3))
)
#> Warning: Removed 1728 rows containing missing values or values outside the scale range
#> (`geom_line()`).
```
