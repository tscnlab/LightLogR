#' Aggregate dates to a single day
#'
#' Condenses a `dataset` by aggregating the data to a single day per group, with
#' a resolution of choice `unit`. [aggregate_Date()] is opinionated in the sense
#' that it sets default handlers for each data type of `numeric`, `character`,
#' `logical`, and `factor`. These can be overwritten by the user. Columns that
#' do not fall into one of these categories need to be handled individually by
#' the user (`...` argument) or will be removed during aggregation. If no unit
#' is specified the data will simply be aggregated to the most common interval
#' (`dominant.epoch`) in every group. [aggregate_Date()] is especially useful
#' for summary plots that show an average day.
#'
#' @inheritParams aggregate_Datetime
#' @param unit Unit of binning. See [lubridate::round_date()] for examples. The
#'   default is `"none"`, which will not aggregate the data at all, but is only
#'   recommended for regular data, as the condensation across different days
#'   will be performed by time. Another option is `"dominant.epoch"`, which
#'   means everything will be aggregated to the most common interval. This is
#'   especially useful for slightly irregular data, but can be computationally
#'   expensive.
#' @param date.handler A function that calculates the aggregated day for each
#'   group. By default, this is set to `median`.

#' @return A `tibble` with aggregated `Datetime` data, at maximum one day per
#'   group. If the handler arguments capture all column types, the number of
#'   columns will be the same as in the input `dataset`.
#'
#' @details [aggregate_Date()] splits the `Datetime` column into a `Date.data`
#'   and a `Time.data` column. It will create subgroups for each `Time.data`
#'   present in a group and aggregate each group into a single day, then remove
#'   the sub grouping.
#'
#'   Use the `...` to create summary statistics for each group, e.g. maximum or
#'   minimum values for each time point group.
#'
#'   Performing [aggregate_Datetime()] with any `unit` and then
#'   [aggregate_Date()] with a `unit` of `"none"` is equivalent to just using
#'   [aggregate_Date()] with that `unit` directly (provided the other arguments
#'   are set the same between the functions). Disentangling the two functions
#'   can be useful to split the computational cost for very small instances of
#'   `unit` in large datasets. It can also be useful to apply different handlers
#'   when aggregating data to the desired `unit` of time, before further
#'   aggregation to a single day, as these handlers as well as `...` are used
#'   twice if the `unit` is not set to `"none"`.
#'
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' #gg_days without aggregation
#' sample.data.environment %>%
#'  gg_days()
#'
#' #with daily aggregation
#' sample.data.environment %>%
#'  aggregate_Date() %>%
#'  gg_days()
#'
#' #with daily aggregation and a different time aggregation
#' sample.data.environment %>%
#'  aggregate_Date(unit = "15 mins", type = "floor") %>%
#'  gg_days()
#'
#' #adding further summary statistics about the range of MEDI
#'  sample.data.environment %>%
#'  aggregate_Date(unit = "15 mins", type = "floor",
#'                 MEDI_max = max(MEDI),
#'                 MEDI_min = min(MEDI)) %>%
#'  gg_days() +
#'  geom_ribbon(aes(ymin = MEDI_min, ymax = MEDI_max), alpha = 0.5)

aggregate_Date <- function(dataset,
                           Datetime.colname = Datetime,
                           unit = "none",
                           type = c("round", "floor", "ceiling"),
                           date.handler = stats::median,
                           numeric.handler = 
                                 mean,
                           character.handler = 
                                 \(x) names(which.max(table(x, useNA = "ifany"))),
                               logical.handler = 
                                 \(x) mean(x) >= 0.5,
                               factor.handler = 
                                 \(x) factor(names(which.max(table(x, useNA = "ifany")))),
                               ...) {
  
  # Initial Checks ----------------------------------------------------------
  
  # Match input arguments
  type <- match.arg(type)
  
  Datetime.colname.str <- colname.defused({{ Datetime.colname }})
  Datetime.colname.defused <- colname.defused({{ Datetime.colname }}, as_string = FALSE)
  #most checks will be made by aggregate_Datetime later
  stopifnot(
    "date.handler must be a function" = is.function(date.handler)
  )
  
  # Function ----------------------------------------------------------
  #aggregate the datetime
    dataset <- 
      dataset  %>% 
      aggregate_Datetime(Datetime.colname = !!Datetime.colname.defused,
                         unit = unit,
                         type = type,
                         numeric.handler = numeric.handler,
                         character.handler = character.handler,
                         logical.handler = logical.handler,
                         factor.handler = factor.handler,
                         ...
                         )

  #create a date and a time column

  dataset <- 
    dataset %>% 
    create_Timedata(Datetime.colname = !!Datetime.colname.defused) %>% 
    dplyr::mutate(Date.data = lubridate::date(!!Datetime.colname.defused),
                  Date.data = (!!date.handler)(Date.data)) #set the date according to the date handler
  
  #group by Time.data
  dataset <- 
    dataset %>% 
    dplyr::group_by(Time.data, .add = TRUE)
  
  #aggregate the data by group
  numeric.handler <- rlang::enexpr(numeric.handler)
  character.handler <- rlang::enexpr(character.handler)
  logical.handler <- rlang::enexpr(logical.handler)
  factor.handler <- rlang::enexpr(factor.handler)
  
  dataset <- 
    dataset %>% 
    dplyr::summarize(
      Date.data = unique(Date.data),
      ...,
      dplyr::across(dplyr::where(is.numeric), !!numeric.handler), #default: average all numerics
      dplyr::across(dplyr::where(is.character), !!character.handler), #default: choose the dominant string
      dplyr::across(dplyr::where(is.logical), !!logical.handler), #default:  average a binary outcome
      dplyr::across(dplyr::where(is.factor), !!factor.handler), #default: choose the dominant factor
      #allow for additional functions
      .groups = "keep") %>% 
    dplyr::ungroup(Time.data) #remove the rounded Datetime group
    
  #bringing Date and Time together for the final output
  dataset <- 
    dataset %>% 
    dplyr::mutate(!!Datetime.colname.str := paste(Date.data, Time.data) %>% 
                    lubridate::as_datetime(),
                  .after = Date.data) %>% 
    dplyr::select(-Date.data, -Time.data)
  
  # Return ----------------------------------------------------------
  
  dataset
  
}

