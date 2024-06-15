#' Aggregate Datetime data
#'
#' Condenses a `dataset` by aggregating the data to a given (shorter) interval
#' `unit`. [aggregate_Datetime()] is opinionated in the sense that it sets
#' default handlers for each data type of `numeric`, `character`, `logical`, and
#' `factor`. These can be overwritten by the user. Columns that do not fall into
#' one of these categories need to be handled individually by the user (`...`
#' argument) or will be removed during aggregation. If no unit is specified the
#' data will simply be aggregated to the most common interval
#' (`dominant.epoch`), which is most often not an aggregation but a rounding.)
#'
#' @inheritParams cut_Datetime
#' @param numeric.handler,character.handler,logical.handler,factor.handler
#'   functions that handle the respective data types. The default handlers
#'   calculate the `mean` for `numeric` and the `mode` for `character`, `factor`
#'   and `logical` types.
#' @param unit Unit of binning. See [lubridate::round_date()] for examples. The
#'   default is `"dominant.epoch"`, which means everything will be aggregated to
#'   the most common interval. This is especially useful for slightly irregular
#'   data, but can be computationally expensive. `"none"` will not aggregate the
#'   data at all.
#' @param ... arguments given over to [dplyr::summarize()] to handle columns
#'   that do not fall into one of the categories above.
#'
#' @return A `tibble` with aggregated `Datetime` data. Usually the number of
#'   rows will be smaller than the input `dataset`. If the handler arguments
#'   capture all column types, the number of columns will be the same as in the
#'   input `dataset`.
#' @export
#'
#' @examples
#' #dominant epoch without aggregation
#' sample.data.environment %>%
#'  dominant_epoch()
#'
#' #dominant epoch with 5 minute aggregation
#' sample.data.environment %>%
#'  aggregate_Datetime(unit = "5 mins") %>%
#'  dominant_epoch()
#'
#' #dominant epoch with 1 day aggregation
#' sample.data.environment %>%
#'  aggregate_Datetime(unit = "1 day") %>%
#'  dominant_epoch()
aggregate_Datetime <- function(dataset,
                               Datetime.colname = Datetime,
                               unit = "dominant.epoch",
                               type = c("round", "floor", "ceiling"),
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

  #capture the handlers
  
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "Datetime.colname must be part of the dataset" =
      Datetime.colname.str %in% names(dataset),
    "Datetime.colname must be a Datetime" =
      lubridate::is.POSIXct(dataset[[Datetime.colname.str]]),
    "numeric.handler must be a function" = is.function(numeric.handler),
    "character.handler must be a function" = is.function(character.handler),
    "logical.handler must be a function" = is.function(logical.handler),
    "factor.handler must be a function" = is.function(factor.handler)
  )

  # Function ----------------------------------------------------------
  
  if(unit != "none") {
    dataset <- 
    dataset  %>% 
    cut_Datetime(
      Datetime.colname = !!Datetime.colname.defused,
      unit = unit, 
      type = type,
      group_by = TRUE) %>% #choose the resolution of our aggregated data
    dplyr::summarize(
      ..., #allow for additional functions
      dplyr::across(dplyr::where(is.numeric), !!numeric.handler), #default: average all numerics
      dplyr::across(dplyr::where(is.character), !!character.handler), #default: choose the dominant string
      dplyr::across(dplyr::where(is.logical), !!logical.handler), #default:  average a binary outcome
      dplyr::across(dplyr::where(is.factor), !!factor.handler), #default: choose the dominant factor
      .groups = "drop_last") %>% #remove the rounded Datetime group
    dplyr::rename(Datetime = Datetime.rounded) #remove the rounded Datetime column
  }
    
  # Return ----------------------------------------------------------
  
  dataset
  
}

