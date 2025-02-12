
#' Create Datetime bins for visualization and calculation
#'
#' `cut_Datetime` is a wrapper around [lubridate::round_date()] (and friends)
#' combined with [dplyr::mutate()], to create a new column in a light logger
#' dataset with a specified binsize. This can be `"3 hours"`, `"15 secs"`, or
#' `"0.5 days"`. It is a useful step between a dataset and a visualization or
#' summary step.
#'
#' @param dataset A light logger dataset. Expects a `dataframe`. If not imported
#'   by [LightLogR], take care to choose a sensible variable for the
#'   `Datetime.colname`.
#' @param unit Unit of binning. See [lubridate::round_date()] for examples. The default is `"3 hours"`. 
#' @param type One of `"round"`(the default), `"ceiling"` or `"floor"`. Setting
#'   chooses the relevant function from \pkg{lubridate}.
#' @param Datetime.colname column name that contains the datetime. Defaults to
#'   `"Datetime"` which is automatically correct for data imported with
#'   [LightLogR]. Expects a `symbol`. Needs to be part of the `dataset`. Must
#'   be of type `POSIXct`.
#' @param New.colname Column name for the added column in the `dataset`.
#' @param ... Parameter handed over to [lubridate::round_date()] and siblings
#' @param group_by Should the data be grouped by the new column? Defaults to `FALSE`
#'
#' @return a `data.frame` object identical to `dataset` but with the added
#'   column of binned datetimes.
#' @export
#' @importFrom rlang :=
#' @examples
#' #compare Datetime and Datetime.rounded
#' sample.data.environment %>%
#'   cut_Datetime() %>%
#'   dplyr::slice_sample(n = 5)


cut_Datetime <- function(dataset,
                         unit = "3 hours",
                         type = c("round", "floor", "ceiling"),
                         Datetime.colname = Datetime,
                         New.colname = Datetime.rounded,
                         group_by = FALSE,
                         ...) {
  
  # Initial Checks ----------------------------------------------------------
  
  # Match input arguments
  type <- match.arg(type)
  
  Datetime.colname.defused <- 
    rlang::enexpr(Datetime.colname) %>% rlang::as_string()
  New.colname.defused <- 
    rlang::enexpr(New.colname)
  
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "unit is not a scalar" = length(unit) == 1,
    # "unit is not a character string or a duration" = 
    #   lubridate::is.duration(unit) | is.character(unit),
    "Datetime.colname must be part of the dataset" = 
      Datetime.colname.defused %in% names(dataset),
    "Datetime.colname must be a Datetime" = 
      lubridate::is.POSIXct(dataset[[Datetime.colname.defused]]),
    "New.colname must be a Symbol" = 
      rlang::is_symbol({{ New.colname.defused }}),
    "group_by must be a Logical" = 
      is.logical(group_by)
  )
  
  # Manipulation ----------------------------------------------------------
  
  #create the epochs list
  if(unit == "dominant.epoch") {
  unit <- epoch_list(dataset, Datetime.colname = {{ Datetime.colname }})
  }
  else {
    unit <- dataset %>% 
      dplyr::summarize(
        dominant.epoch = unit %>% lubridate::as.period(),
        group.indices = dplyr::cur_group_id()
      )
  } 
  
  #give the user the chance to use whatever function they want
  round_function_expr <- rlang::parse_expr(paste0("lubridate::", type, "_date"))
  
  #perform the actual rounding
  dataset <- dataset %>% 
    dplyr::mutate(
      group.indices2 = dplyr::cur_group_id(),
      {{ New.colname }} := 
        {{ Datetime.colname }} %>% 
        eval(round_function_expr)(
          unit = unit %>% 
                                    dplyr::filter(group.indices == 
                                                    unique(group.indices2)) %>% 
                                    .[["dominant.epoch"]] %>% 
                                    lubridate::as.period(), 
                                  ...
                                  ),
      .after = {{ Datetime.colname }}
      ) %>% 
    dplyr::select(-group.indices2)
  
  if(group_by) {
    dataset <- dataset %>% dplyr::group_by({{ New.colname }}, .add = TRUE)
  }
  
  # Return ----------------------------------------------------------
  dataset
}