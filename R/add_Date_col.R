
#' Create a Date column in the dataset
#'
#' @inheritParams cut_Datetime
#' @param Date.colname Name of the newly created column. Expects a `symbol`. The
#'   default(`Date`) works well with other functions in [LightLogR]. Will
#'   overwrite existing columns of identical name.
#' @param as.wday Logical of whether the added column should calculate day of
#'   the week instead of date. If `TRUE` will create a factor with weekday
#'   abbreviations, where the week starts with `Mon`.
#' @param group.by Logical whether the output should be (additionally) grouped
#'   by the new column
#'
#' @return a `data.frame` object identical to `dataset` but with the added
#'   column of Date data
#' @export
#' @importFrom rlang :=
#' @examples
#' sample.data.environment %>% add_Date_col()

#' #days of the week
#' sample.data.environment %>% 
#'   add_Date_col(as.wday = TRUE, group.by = TRUE) |> 
#'   summarize_numeric(remove = c("Datetime"))
#' 
add_Date_col <- function(dataset, 
                         Date.colname = Date,
                         group.by = FALSE,
                         as.wday = FALSE,
                         Datetime.colname = Datetime) {
  
  # Initial Checks ----------------------------------------------------------
  Datetime.colname.defused <- colname.defused({{ Datetime.colname }})
  
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "Datetime.colname must be part of the dataset" = 
      Datetime.colname.defused %in% names(dataset),
    "Datetime.colname must be a Datetime" = 
      lubridate::is.POSIXct(dataset[[Datetime.colname.defused]]),
    "as.wday has to be a logical" = 
      is.logical(as.wday),
    "group.by has to be a logical" = is.logical(group.by)
  )
  
  # Manipulation ----------------------------------------------------------
  if(!as.wday)
    dataset <-
      dataset |> 
       dplyr::mutate(
         {{ Date.colname }} := {{ Datetime.colname }} %>% lubridate::as_date()
    )  else{
      dataset <-
        dataset |> 
        dplyr::mutate(
          {{ Date.colname }} := {{ Datetime.colname }} |> 
            lubridate::wday(label = TRUE, week_start = 1)
        )
    }
    
  if(group.by){
    dataset <- 
      dataset |> dplyr::group_by({{ Date.colname }}, .add = TRUE)
  }
  
  # Return ----------------------------------------------------------
  dataset
}
