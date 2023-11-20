#' Join similar Datasets
#'
#' Join Light logging datasets that have a common structure. The least commonality are identical columns for `Datetime` and `Id` across all sets.
#'
#' @param ... `Object names` of datasets that need to be joined.
#' @param Datetime.column,Id.column Column names for the `Datetime` and `id` columns. The defaults (`Datetime`, `Id`) are already set up for data imported with [LightLogR].
#' @param add.origin Should a column named `dataset` in the joined data indicate from which dataset each observation originated? Defaults to `FALSE` as the `Id` column should suffice. Expects a `logical`.
#' @param debug Output changes to a tibble indicating which dataset is missing the respective `Datetime` or `Id` column. Expects a `logical` and defaults to `FALSE`.
#'
#' @return One of
#' * a `data.frame` of joined datasets
#' * a `tibble` of datasets with missing columns. Only if `debug = TRUE`
#' @export
#' @examples
#' #load in two datasets
#' path <- system.file("extdata", 
#' package = "LightLogR")
#' file.LL <- "205_actlumus_Log_1020_20230904101707532.txt.zip"
#' file.env <- "cyepiamb_CW35_Log_1431_20230904081953614.txt.zip"
#' dataset.LL <- import$ActLumus(file.LL, path, auto.id = "^(\\d{3})")
#' dataset.env <- import$ActLumus(file.env, path, manual.id = "CW35")
#' 
#' #join the datasets
#' joined <- join_datasets(dataset.LL, dataset.env)
#' 
#' #compare the number of rows
#' nrow(dataset.LL) + nrow(dataset.env) == nrow(joined)
#' 
#' #debug, when set to TRUE, will output a tibble of datasets with missing necessary columns
#' dataset.LL <- dataset.LL %>% dplyr::select(-Datetime)
#' join_datasets(dataset.LL, dataset.env, debug = TRUE)

join_datasets <- function(...,
                          Datetime.column = Datetime,
                          Id.column = Id,
                          add.origin = FALSE,
                          debug = FALSE) {
  datasets <- rlang::dots_list(..., .named = TRUE)
  
  all.datasets <-
    datasets %>% purrr::map(is.data.frame) %>% as.logical()
  
  stopifnot(
    "all given datasets must be data.frames" = all(all.datasets),
    "add.origin must be a logical" = is.logical(add.origin),
    "debug" = is.logical(debug)
  )
  
  Datetime.column.defused <- colname.defused({{ Datetime.column }})
  Id.column.defused <- colname.defused({{ Id.column }})
  column.names <- c(Datetime.column.defused, Id.column.defused)
  
  dataset.names <- datasets %>% purrr::map(names)
  
  are.names.present <- 
    dataset.names %>% 
    purrr::map(\(x) column.names %in% x)
  
  all.present <- are.names.present %>% unlist() %>% all()
  
  if(debug) {
    return(are.names.present %>%
             tibble::as_tibble() %>%
             dplyr::mutate(column.names.in = column.names, .before = 1) %>% 
             dplyr::select(dplyr::where(\(x) if(is.logical(x)) !all(x) else TRUE))
    )
  }
  
  stopifnot(
    "Not all datasets have the required Datetime and ID columns. Use debug = TRUE if you want to see which datasets are not ok." = 
      all.present)
  
  if(add.origin) {
    datasets <- datasets %>%
      purrr::imap(\(x,y) {x %>% dplyr::mutate(dataset = y)})
  }
  
  # return
  dplyr::bind_rows(datasets)
}

