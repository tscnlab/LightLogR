#' Filter Datetimes in a dataset.
#'
#' Filtering a dataset based on Dates or Datetimes may often be necessary prior
#' to calcuation or visualization. The functions allow for a filtering based on
#' simple `strings` or `Datetime` scalars, or by specifying a length. They also
#' support prior \pkg{dplyr} grouping, which is useful, e.g., when you only want to
#' filter the first two days of measurement data for every participant,
#' regardless of the actual date. If you want to filter based on times of the
#' day, look to [filter_Time()].
#'
#' @inheritParams cut_Datetime
#' @param start,end For [filter_Datetime()] a `POSIXct` or `character` scalar in
#'   the form of `"yyyy-mm-dd hh-mm-ss"` giving the respective start and end
#'   time positions for the filtered dataframe. If you only want to provide
#'   `dates` in the form of `"yyyy-mm-dd"`, use the wrapper function
#'   [filter_Date()].
#' * If one of start/end are not provided, the times will be taken from the respective extreme values of the `dataset`.
#' * If `length` is provided and one of start/end is not, the other will be calculated based on the given value.
#' * If `length` is provided and both of start/end are NULL, the time from the
#'   respective start (within each group) is taken.
#' @param length Either a Period or Duration from \pkg{lubridate}. E.g., `days(2) +
#'   hours(12)` will give a period of 2.5 days, whereas `ddays(2) + dhours(12)`
#'   will give a duration. For the difference between periods and durations look
#'   at the documentation from \pkg{lubridate}. Basically, periods model clocktimes,
#'   whereas durations model physical processes. This matters on several
#'   occasions, like leap years, or daylight savings. You can also provide a
#'   `character` scalar in the form of e.g. "1 day", which will be converted
#'   into a period.
#' @param filter.expr Advanced filtering conditions. If not `NULL` (default) and
#'   given an `expression`, this is used to [dplyr::filter()] the results. This
#'   can be useful to filter, e.g. for group-specific conditions, like starting
#'   after the first two days of measurement (see examples).
#' @param tz Timezone of the start/end times. If `NULL` (the default), it will
#'   take the timezone from the `Datetime.colname` column.
#' @param full.day A `logical` indicating whether the `start` param should be
#'   rounded to a full day, when only the `length` argument is provided (Default
#'   is FALSE). This is useful, e.g., when the first observation in the dataset
#'   is slightly after midnight. If TRUE, it will count the length from midnight
#'   on to avoid empty days in plotting with [gg_day()].
#' @param length_from_start A `logical` indicating whether the `length` argument
#'   should be applied to the start (default, TRUE) or the end of the data
#'   (FALSE). Only relevant if neither the `start` nor the `end` arguments are
#'   provided.
#' @param only_Id An expression of `ids` where the filtering should be applied
#'   to. If `NULL` (the default), the filtering will be applied to all `ids`.
#'   Based on the this expression, the dataset will be split in two and only
#'   where the given expression evaluates to `TRUE`, will the filtering take
#'   place. Afterwards both sets are recombined and sorted by `Datetime`.
#'
#' @return a `data.frame` object identical to `dataset` but with only the
#'   specified Dates/Times.
#' @export
#' @family filter
#' @examples
#'
#' library(lubridate)
#' library(dplyr)
#' #baseline
#' range.unfiltered <- sample.data.environment$Datetime %>% range()
#' range.unfiltered
#'
#' #setting the start of a dataset
#' sample.data.environment %>%
#' filter_Datetime(start = "2023-08-31 12:00:00") %>%
#' pull(Datetime) %>%
#' range()
#'
#' #setting the end of a dataset
#' sample.data.environment %>%
#' filter_Datetime(end = "2023-08-31 12:00:00") %>% pull(Datetime) %>% range()
#'
#' #setting a period of a dataset
#' sample.data.environment %>%
#' filter_Datetime(end = "2023-08-31 12:00:00", length = days(2)) %>%
#' pull(Datetime) %>% range()
#'
#' #setting only the period of a dataset
#' sample.data.environment %>%
#' filter_Datetime(length = days(2)) %>%
#' pull(Datetime) %>% range()
#'
#' #advanced filtering based on grouping (second day of each group)
#' sample.data.environment %>%
#' #shift the "Environment" group by one day
#' mutate(
#' Datetime = ifelse(Id == "Environment", Datetime + ddays(1), Datetime) %>%
#' as_datetime()) -> sample
#' sample %>% summarize(Daterange = paste(min(Datetime), max(Datetime), sep = " - "))
#' #now we can use the `filter.expr` argument to filter from the second day of each group
#' sample %>%
#' filter_Datetime(filter.expr = Datetime > Datetime[1] + days(1)) %>%
#' summarize(Daterange = paste(min(Datetime), max(Datetime), sep = " - "))


filter_Datetime <- function(dataset, 
                            length = NULL,
                            start = NULL, 
                            end = NULL, 
                            length_from_start = TRUE,
                            full.day = FALSE,
                            only_Id = NULL,
                            filter.expr = NULL,
                            Datetime.colname = Datetime, 
                            tz = NULL) {
  
  # Initial Checks ----------------------------------------------------------
  
  filter.expr <- rlang::enexpr(filter.expr)
  
  only_Id <- rlang::enexpr(only_Id)
  
  Datetime.colname.defused <- 
    rlang::enexpr(Datetime.colname) %>% rlang::as_string()
  #timezone
  if(is.null(tz) & lubridate::is.POSIXct(dataset[[Datetime.colname.defused]])) {
    tz <- lubridate::tz(dataset[[Datetime.colname.defused]][1])
  }
  
  if(!is.null(length)){
    test <- lubridate::is.duration(length) | lubridate::is.period(length) | is.character(length)
    stopifnot("length needs to be either a valid character, duration or a period from {lubridate}" = test)
  }
  
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "Datetime.colname must be part of the dataset" = 
      Datetime.colname.defused %in% names(dataset),
    "Datetime.colname must be a Datetime" = 
      lubridate::is.POSIXct(dataset[[Datetime.colname.defused]]),
    "At least one parameter from `start`, `end`, `length` or `filter.expr` must be specified" =
      !all(is.null(start), is.null(end), is.null(length), is.null(filter.expr)),
    "tz needs to be a character" = is.character(tz),
    "tz needs to be a valid time zone, see `OlsonNames()`" = tz %in% OlsonNames(),
    "full.day needs to be a `logical`" = is.logical(full.day)
    )
 
  # Manipulation ----------------------------------------------------------
   
  #split the dataset in two parts, based on the only_Id expression
  if(!is.null(only_Id)) {
    dataset_unfiltered <-
      dataset %>% 
      dplyr::filter(!(!!only_Id), .preserve = TRUE)
    dataset <- 
      dataset %>% 
      dplyr::filter(!!only_Id, .preserve = TRUE)
  }
  
  #if length is a character, convert it to a period
  if(is.character(length)) {
    length <- lubridate::as.period(length)
  }
  
  #was only a length provided?
  only_length <- all(is.null(start), is.null(end), !is.null(length))
  if(only_length) {
    only_length <- if(length_from_start) "start" else "end"
  }

  #calculate starting time if length and end are given
  if(is.null(start) & !is.null(length) & !is.null(end)) {
    start <- lubridate::as_datetime(end, tz = tz) - length
  }
  
  #calculate starting time if NULL
  if(is.null(start)) {
    start <- dataset[[Datetime.colname.defused]] %>% min()
  }
  
  
  
  #calculate end time if length is given & length_from_start is TRUE
  if(is.null(end) & !is.null(length)) {
  if(length_from_start) {
    end <- switch (full.day %>% as.character(),
      "TRUE" = lubridate::as_date(start, tz = tz),
      "FALSE" = lubridate::as_datetime(start, tz = tz)
    )  + length
  } else {
    end <- dataset[[Datetime.colname.defused]] %>% max()
    start <- switch (full.day %>% as.character(),
                     "TRUE" = lubridate::as_date(end, tz = tz),
                     "FALSE" = lubridate::as_datetime(end, tz = tz)
    ) - length
  }
  }
  
  #calculate end time if NULL
  if(is.null(end)) {
    end <- dataset[[Datetime.colname.defused]] %>% max()
  }
  
  # filter start
  if(only_length %in% c("start", "end")) {
    dataset <-
      dataset %>% 
      dplyr::filter(
        switch(only_length,
        "start" = {{ Datetime.colname }} < (min({{ Datetime.colname }}) + length),
        "end" = {{ Datetime.colname }} >= (max({{ Datetime.colname }}) - length)
        )
      )
  } else {
    dataset <-
      dataset %>% 
      dplyr::filter(
        {{ Datetime.colname }} >= lubridate::as_datetime(start, tz = tz),
        {{ Datetime.colname }} < lubridate::as_datetime(end, tz = tz),
        )
  }
    #possible extra filter step
    if(!is.null(filter.expr)) {
      dataset <- dataset %>% dplyr::filter(!!filter.expr)
    }
  
  # Return --------------------------------------------------------------
    if(!is.null(only_Id)) {
      dplyr::bind_rows(dataset, dataset_unfiltered) %>% 
        dplyr::arrange({{ Datetime.colname }}, .by_group = TRUE)
    } else dataset
}


# filter_Date -------------------------------------------------------------

#' Filter Dates in a dataset.
#'
#' @rdname filter_Datetime
#' @export
#' @family filter
#' @examples
#' 
#' sample.data.environment %>% filter_Date(end = "2023-08-31")

filter_Date <- function(...,
                        start = NULL, 
                        end = NULL) {

  if(!is.null(start)) {
    start <- lubridate::as_date(start)
  }

  if(!is.null(end)) {
    end <- lubridate::as_date(end) + lubridate::days()
  }
  
  filter_Datetime(...,
                  start = start,
                  end = end)
}

# multiple filter_Date -------------------------------------------------------------

#' Filter multiple times based on a list of arguments.
#'
#' [filter_Datetime_multiple()] is a wrapper around [filter_Datetime()] or
#' [filter_Date()] that allows the cumulative filtering of `Datetimes` based on
#' varying filter conditions. It is most useful in conjunction with the
#' `only_Id` argument, e.g., to selectively cut off dates depending on
#' participants (see examples)
#'
#' @param dataset A light logger dataset
#' @param arguments A list of arguments to be passed to [filter_Datetime()] or
#'   [filter_Date()]. each list entry must itself be a list of arguments, e.g,
#'   `list(start = "2021-01-01", only_Id = quote(Id == 216))`. Expressions have
#'   to be quoted with [quote()] or [rlang::expr()].
#' @param filter_function The function to be used for filtering, either
#'   `filter_Datetime` (the default) or `filter_Date`
#' @param ... Additional arguments passed to the filter function. If the
#'   `length` argument is provided here instead of the `argument`, it has to be
#'   written as a string, e.g., `length = "1 day"`, instead of `length =
#'   lubridate::days(1)`.
#'
#' @return A dataframe with the filtered data
#' @export
#'
#' @examples
#' arguments <- list(
#'  list(start = "2023-08-31", only_Id = quote(Id == "Participant")),
#'  list(end = "2023-08-31", only_Id = quote(Id == "Environment")))
#'  #compare the unfiltered dataset
#'  sample.data.environment %>% gg_overview(Id.colname = Id)
#'  #compare the unfiltered dataset
#'  sample.data.environment %>%
#'  filter_Datetime_multiple(arguments = arguments, filter_Date) %>%
#'  gg_overview(Id.colname = Id)
filter_Datetime_multiple <- function(dataset, 
                                     arguments, 
                                     filter_function = filter_Datetime,
                                     ...) {
  
  purrr::reduce(arguments, function(dataset, params) {
    do.call({{ filter_function }}, c(list(dataset = dataset), params, ...))
  }, .init = dataset)
}