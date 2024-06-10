#tests whether the given input is a POSIXct, hms, or string of correct input
test.Time.regex <- function(input) {
  
  if(!is.null(input)){
    
    input.defused <- rlang::enexpr(input)
    
    test <- dplyr::case_when(
      lubridate::is.POSIXct(input) ~ TRUE,
      hms::is_hms(input) ~ TRUE,
      stringr::str_detect(
        input, 
        pattern = "^([01]?[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$") ~ TRUE,
      .default = FALSE
    )
    
    if(!test){
      stop(paste("input:",
                 rlang::as_string(
                   input.defused), 
                 "needs to be in the format 'hh:mm:ss'"))
    }
  }
}

#extracts a symbol and gives it back as a string when necessary
colname.defused <- function(Colname, as_string = TRUE) {
  Colname <- rlang::ensym(Colname) 
  if(as_string) {
    rlang::as_string(Colname)
  }
  else Colname
}

#tests whether the inputs are all scalar
is.all.scalar <- function(...) {
  list(...) %>% 
    purrr::every(\(x) length(x) == 1)
}

#calculate the whether the nth quantile of time differences in one dataset is smaller or equal to the nth quantile of time differences in another dataset
compare_difftime <- function(dataset1, dataset2, Datetime.colname = Datetime, n = 0.95) {
  Quant1 <- nth.difftime(dataset1, {{ Datetime.colname }}, n = n)
  Quant2 <- nth.difftime(dataset2, {{ Datetime.colname }}, n = n)
  #do a full join with every column but Quantile
  group_variables <- setdiff(names(Quant2), "Quant")
  dplyr::full_join(Quant1, Quant2, by = group_variables) %>% 
    dplyr::mutate(
      comparison = Quant.x <= Quant.y
    )
}

#calculate whether any of the comparisons in compare_difftime is FALSE
compare_difftime.any <- function(...) {
  comparison <- compare_difftime(...) %>% 
    dplyr::filter(comparison == FALSE) %>% 
    dplyr::rename(Dataset.Interval = Quant.x,
                  Reference.Interval = Quant.y) %>% 
    dplyr::select(-comparison)
  
  if(nrow(comparison) > 0) comparison else TRUE
}

#create a reference label
create.Reference.label <- function(dataset, 
                                   Reference.column, 
                                   Reference.label = NULL) {
  if(!is.null(Reference.label)) {
    Reference.column.str <- colname.defused({{ Reference.column }})
    Reference.label.column.str <- paste0(Reference.column.str, ".label")
    
    dataset <- 
      dataset %>% 
      dplyr::mutate(!!Reference.label.column.str := 
                      dplyr::if_else(
                        !is.na({{ Reference.column }}), Reference.label, NA
                      )
      )
    dataset
  } else dataset
}

#helper to pick the colums that are used for grouping
pick.grouping.columns <- function(dataset) {
  dplyr::pick(
    dplyr::group_vars(dataset)
  )
}

# Compare with threshold
compare_threshold <- function(Light.vector,
                              threshold,
                              comparison = c("above", "below"),
                              na.replace = FALSE){
  
  comparison = match.arg(comparison)
  
  stopifnot(
    "`Light.vector` must be numeric!" = is.numeric(Light.vector),
    "`threshold` must be numeric!" = is.numeric(threshold),
    "`threshold` must be either one or two values!" = length(threshold) %in% c(1, 2),
    "`na.replace` must be logical!" = is.logical(na.replace)
  )
  
  if(length(threshold) == 1){
    out <- switch(comparison,
                  "above" = Light.vector >= threshold,
                  "below" = Light.vector <= threshold)
  }
  else{
    threshold <- sort(threshold)
    out <- Light.vector >= threshold[1] & Light.vector <= threshold[2]
  }
  
  if(na.replace){
    out <- tidyr::replace_na(out, FALSE)
  }
  
  return(out)
}

# Find clusters of consecutive `TRUE` values in logical vector. 
# Allows to concatenate periods of consecutive values that are interrupted 
# by periods of `FALSE` values. 
find_clusters <- function(x,
                          min.length,
                          max.interrupt = 0,
                          prop.interrupt = 1,
                          cluster_name = "cluster") {
  
  stopifnot(
    "`x` must be logical" = is.logical(x),
    "`min.length` must be larger than 0" = min.length > 0,
    "`max.interrupt` must be larger than or equal to 0" = max.interrupt >= 0,
    "`prop.interrupt` must be between 0 and 1" = 
      prop.interrupt >= 0 & prop.interrupt <= 1
  )
  
  # Replace NA with FALSE
  x <- tidyr::replace_na(x, FALSE)
  
  # Find the start and end indices of each cluster of consecutive values
  start_indices <- which(x & !dplyr::lag(x, default = FALSE))
  end_indices <- which(x & !dplyr::lead(x, default = FALSE))
  ranges <- as.numeric(matrix(rbind(start_indices, end_indices), nrow = 2))
  
  # Remove ranges < min.length
  intra_diff <- diff(ranges)[1:(length(ranges) - 1) %% 2 != 0] + 1
  exclude_ranges <- c(
    which(intra_diff < min.length) * 2,
    which(intra_diff < min.length) * 2 - 1
  )
  if (length(exclude_ranges) > 0) {
    ranges <- ranges[-exclude_ranges]
  }
  
  # Calculate cumulative ranges
  intra_diff <- diff(ranges)[1:(length(ranges) - 1) %% 2 != 0] + 1
  intra_cumsum <- intra_diff[1:length(intra_diff)-1] + intra_diff[2:length(intra_diff)]
  
  # Inter-range differences
  inter_diff <- diff(ranges)[1:(length(ranges) - 1) %% 2 == 0] - 1
  
  # Proportion inter-range difference and cumulative range sums
  interrupt_ratio <- inter_diff / intra_cumsum
  
  # Combine ranges with inter-range difference <= max.interrupt &
  # interrupt ratio <= prop.interrupt
  exclude_ranges <- c(
    which(inter_diff <= max.interrupt & interrupt_ratio <= prop.interrupt) * 2,
    which(inter_diff <= max.interrupt & interrupt_ratio <= prop.interrupt) * 2 + 1
  )
  if (length(exclude_ranges) > 0) {
    ranges <- ranges[-exclude_ranges]
  }
  
  # Make data frame with intervals
  if (length(ranges) > 0) {
    intervals <-
      matrix(ranges, ncol = 2, byrow = TRUE) %>%
      as.data.frame() %>%
      magrittr::set_names(c("cluster_start", "cluster_end")) %>%
      dplyr::mutate(cluster_idx = 1:length(cluster_start)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(row_idx = list(seq(cluster_start, cluster_end))) %>%
      tidyr::unnest(cols = c(row_idx)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(is_cluster = TRUE) %>%
      dplyr::relocate(row_idx, is_cluster, cluster_idx, cluster_start, cluster_end)
  } else {
    intervals <-
      tibble::tibble(
        row_idx = 1, is_cluster = NA, cluster_idx = NA,
        cluster_start = NA, cluster_end = NA
      )
  }
  # Replace "cluster" with custom name
  intervals <- intervals %>% 
    dplyr::rename_with(~gsub("cluster", cluster_name, .x))
  
  return(intervals)
}

# Convert `x` to time scale of `t`
convert_to_timescale <- function(x, t){
  if(lubridate::is.POSIXct(t)){
    x <- lubridate::as_datetime(x, tz = lubridate::tz(t))
  }
  if(hms::is_hms(t)){
    x <- hms::as_hms(x)
  }
  if(lubridate::is.duration(t)){
    x <- lubridate::as.duration(x)
  }
  if(lubridate::is.difftime(t) & !hms::is_hms(t)){
    x <- lubridate::as.difftime(x, unit = units(t))
  }
  return(x)
}

# Create the list of epochs, which are either dominant or not
epoch_list <- function(dataset = dataset, 
                       Datetime.colname = Datetime, 
                       epoch = "dominant.epoch") {
  
  #get the epochs based on the data
  epochs <- dataset %>% dominant_epoch(Datetime.colname = {{ Datetime.colname }})
  
  #if the user specified an epoch, use that instead
  if(epoch != "dominant.epoch") {
    epochs <- 
      epochs %>% dplyr::mutate(dominant.epoch = lubridate::as.duration(epoch))
  }
  
  epochs
}
