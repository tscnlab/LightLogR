#create bindings
.metric <- .weight <- .date <- max_date <- is.irregular <- NULL
marked.for.removal <- wavelength <- total_duration_n <- Group <- is.cluster <- NULL
gap <- episodes <- total_duration <- total <- interval <- gaps <- NULL
state <- epoch <- state.count <- .variable <- dominant.epoch <- type <- NULL
available_data <- available_data_n <- max_datetime <- mean_epoch <- NULL
mean_gap_duration <- mean_gap_duration_n <- missing_data <- NULL
missing_implicit <- missing_pct <- number_gaps <- number_irregulars <- NULL
.data <- gain.ratio <- date.grouper <- midnight.before <- NULL
midnight.after <- temporary.state <- temporary.counter <- NULL
date.for.photoperiod <- dawn <- dusk <- Time <- mEDI <- Time <- NULL
Datetime <- timestamp <- tz <- Day.data <- `DATE/TIME` <- n <- NULL
Datetime.rounded <- id <- sleep.colname.string <- file.name <- Interval <- NULL
original.datapoints.fleeting <- MEDI <- State.Brown <- Reference <- NULL
Reference.check <- Id <- Start.date.shift <- data <- Shift <- NULL
`MELANOPIC EDI` <- State <- group <- End <- Start <- Quant.x <- Quant.y <- NULL
is.implicit <- group.indices <- Id2 <- gap.id <- start <- end <- path <- NULL
auto.id <- n_max <- manual.id <- silent <- Light <- Day <- N <- is_missing <- NULL 
Hour <- .change <- dst_start <- .dst <- .dst2 <- dst_adjustment <- NULL
auto.plot <- group.1 <- group.2 <- group.indices2 <- cluster_start <- NULL
cluster_end <- row_idx <- is_cluster <- cluster_idx <- is_pulse <- NULL
pulse_idx <- light <- time <- level <- duration <- mean_duration <- NULL
onset <- midpoint <- offset <- mean_onset <- mean_midpoint <- NULL
mean_offset <-  Date.data <- print_n <- remove_duplicates <-  NULL

.onLoad <- function(libname, pkgname) {
  utils::globalVariables(c(".", "Date", "Value"))
}

