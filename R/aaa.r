Time <- mEDI <- Time.data <- Datetime <- timestamp <- tz <- Day.data <- `DATE/TIME` <- n <- Datetime.rounded <- id <- sleep.colname.string <- file.name <- Interval <- original.datapoints.fleeting <- MEDI <- State.Brown <- Reference <- Reference.check <- Id <- Start.date.shift <- data <- Shift <- `MELANOPIC EDI` <- State <- group <- End <- Start <- NULL

empty_function <- function() {
  rsconnect::accountInfo()
  pkgload::check_dep_version()
  flextable::add_body()
}

# supported.devices <- supported.devices

.onLoad <- function(libname, pkgname) {
  utils::globalVariables("supported.devices")
}
