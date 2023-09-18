Time <- mEDI <- Time.data <- Datetime <- timestamp <- tz <- Day.data <- `DATE/TIME` <- n <- Datetime.rounded <-  NULL

empty_function <- function() {
  rsconnect::accountInfo()
  pkgload::check_dep_version()
  flextable::add_body()
}

# supported.devices <- supported.devices

.onLoad <- function(libname, pkgname) {
  utils::globalVariables("supported.devices")
}
