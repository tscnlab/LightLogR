## code to prepare `supported.devices` dataset goes here

library(LightLogR)
supported.devices <- names(import_arguments)

usethis::use_data(supported.devices, overwrite = TRUE)
