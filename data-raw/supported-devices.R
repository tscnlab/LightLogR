## code to prepare `supported.devices` dataset goes here

library(LightLogR)
supported.devices <- names(import_arguments) %>% sort()

usethis::use_data(supported.devices, overwrite = TRUE)
