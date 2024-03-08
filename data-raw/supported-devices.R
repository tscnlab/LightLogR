## code to prepare `supported.devices` dataset goes here

library(LightLogR)
supported.devices <- names(ll_import_expr) %>% sort()

usethis::use_data(supported.devices, overwrite = TRUE)
