## code to prepare `ll_import_expr` dataset goes here
#this list contains the expressions to import data for each device

#this is needed to make the list available to the user
usethis::use_data(ll_import_expr, overwrite = TRUE)

## code to prepare `supported.devices` dataset goes here
supported.devices <- names(ll_import_expr)
usethis::use_data(supported.devices, overwrite = TRUE)
