## code to prepare `ll_import_expr` dataset goes here
#this list contains the expressions to import data for each device

library(LightLogR)
#this is needed to make the list available to the user
usethis::use_data(ll_import_expr, overwrite = TRUE)
