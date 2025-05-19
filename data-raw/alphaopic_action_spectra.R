## code to prepare `alphaopic_action_spectra` dataset goes here

library(here)

path <- paste0(here(), "/data-raw/alphaopic.action.spectra.csv")

alphaopic.action.spectra <- read.csv(path)

usethis::use_data(alphaopic.action.spectra, overwrite = TRUE)
