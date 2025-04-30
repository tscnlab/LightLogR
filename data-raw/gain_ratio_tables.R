## code to prepare `gain_ratio_tables` dataset goes here

gain.ratio.tables <- 
  list(
    TSL2585 = 
      tibble::tibble(
        gain = c(0.5, 2^(0:12)),
        gain.ratio = c(0.004014, 0.008074, 0.015881, 0.031526, 0.064392,
                       0.125471, 0.250627, 0.497512, 1, 1.93, 3.8, 7.42,
                       14.06, 25.35)
      ),
    Info = c(TSL2585 = "v1, 2025-02-18")
  )

usethis::use_data(gain.ratio.tables, overwrite = TRUE)
