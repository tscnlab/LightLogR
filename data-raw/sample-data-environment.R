## code to prepare `sample.data.environment` dataset goes here

library(LightLogR)
library(tidyverse)

path <- system.file("extdata", 
                    package = "LightLogR")

file.LL <- "205_actlumus_Log_1020_20230904101707532.txt.zip"
file.env <- "cyepiamb_CW35_Log_1431_20230904081953614.txt.zip"

tz <- "Europe/Berlin"
sample.data.environment <- import$ActLumus(c(file.LL, file.env), path, auto.id = "^(\\d{3})", tz = tz)

sample.data.environment <- 
  sample.data.environment |> 
  mutate(Id = case_when(
    Id == "205" ~ "Participant",
    is.na(Id) ~ "Environment") |> 
      factor(levels = c("Environment", "Participant")))

sample.data.environment <- 
  sample.data.environment |>  
  filter_Date(start = "2023-08-29", end = "2023-09-03") |>  
  select(Id, Datetime, MEDI)

usethis::use_data(sample.data.environment, overwrite = TRUE)
