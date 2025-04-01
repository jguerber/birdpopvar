library(targets)
library(tarchetypes)

tar_source(here::here("R"))

list(
  tar_target(
    raw_data,
    read_path(
      "data/STOC/fbbs_200m/csv", sep = ";"
    )
  ),
  tar_target(
    data_clean,
    clean_stoc_short(raw_data)
  )
)
