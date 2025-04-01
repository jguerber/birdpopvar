library(targets)
library(tarchetypes)

tar_source(here::here(c("R", "scripts/import_dependencies.R")))

list(
  tar_target(
    raw_data,
    read_path(
      "data/STOC/fbbs_200m.csv", sep = ";", row.names = NULL
    )
  ),
  tar_target(
    data_clean,
    clean_stoc_short(raw_data)
  )
)
