# Write the raw files to a path with only the square plots used in stats
devtools::load_all()
source(here::here("scripts/import_dependencies.R"))

output_dir <- here::here("data/raw/STOC_filtered")
dir.create(output_dir, showWarnings = T, recursive = F)

plot_ids <- read.csv(
  here::here("data/processed/bird_population_variability.csv")
) %>%
  split_community_id %>%
  pull(SITE2) %>%
  unique

read.csv(
  here::here("data/raw/STOC/fbbs_200m.csv"),
  header = F,
  sep = ";"
) %>%
  filter(V1 %in% plot_ids) %>%
  write.table(
    file.path(output_dir, "fbbs_200m.csv"),
    col.names = F,
    row.names = F,
    quote = F,
    sep = ";"
  )

read.csv(
  here::here("data/raw/STOC/sampling.csv"),
  header = T,
  sep = ","
) %>%
  filter(carre %in% plot_ids) %>%
  write.csv(
    file.path(output_dir, "sampling.csv"),
    row.names = F
  )
