library(targets)
library(tarchetypes)

tar_source(here::here(c("R", "scripts/import_dependencies.R")))

parameters <- yaml::read_yaml(here::here("parameters.yaml"))

tar_option_set(
  # performance : delegate as much work as possible to worker processes
  garbage_collection = TRUE,
  memory = "transient",
  storage = "worker",
  retrieval = "worker",
  deployment = "worker",
  trust_timestamps = T, # don't hash big files
  # available packages and libraries
  packages = default_dependencies(),
  library = renv::paths$library()
)

target_list(
  parameters
)
