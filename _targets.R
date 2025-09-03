library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)

tar_source(here::here(c("R", "scripts/import_dependencies.R")))

# load globals
parameters <- yaml::read_yaml(here::here("parameters.yaml"))
survey_data_file <- parameters$survey_data


controller_group <- custom_controller_group(
  parameters
)

tar_option_set(
  # performance : delegate as much work as possible to worker processes
  garbage_collection = TRUE,
  memory = "transient",
  storage = "worker",
  retrieval = "worker",
  deployment = "worker",
  trust_timestamps = T, # don't compare file contents when timestamps match
  format = "qs", # compress objects in target store
  # available packages and libraries
  packages = default_dependencies(),
  library = renv::paths$library(),
  # crew settings :
  controller = controller_group$controller,
  resources = tar_resources( # set controller for small jobs as default
    crew = tar_resources_crew(controller = controller_group$names$normal)
  ),
  error = "trim"
)

tar_proj <- tar_get_current_project() # safely read $TAR_PROJECT

message(
  "Running pipeline with project ", tar_proj
)

invisible(assert_data_files(tar_proj))

# call the correct target list in R/pipeline_*
target_list(
  parameters,
  tar_proj
)
