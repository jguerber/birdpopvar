step_load_targets <- function(parameters) {

  loading_targets <- tibble(
    name = c(
      "aggregate_survey",
      "all_local_trends_outputs",
      "community_coordinates",
      "population_variability_data"
    ),
    path = c(
      "data/processed/aggregate_survey.csv",
      "data/processed/all_local_trends_outputs.csv",
      "data/processed/community_coordinates.csv",
      "data/processed/bird_population_variability.csv"
    )
  )

  tar_eval(
    list(
      tar_target(
        file_target_name,
        command = file_rel_path,
        format = "file"
      ),
      tar_target(
        data_target_name,
        command = read_path(
          file_target_name,
          method = read.csv
        )
      )
    ),
    values = list(
      file_target_name = rlang::syms(paste0(loading_targets$name, "_file")),
      data_target_name = rlang::syms(loading_targets$name),
      file_rel_path = loading_targets$path
    )
  )

}
