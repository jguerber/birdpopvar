step_load_targets <- function(parameters) {

  list(
    tar_target(
      population_variability_data_file,
      "data/processed/bird_population_variability.csv",
      format = "file"
    ),
    tar_target(
      population_variability_data,
      read_path(
        population_variability_data_file,
        method = read.csv
      )
    )
  )

}
