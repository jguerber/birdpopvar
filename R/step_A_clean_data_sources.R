step_data_cleaning <- function() {
  list(
    tar_target(
      raw_data,
      read_path(
        file.path("data", parameters$survey_data), sep = ";", row.names = NULL
      )
    ),
    tar_target(
      data_clean,
      clean_fbbs(raw_data)
    ),
    tar_target(
      sampling_info,
      read_path(
        file.path("data", "STOC", "sampling.csv"),
        sep = ",",
        header = T
      ) %>% clean_sampling_info
    )
  )
}
