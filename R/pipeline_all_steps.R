target_list_all <- function() {
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
    )
  )
}
