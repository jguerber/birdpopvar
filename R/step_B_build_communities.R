step_build_communities <- function() {
  list(
    tar_target(
      points_by_habitat,
      sampling_info %>%
        observer_habitats_fbbs
    ),
    tar_target(
      available_data, # available data : point-level data with an available habitat description
      data_clean %>%
        left_join(points_by_habitat, by = c("SITE2", "SITE1")) %>%
        filter(!is.na(HABITAT_GROUP))
    ),
    tar_target(
      available_data_in_habitats_of_interest, # habitats of interest
      available_data %>%
        filter(HABITAT_GROUP %in% c("farmland", "woodland", "built"))
    ),
    tar_target(
      aggregate_survey,
      available_data %>%
        fill_absences %>%
        left_join(points_by_habitat, by = str_subset(colnames(.), "^SITE")) %>%
        aggregate_communities
    )
  )
}
