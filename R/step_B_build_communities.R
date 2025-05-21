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
    tar_target( # aggregate species from different listening points in the same community
      aggregate_survey,
      available_data %>%
        select(c(matches("SITE"), YEAR, SAMPLING, SPECIES, ABUNDANCE)) %>%
        fill_absences(sampling = "SAMPLING") %>%
        left_join(points_by_habitat, by = str_subset(colnames(.), "^SITE")) %>%
        aggregate_communities
    ),
    tar_target(
      aggregate_survey_file,
      write_path(
        aggregate_survey,
        rel_path = "data/processed/aggregate_survey.csv",
        method = write.csv
      )
    ),
    tar_target(
      aggregate_survey_filtered,
      aggregate_survey %>%
        filter_survey_coverage %>%
        filter_species_presence %>%
        filter_habitats(col_check = "COMMUNITY_ID")
    ),
    tar_target( # from survey and point data, built community_id, year, group, site2, site1
      communities_points,
      aggregate_survey %>% # unfiltered because we might want to extract pressure values everywhere
        coordinates_by_community(
          points_by_habitat,
          sampling_info
        ) %>%
        check_coordinates(threshold_distance = 5000),
      packages = c("sf")
    )
  )
}
