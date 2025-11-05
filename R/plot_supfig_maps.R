build_fig_maps <- function(
   population_variability_data,
   community_coordinates,
   france_shape
  ) {

  final_data_communities <- population_variability_data %>%
    pull(COMMUNITY_ID)

  communities_info <- population_variability_data %>%
    split_community_id %>%
    group_by(HABITAT_GROUP) %>%
    summarise(N = n_distinct(COMMUNITY_ID)) %>%
    add_row(
      HABITAT_GROUP = "all", N = sum(pull(., N))
    ) %>%
    arrange(desc(N)) %>%
    mutate(
      label = paste0(str_to_sentence(HABITAT_GROUP), " (", N, ")")
    )

  community_centroids <- community_coordinates %>%
    filter(COMMUNITY_ID %in% population_variability_data$COMMUNITY_ID) %>%
    split_community_id

  point_data <- community_centroids %>%
    mutate(
      HABITAT_GROUP = "all"
    ) %>%
    bind_rows(community_centroids) %>%
    left_join(communities_info, by = "HABITAT_GROUP") %>%
    mutate(
      type_milieu_legend = factor(label, levels = communities_info$label)
    ) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(4326))

  ggplot() +
    geom_sf(
      data = france_shape,
      aes(geometry = geometry),
      fill = "grey90"
    ) +
    geom_sf(
      data = point_data,
      aes(geometry = geometry, color = HABITAT_GROUP),
      size = 0.66
    ) +
    theme_minimal() +
    scale_habitats() +
    facet_wrap(~type_milieu_legend) +
    theme(
      legend.position = "none",
      panel.grid = element_blank()
    )
}
