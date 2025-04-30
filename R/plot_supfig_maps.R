build_fig_maps <- function(target_store) {

  final_data <- tar_read(
    population_variability_data,
    store = target_store
  )

  final_data_communities <- final_data %>%
    pull(COMMUNITY_ID)

  communities_info <- final_data %>%
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



  france <- france_shp(
    path = here_from_pipeline("data/maps/regions/regions-20180101.shp"),
    corsica = F
  )

  community_centroids <- tar_read(community_coordinates, store = target_store) %>%
    filter(COMMUNITY_ID %in% final_data$COMMUNITY_ID) %>%
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
      data = france,
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
