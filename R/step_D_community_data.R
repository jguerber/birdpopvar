# Community-level data
#
# From all different data sources, collect relevant information for each community
#  - geographical coordinates from sampling information
#  - landscape complexity from Corine Land Cover
#  - human impact data from HII rasters
#  - average stability metrics from results of step C

step_community_data <- function(parameters) {
  geography <- list(
    tar_target( # a single pair of coordinates for each community
      community_coordinates,
      communities_points %>%
        select(-YEAR) %>%
        unique %>%
        group_by(COMMUNITY_ID) %>%
        centroid_coordinates(
          coords = c("lon1", "lat1")
        )
    )
  )

  landscape_complexity <- list(
    tar_map(
      values = tibble(
        buffer_size = c(250, 500, 750)
      ),
      names = "buffer_size",
      tar_target(
        buffers_landuse,
        communities_points %>%
          select(-YEAR) %>%
          unique %>%
          intersect_buffer_clc(clc_2018, buffer_radius_m = buffer_size),
        packages = c(default_dependencies(), "sf")
      ),
      tar_target(
        habitat_proportions,
        buffers_landuse %>%
          build_habitat_proportions(clc_year = "2018"),
        packages = c(default_dependencies(), "sf")
      ),
      tar_target(
        landscape_complexity,
        habitat_proportions %>%
          select(COMMUNITY_ID, area_buffer, H, H_fine)
      )
    )
  )

  human_impact <- list(
    tar_target(
      hii_proxy_relative_path,
      "data/HII/HII_France"
    ), # for each point (focal) or community, extract the 20-year time series of HII
    tar_target(
      yearly_hii_focal,
      communities_points %>%
        select(-YEAR) %>% # remove year from sampling to keep all hii years
        unique %>%
        build_yearly_hii_focal(
          stack_from_repo(
            hii_proxy_relative_path,
            as_proxy = T
          ),
          .,
          crs_epsg = 4326,
          buffer_size = "focal"
        ),
      packages = c(default_dependencies(), "sf", "stars")
    ),
    # extracting HII in buffers is a bit slow :
    # parallelize via dynamic (over community batches)-within-static
    # (over buffer size) branching
    tar_group_by(
      community_coordinates_in_batches,
      community_coordinates %>%
        prepare_trend_batches(parameters) %>%
        left_join(community_coordinates, by = "COMMUNITY_ID"),
      batch_id
    ),
    tar_map(
      values = tibble(
        buffer_size = c("5km", "10km")
      ),
      names = "buffer_size",
      tar_target(
        yearly_hii_buffer,
        community_coordinates_in_batches %>%
          head(n = 2) %>%
          summarise(n = n(), nc = n_distinct(COMMUNITY_ID)) %>%
          mutate(
            hii_buffer = buffer_size
          ),
        pattern = map(community_coordinates_in_batches),
        resources = tar_resources( # pass the heavy-duty crew controller
          crew = tar_resources_crew(controller = controller_group$names$heavy)
        )
      )
    )
  )

  list(
    geography,
    landscape_complexity,
    human_impact
  )
}
