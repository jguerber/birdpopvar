# Community-level data
#
# From all different data sources, collect relevant information for each community
#  - geographical coordinates from sampling information
#  - landscape complexity from Corine Land Cover
#  - human impact data from HII rasters
#  - average stability metrics from results of step C

step_community_data <- function(parameters) {
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

  list(
    landscape_complexity
  )
}
