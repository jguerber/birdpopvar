# Community-level data
#
# From all different data sources, collect relevant information for each community
#  - geographical coordinates from sampling information
#  - landscape complexity from Corine Land Cover
#  - human impact data from HII rasters
#  - average stability metrics from results of step C
#  - sampling and survey covariates (species richness and number of listening points)

step_community_data <- function(parameters) {
  geography <- list(
    tar_target( # a single pair of coordinates for each community
      community_coordinates, # X/Y in meters
      communities_points %>%
        select(-YEAR) %>%
        unique %>%
        group_by(COMMUNITY_ID) %>%
        centroid_coordinates(
          coords = c("lon1", "lat1")
        )
    ),
    tar_target(
      community_coordinates_file,
      write_path(
        community_coordinates,
        rel_path = "data/processed/community_coordinates.csv"
      )
    )
  )

  landscape_complexity <- tar_map(
      values = tibble(
        buffer_size = c(250, 500)
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

  human_impact_sequential <- list(
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
    )
  )

  human_impact_mapped <- tar_map(
      values = tibble(
        buffer_size = c("5km", "10km", "25km")
      ),
      names = "buffer_size",
      unlist = F,
      tar_target(
        yearly_hii_buffer,
        community_coordinates_in_batches %>%
          build_yearly_hii_buffer(
            stack_from_repo(
              hii_proxy_relative_path, as_proxy = T
            ),
            .,
            crs_epsg = 4326,
            buffer_size = buffer_size
          ),
        pattern = map(community_coordinates_in_batches),
        resources = tar_resources( # pass the heavy-duty crew controller
          crew = tar_resources_crew(controller = controller_group$names$heavy)
        ),
        packages = c(default_dependencies(), "sf", "stars")
      )
    )

  human_impact_combined <- tar_combine(
    yearly_hii_all_buffers,
    human_impact_mapped[["yearly_hii_buffer"]],
    command = dplyr::bind_rows(!!!.x)
  )

  human_impact_summaries <- list(
    tar_target(
      all_yearly_hii,
      bind_rows(
        yearly_hii_focal,
        yearly_hii_all_buffers
      )
    ),
    tar_target(
      human_impact,
      all_yearly_hii %>%
        mutate( # the HII protocol change in 2015 really impact French values,
          # keep the two values separated
          hii_version = ifelse(YEAR < 2015, "v1", "v1b")
        ) %>%
        group_by(COMMUNITY_ID, buffer, hii_version, YEAR) %>%
        summarise(HII_com = mean(HII, na.rm = F), .groups = "drop_last") %>%
        summarise(HII_mu = mean(HII_com, na.rm = F)) %>%
        pivot_wider(names_from = buffer, values_from = HII_mu, names_prefix = "HII_")
    )
  )

  survey_covariates <- list(
    tar_target(
      survey_information,
      aggregate_survey %>%
        group_by(COMMUNITY_ID, YEAR) %>%
        summarise( # for each community each year, number of present species and
          # sampling effort (number of listening points)
          SR = sum(AB_SUM > 0),
          N_POINTS = unique(N_POINTS),
          .groups = "drop_last"
        ) %>% # across years : for each community, average richness and average sampling effort
        summarise(
          mu_SR = mean(SR),
          N_POINTS_avg = mean(N_POINTS),
          N_YEARS = n_distinct(YEAR),
          .groups = "drop"
        )
    )
  )

  stability_metrics <- list(
    tar_target(
      stability_metrics,
      all_local_trends_outputs %>%
        filter(
          model_name == "poisson",
          residual_type == "pearson",
          convProblems <= threshold_warnings
        ) %>%
        split_series_id %>%
        group_by(COMMUNITY_ID) %>%
        mutate(
          abs_trend = abs(trend)
        ) %>%
        summarise(
          across(
            c(sd_r, abs_trend),
            ~ mean(.x),
            .names = "{.col}_average"
          ),
          N_series = n_distinct(series_id) # this is NOT the species richness
        )
    )
  )

  join_everything <- list(
    tar_target(
      all_community_data,
      stability_metrics %>%
        left_join(
          community_coordinates, by = "COMMUNITY_ID"
        ) %>%
        left_join(
          landscape_complexity_250, by = "COMMUNITY_ID"
        ) %>%
        left_join(
          filter(human_impact, hii_version == "v1"), by = "COMMUNITY_ID"
        ) %>%
        left_join(
          survey_information, by = "COMMUNITY_ID"
        ) %>%
        select(
          -c(hii_version)
        )
    ),
    tar_target(
      population_variability_data,
      all_community_data %>%
        filter(rowSums(is.na(.)) == 0)
    ),
    tar_target(
      population_variability_data_file,
      write_path(
        population_variability_data,
        rel_path = "data/processed/bird_population_variability.csv",
        row.names = F
      ),
      format = "file"
    )
  )

  list(
    geography,
    landscape_complexity,
    human_impact_sequential, # first target group of single operation per community
    human_impact_mapped, # static branching over buffer sizes
    human_impact_combined, # aggregate static branches in the same dataframe
    human_impact_summaries, # summarise metrics from the (big) yearly_hii_all_buffers target
    survey_covariates,
    stability_metrics,
    join_everything
  )
}
