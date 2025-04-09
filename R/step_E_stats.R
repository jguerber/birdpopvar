step_stats <- function(parameters) {
  list(
    tar_target( # Stats for fig 2 : all communities, scale covariates
      data_for_models_across_habitats,
      population_variability_data %>%
        split_community_id %>%
        mutate(
          across(
            c(HII_focal, H_fine, N_POINTS_avg, mu_SR), ~ scale(.x)
          ),
          HABITAT_GROUP = factor(HABITAT_GROUP, levels = c("woodland", "farmland", "built"))
        )
    ),
    tar_target( # Run comparisons across habitats with glmmTMB
      models_stab_across_habitats,
      calibrate_and_run_models(
        data_for_models_across_habitats,
        fixed_effects_list("HABITAT_GROUP"),
        responses = c("sd_r_average", "abs_trend_average"),
        families = c("lognormal", "lognormal"),
        full_model = "full"
      ),
      packages = c("glmmTMB")
    ),
    tar_target(
      data_for_spatial_sem, # Data for SEM : data will be split by habitat group
      population_variability_data %>%
        split_community_id %>%
        mutate(
          across(
            c(lon, lat), ~ scale(.x)
          ),
          across(c(sd_r_average, abs_trend_average), log, .names = "{.col}_log"),
          HABITAT_GROUP = factor(HABITAT_GROUP, levels = c("woodland", "farmland", "built"))
      )
    ),
    tar_target( # Supplementary 1 : re-run SEMs, but add HII in buffer as impact variable
      spatial_sem_output,
      data_for_spatial_sem %>%
        run_spatial_sems,
      packages = c("piecewiseSEM", "nlme")
    ),
    tar_map(
      values = tibble(
        buffer_size = c("5km", "10km", "25km"),
        hii_colname = paste0("HII_", buffer_size)
      ),
      names = "buffer_size",
      tar_target(
        data_for_spatial_sem_buffer,
        data_for_spatial_sem %>%
          convert_hii_raw(hii_colname)
      ),
      tar_target(
        spatial_sem_buffer_output,
        data_for_spatial_sem_buffer %>%
          run_spatial_sems(fun_runsem = spatial_psem_buffered),
        packages = c(default_dependencies(), c("piecewiseSEM", "nlme"))
      )
    )
  )
}
