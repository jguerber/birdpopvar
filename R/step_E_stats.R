step_stats <- function(parameters) {
  run_stats <- list(
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
          across( # don't scale covariates because piecewiseSEM scales within communities later,
            # but scale coordinates for nlme corExp object
            c(lon, lat), ~ scale(.x)
          ),
          across(c(sd_r_average, abs_trend_average), log, .names = "{.col}_log"),
          HABITAT_GROUP = factor(HABITAT_GROUP, levels = c("woodland", "farmland", "built"))
      )
    ),
    tar_target(
      spatial_sem_output,
      data_for_spatial_sem %>%
        run_spatial_sems,
      packages = c("piecewiseSEM", "nlme")
    ),
    tar_target( # Supplementary 1 : fit models for fig2 with a sdmTMB spatial random intercept
      spatial_models_across_habitats,
      data_for_models_across_habitats %>%
        mutate(
          across(c(sd_r_average, abs_trend_average), log, .names = "{.col}_log")
        ) %>%
        run_spatial_models(
          responses = c("sd_r_average_log", "abs_trend_average_log"),
          fixed_effects = fixed_effects_list("HABITAT_GROUP"),
          spde_cutoff = 10,
          random_effects = "+ (1|SITE2)"
        ),
      packages = c("sdmTMB")
    ),
    tar_map(
      values = tibble(
        model_name = c("full", "habitat_controlled")
      ),
      names = "model_name",
      tar_target(
        residuals_spatial_models,
        spatial_models_across_habitats %>%
          simulate_residuals_from_spatial_model(model_name),
        packages = c(default_dependencies(), "DHARMa", "sdmTMB")
      ),
      tar_target(
        estimates_spatial_models,
        spatial_models_across_habitats %>%
          extract_estimates_from_spatial_model(model_name),
        packages = c(default_dependencies(), "sdmTMB")
      )
    ),
    tar_map( # Supplementary 2 : re-run SEMs, but add HII in buffer as impact variable
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
    ),
    tar_group_by(
      mc_replicate_batches,
      tibble::tibble(
        # rep_id = 1:10000, # ~ 10000 reps take around 15 min on 32 workers
        # batch = rep(1:100, each = 100)
        rep_id = 1:10, # for tests
        batch = rep(1:2, each = 5) # for tests
      ),
      batch
    ),
    tar_target(
      mc_meanabstrend_samples, # grouped by rep, batched in groups of replicates
      draw_mc_meanabstrend(
        all_local_trends_outputs,
        mc_replicate_batches$rep_id
      ),
      pattern = map(mc_replicate_batches),
      resources = tar_resources( # select the heavy-duty crew controller
        crew = tar_resources_crew(controller_group$names$heavy)
      ),
      packages = c(default_dependencies(), "glmmTMB", "broom.mixed")
    ),
    tar_target(
      mc_stats1_estimates,
      stats_fun_on_mc(
        mc_meanabstrend_samples,
        population_variability_data,
        stats_fun = single_rep_stats1
      ),
      pattern = map(mc_meanabstrend_samples),
      resources = tar_resources( # select the heavy-duty crew controller
        crew = tar_resources_crew(controller_group$names$heavy)
      ),
      packages = c(default_dependencies(), "glmmTMB", "broom.mixed")
    )
  )

  mapped_summaries <- tar_map(
    values = tibble(
      # N_replicates = c(100, 500, 1000, 2500, 5000, 10000)
      N_replicates = c(2,4, 10) # for tests
    ),
    names = "N_replicates",
    unlist = FALSE,
    tar_target(
      mc_summaries_meanabstrend,
      mc_meanabstrend_samples %>% 
        group_by(COMMUNITY_ID) %>% 
        summarise_mc_vars(
          "mean_abs_trend_mc",
          R = N_replicates
        )
    ),
    tar_target(
      mc_summaries_stats1,
      mc_stats1_estimates %>% 
        filter(!is.na(term), term != "sd__(Intercept)") %>% 
        group_by(term, model) %>% 
        summarise_mc_vars(
          vars = c("estimate", "std.error"),
          R = N_replicates
        )
      )
  )

  # all_mc_summaries <- tar_combine(
  #   mc_summary_all,
  #   mapped_summaries[["mc_summary"]],
  #   command = bind_rows(!!!.x, .id = "N_replicates")
  # )

  simulate_examples <- list(
      tar_target(
        example_communities_models,
        generate_example_communities(seed = 1876) %>%
          measure_var_trend()
      ),
      tar_target(
        table_summary,
        example_communities_models %>%
          summarise_metrics() %>%
          build_summary_table()
      )
    )

  list(
    run_stats,
    mapped_summaries,
    # all_mc_summaries,
    simulate_examples
  )
}
