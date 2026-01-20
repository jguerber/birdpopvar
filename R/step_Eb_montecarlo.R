step_montecarlo_stats <- function(params) {
  run_replicates <- list(
    tar_group_by(
      mc_replicate_batches,
      tibble::tibble(
        rep_id = 1:10000, 
        batch = rep(1:1000, each = 10)
        # rep_id = 1:10, # for tests
        # batch = rep(1:5, each = 2) # for tests
      ),
      batch
    ),
    tar_target(
      mc_meanabstrend_samples, # grouped by rep, batched in groups of replicates
      draw_mc_meanabstrend(
        all_local_trends_outputs,
        mc_replicate_batches$rep_id,
        communities = population_variability_data$COMMUNITY_ID
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
    ),
    tar_target(
      mc_sem_estimates,
      stats_fun_on_mc(
        mc_meanabstrend_samples,
        population_variability_data,
        stats_fun = single_rep_sem
      ),
      pattern = map(mc_meanabstrend_samples),
      resources = tar_resources( # select the heavy-duty crew controller
        crew = tar_resources_crew(controller_group$names$heavy)
      ),
      packages = c(default_dependencies(), "piecewiseSEM", "nlme")
    )
  )

  mapped_summaries <- tar_map(
    values = tibble(
      N_replicates = c(100, 500, 1000, 2500, 5000, 10000)
      # N_replicates = c(2,4, 10) # for tests
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
        filter(!is.na(term), term != "sd__(Intercept)") %>% # remove models that did not run properly
        group_by(term, model) %>% 
        summarise_mc_vars(
          vars = c("estimate", "std.error"),
          R = N_replicates
        )
    ),
    tar_target(
      mc_summaries_sem,
      mc_sem_estimates %>%
        filter(Response != "Fisher.C") %>% 
        mutate(
            relative_sd = Std.Estimate / Estimate,
            error_on_std_scale = Std.Error * relative_sd
        ) %>%
        select(Response, Predictor, HABITAT, rep_id, Std.Estimate, error_on_std_scale) %>% 
        group_by(HABITAT, rep_id) %>%  # reject models for which at least one path is weird
        mutate(
            n_problems = sum(Std.Estimate == 0) + sum(error_on_std_scale == 0) + sum(is.na(Std.Estimate)) + sum(is.na(error_on_std_scale)) + sum(is.infinite(error_on_std_scale))
        ) %>% 
        filter(n_problems == 0) %>% 
        select(-n_problems) %>% 
        group_by(HABITAT, Response, Predictor) %>% # extract estimate and MC error for coef and error
        summarise_mc_vars(vars = c("Std.Estimate", "error_on_std_scale"), R = N_replicates)
    ),
    tar_target(
      mc_summaries_chisq_sem,
      mc_sem_estimates %>% 
        filter(Response == "Fisher.C") %>%
        group_by(HABITAT) %>% 
        summarise_mc_vars(
            vars = c("Std.Estimate"),
            R = N_replicates
        )
    )
  )

  save_outputs <- list(
    tar_target(
      mc_summaries_sem_10000_file,
      write_path(
        mc_summaries_sem_10000,
        rel_path = "data/processed/mc_summaries_sem_10000.csv",
        row.names = F
      )
    ),
    tar_target(
      mc_summaries_stats1_10000_file,
      write_path(
        mc_summaries_stats1_10000,
        rel_path = "data/processed/mc_summaries_stats1_10000.csv",
        row.names = F
      )
    ),
    tar_target(
      mc_summaries_meanabstrend_10000_file,
      write_path(
        mc_summaries_meanabstrend_10000,
        rel_path = "data/processed/mc_summaries_meanabstrend_10000.csv",
        row.names = F
      )
    ),
    tar_target(
      mc_summaries_chisq_sem_10000_file,
      write_path(
        mc_summaries_chisq_sem_10000,
        rel_path = "data/processed/mc_summaries_chisq_sem_10000.csv",
        row.names = F
      )
    )
  )

  list(
    run_replicates,
    mapped_summaries,
    save_outputs
  )
}