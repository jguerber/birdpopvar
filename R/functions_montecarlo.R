#' Draw `length(reps)` repetitions of species trends around their estimated
#' standard deviations, and summarise as `mean(abs(trend))`
draw_mc_meanabstrend <- function(species_trends, reps, communities = NULL) {

  if (!("COMMUNITY_ID" %in% colnames(species_trends))) {
    species_trends <- species_trends %>% 
      split_series_id %>% 
      ungroup
  }

  if (!is.null(communities)) {
    species_trends <- species_trends %>% 
      filter(COMMUNITY_ID %in% communities)
  }
    
  map(reps, function(r) {
    species_trends %>% 
      mutate(
          trend_mc = rnorm(n = n(), mean = trend, sd = se)
      ) %>% 
      group_by(COMMUNITY_ID) %>% 
      summarise(
          mean_abs_trend_mc = mean(abs(trend_mc))
      ) %>% 
      mutate(
        rep_id = r
      )
  }) %>% 
    bind_rows()
} 

#' run a function on a MC samples dataframe
#' 
#' Used to generate MC distributions of statistics on the resampled dataframes.
#' 
#' @param stats_fun A function that takes three arguments:
#' `.x`, a dataframe that corresponds to a single MC-resampled dataframe
#' `covariates`, community-level covariates dataframe
#' `.r`, the current id of MC resampling.
#' Make sure that, if it modifies `.x`, `stats_fun` add the information in `.r`
#' somewhere in its output
#' 
#' The default `stats_fun` passes `.x` through without summarising
stats_fun_on_mc <- function(samples, covariates, stats_fun = \(.x,.c,.r) .x) {
    
  samples %>% 
    group_by(rep_id) %>% 
    group_map(
      ~ stats_fun(.x, covariates, .y$rep_id)
    ) %>% 
    bind_rows()

}

single_rep_stats1 <- function(sample, covariates, .r) {
  dat <- sample %>% 
        split_community_id() %>% 
        left_join(covariates, by = "COMMUNITY_ID") %>% 
        mutate(
            across(
            c(HII_focal, H_fine, N_POINTS_avg, mu_SR), ~ scale(.x)
            ),
            HABITAT_GROUP = factor(HABITAT_GROUP, levels = c("woodland", "farmland", "built"))
        ) 

  mod_hc <- safe_model_call(
    form = mean_abs_trend_mc ~HABITAT_GROUP + N_POINTS_avg + (1|SITE2),
    data = dat,
    family= "lognormal"
  ) %>% 
  mutate(
      model = "habitat_controlled"
  )

  mod_full <- safe_model_call(
    form = mean_abs_trend_mc ~HABITAT_GROUP + H_fine + HII_focal + mu_SR + N_POINTS_avg + (1|SITE2),
    data = dat,
    family = "lognormal"
  ) %>% 
  mutate(
      model = "full"
  )

  bind_rows(
      mod_full,
      mod_hc
  ) %>% 
    mutate(
      rep_id = .r
    )
}

single_rep_sem <- function(sample, covariates, .r) {
    dat <- sample %>% 
      split_community_id() %>% 
      left_join(covariates, by = "COMMUNITY_ID") %>% 
      mutate(
        across( # don't scale covariates because piecewiseSEM scales within communities later,
          # but scale coordinates for nlme corExp object
          c(X, Y), ~ scale(.x)
        ),
        across(c(sd_r_average), log, .names = "{.col}_log"),
        HABITAT_GROUP = factor(HABITAT_GROUP, levels = c("woodland", "farmland", "built")),
        ata_log = log(mean_abs_trend_mc)
    )

    run_spatial_sems(sem_data = dat, fun_runsem = spatial_psem_call_mc) %>% 
      extract_sem_coefficients(c("woodland", "farmland", "built")) %>% 
      mutate(
        rep_id = .r
      )
}

extract_sem_coefficients <- function(wrapper, categories, warnings_tol = 0) {

  n_errors <- sapply(categories, function(c) { # fetch numbers of errors and warnings from SEM outputs
    length(wrapper[[c]]$error) 
  })

  n_warnings <- sapply(categories, function(c) { # fetch numbers of errors and warnings from SEM outputs
    length(wrapper[[c]]$warnings) 
  })

  dat_out <- tibble::tibble(
    category = categories,
    n_errors = n_errors,
    n_warnings = n_warnings
  )

  categories_ok <- dat_out %>% 
    filter(n_errors == 0 & n_warnings <= warnings_tol) %>% 
    pull(category)

  if (length(categories_ok) == 0) {
    return(tibble::tibble(
      Std.Estimate = NA
    ))
  }

  fit_info <- extract_sem_fit_info(map(wrapper, \(x) x$value), categories_ok) %>% 
    rename(
      Std.Estimate = Fisher.C,
      DF = df,
      HABITAT = category
    ) %>% select(-label_clean, -N) %>% 
    mutate(
      Response = "Fisher.C",
      Predictor = "Fisher.C"
    )

  categories_ok %>% 
    set_names() %>% 
    map(function(c) {
      wrapper[[c]]$value$summary_out$coefficients %>% 
        data.frame %>% 
        mutate(
          HABITAT = c
        )
    }) %>% 
    bind_rows() %>%  # rejoin the different habitat categories for SEM coefficients
    bind_rows(., fit_info) # and join with the fit info df
}

safe_model_call <- function(data, form, ...) {
  obj <- catchConditions({
    glmmTMB::glmmTMB(
      formula = form,
      data = data,
      ...
    )
  })

  if ((length(obj$warnings) > 0) || (length(obj$error) > 0)) {
    return(tibble::tibble(estimate = NA))
  }

  obj$value %>% 
    broom.mixed::tidy()
}

#' From a grouped samples dataframe, compute summary stats for the resampled distribution of vars
#' in each group
summarise_mc_vars <- function(mc_samples, vars, R) {
  rep_ids <- unique(mc_samples$rep_id)
  ids_current <- sample(rep_ids, size = min(R, length(rep_ids)), replace = F)

  mc_samples %>% 
    filter(rep_id %in% ids_current) %>% 
    pivot_longer(cols = vars, names_to = "variable", values_to = "mc_output") %>% 
    group_by(variable, .add = T) %>% # add variable as a grouping variable
    summarise(
        med = median(mc_output),
        lwr = quantile(mc_output, 0.025),
        upr = quantile(mc_output, 0.975),
        sd = sd(mc_output),
        mean = mean(mc_output),
        n_replicates_ok = n(),
        .groups = "drop"
    ) %>% 
    mutate(
        N_replicates = R
    )
}