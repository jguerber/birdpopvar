# length(reps) iterations
draw_mc_samples <- function(species_trends, covariates, reps) {
  # first filter only trends that are in communities in covariates
  species_trends <- species_trends %>%
    split_series_id %>% 
    filter(COMMUNITY_ID %in% covariates$COMMUNITY_ID)
  map(reps, function(r) {
    mc_iterate(
        species_trends,
        covariates,
        .r = r
    )
  }) %>% bind_rows()
}

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

stats_fun_on_mc <- function(samples, covariates, stats_fun = identity) {
    
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

extract_sem_coefficients <- function(wrapper, categories) {

  n_errors <- sapply(categories, function(c) { # fetch numbers of errors and warnings from SEM outputs
    length(wrapper[[c]]$error) + length(wrapper[[c]]$warnings)
  })

  dat_out <- tibble::tibble(
    category = categories,
    n_errors = n_errors
  )

  categories_ok <- dat_out %>% 
    filter(n_errors == 0) %>% 
    pull(category)

  if (length(categories == 0)) {
    return(tibble::tibble(
      estimate = head(wrapper[[1]]$error, n = 1)
    ))
  }

  fit_info <- extract_sem_fit_info(map(wrapper, \(x) x$value), categories_ok) %>% 
    rename(
      estimate = Fisher.C,
      Response = N,
      DF = df,
      HABITAT = category
    ) %>% select(-label_clean)

  categories_ok %>% 
    set_names() %>% 
    map(function(c) {
      wrapper[[c]]$value$summary_out$coefficients %>% 
        as.data.frame %>% 
        mutate(
          HABITAT = c
        )
    }) %>% 
    bind_rows(fit_info)
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
  ids_current <- sample(unique(mc_samples$rep_id), size = R, replace = F)

  mc_samples %>% 
    filter(rep_id %in% ids_current) %>% 
    pivot_longer(cols = vars, names_to = "variable", values_to = "mc_output") %>% 
    group_by(variable, add = T) %>% # add variable as a grouping variable
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

# after sampling, summarise over all iterations
summarise_mc <- function(mc_samples, R) {
  ids_current <- sample(unique(mc_samples$rep), size = R, replace = F)

  mc_samples %>% 
    filter(rep %in% ids_current) %>% 
    group_by(rep) %>% 
    mutate(
      n_errors = sum(is.na(estimate)) + sum(is.na(std.error))
    ) %>% 
    filter(n_errors == 1) %>% # 1 error is allowed:  no std.error for sd_Intercept
    select(-n_errors) %>% 
    ungroup() %>% 
    pivot_longer(
        cols = c(estimate, std.error),
        names_to = "component",
        values_to = "mc_output"
    ) %>% 
    filter(!(component == "std.error" & term == "sd__(Intercept)")) %>% # no std.error for sd_Intercept
    group_by(effect, term, group, component) %>% 
    summarise(
        med = median(mc_output),
        lwr = quantile(mc_output, 0.025),
        upr = quantile(mc_output, 0.975),
        se = sd(mc_output)/sqrt(n()),
        mean = mean(mc_output),
        n_models_ok = n(),
        .groups = "drop"
    ) %>% 
    mutate(
        N_replicates = R
    )
}