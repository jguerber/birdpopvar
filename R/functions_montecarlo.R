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

# single iteration : redraw all species trends in their estimated interval and fit model
mc_iterate <- function(dat, covariates, .r = NA, try_max = 3) {
  
  errors <- 1
  tries <- 0

  while (errors > 0 && tries < try_max) {

    dat_s <- dat %>% 
      mutate(
          trend_mc = rnorm(n = n(), mean = trend, sd = se)
      ) %>% 
      group_by(COMMUNITY_ID) %>% 
      summarise(
          abs_trend_average_mc = mean(abs(trend_mc))
      ) %>% 
      left_join(covariates, by = "COMMUNITY_ID") 
  
    model <- catchConditions({glmmTMB::glmmTMB(
      formula = abs_trend_average_mc ~HABITAT_GROUP + N_POINTS_avg + (1|SITE2),
      family = "lognormal",
      data = dat_s
    )})

    errors <- length(model$error)
    tries <- tries + 1

  }

  # after while loop : either tries == try_max and model may be ok, or model succeeded

  if (tries == try_max) {
    if (errors > 0) {
      stop(glue::glue("Model failed with {{ tries }} attempts"))
    }
  }

  model$value %>% 
        broom.mixed::tidy() %>% 
        select(effect, term, group, estimate, std.error) %>% 
        mutate(
            rep = .r
        )
}

# after sampling, summarise over all iterations
summarise_mc <- function(mc_samples, R) {
  ids_current <- sample(unique(mc_samples$rep), size = R, replace = F)

  mc_samples %>% 
    filter(rep %in% ids_current, !is.na(estimate), !is.na(std.error)) %>% 
    pivot_longer(
        cols = c(estimate, std.error),
        names_to = "component",
        values_to = "mc_output"
    ) %>% 
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