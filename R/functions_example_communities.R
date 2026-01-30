generate_species_notrend <- function(sd, alpha, nyears = 25) {
  # means on the log scale
  mu_log <- rnorm(1:nyears, mean = alpha, sd = sd)

  data.frame(
    y = unlist(map(mu_log, \(m) rpois(1, lambda = exp(m)))),
    t = 1:nyears,
    l = exp(mu_log)
  )
}

generate_species_trend <- function( sd, beta, alpha, nyears = 25, seed = 2025) {

  set.seed(seed)

  t_range = 1:nyears
  t_scale = scale(t_range, scale = F)

  # means on the log scale
  mu_log <- rnorm(t_range, mean = 0, sd = sd) + beta*t_scale + alpha

  data.frame(
    y = unlist(map(mu_log, \(m) rpois(1, lambda = exp(m)))),
    t = t_range,
    l = exp(mu_log)
  )
}

reorder_series <- function(p, df) {
  new_t <- df %>%
    pull(t) %>%
    sort(decreasing = (p$direction == -1))

  df %>%
    arrange(y) %>%
    mutate(
      t = new_t
    )
}

generate_example_communities <- function(seed = 1876) {
  set.seed(seed)

  com_notrend <- data.frame(
    species = c("A", "B", "C"),
    sd = c(0.01, 0.02, 0.1),
    alpha = c(3.1, 2.5, 3.2)
  ) %>%
    group_by(species) %>%
    nest(.key = "params") %>%
    mutate(
      sp_data = map(params, \(s) generate_species_notrend(s$sd, s$alpha, 25))
    ) %>%
    unnest(c(params, sp_data)) %>%
    mutate(
      comm = "untrended"
    )

  com_trends <- com_notrend %>%
    mutate(
      direction = case_match(
        species,
        "A" ~ -1,
        "B" ~ 1,
        "C" ~ -1
      )
    ) %>%
    group_by(species, sd, direction) %>%
    nest(.key = "sp_data") %>%
    group_by(species, sp_data) %>%
    nest(.key = "params") %>%
    mutate(
      sorted_data = map2(params, sp_data, reorder_series)
    ) %>%
    ungroup %>%
    select(-c(sp_data, params)) %>%
    unnest(sorted_data) %>%
    mutate(
      comm = "trended"
    )

  bind_rows(
    com_notrend,
    com_trends
  )
}

measure_var_trend <- function(data) {
  data %>%
    group_by(comm, species) %>%
    nest %>%
    mutate(
      model = map(data, \(d) glmmTMB::glmmTMB(y ~ t, data = d, family = "poisson")),
      coefs = map(model, broom.mixed::tidy),
      beta = map(coefs, \(c) c %>% filter(term == "t") %>% pull(estimate)),
      residuals = map(model, \(m) residuals(m, type = "pearson")),
      fit = map(model, \(m) predict(m, type = "response")),
      sdr = map(residuals, \(r) sd(r)),
      cv = map(data, \(d) sd(d$y)/mean(d$y)),
      mu = map(data, \(d) mean(d$y))
    )
}

summarise_metrics <- function(model_data) {
  model_data %>%
    select(-c(data, model, coefs, residuals)) %>%
    unnest(c(beta, sdr, cv, mu)) %>%
    group_by(comm) %>%
    summarise(
      abstrend = mean(abs(beta)),
      sdr = mean(sdr),
      cv_pop = mean(cv),
      cv_w = mean(cv * mu / sum(mu))
    )
}

build_summary_table <- function(metrics_summary) {
  metrics_summary %>%
    select(-cv_pop) %>%
    pivot_longer(cols = -comm, names_to = "metric", values_to = "value") %>%
    pivot_wider(names_from = comm, values_from = value) %>%
    mutate(
      metric = factor(metric, levels = c("cv_w", "abstrend", "sdr"))
    ) %>%
    arrange(metric) %>%
    mutate(
      across(-metric, ~ as.character(signif(.x, digits = 2)))
    ) %>%
    mutate(
      metric = case_match(
        as.character(metric),
        "cv_w" ~ "Weighted average\npopulation variability",
        "abstrend" ~ "Average of\nabsolute trends",
        "sdr" ~ "Average detrended\npopulation variability"
      )
    ) %>%
    tibble::column_to_rownames("metric")
}

simulate_null_abstrend <- function(N_com, mean_sr_log, sd_sr_log, sd_trend) {
  tibble::tibble(
      com_id = 1:N_com,
      SR = round(exp(rnorm(N_com, mean = mean_sr_log, sd = sd_sr_log)))
  ) %>% 
      group_by(com_id, SR) %>% 
      reframe(
          id_sp = 1:SR,
          trend = rnorm(n = SR, mean = 0, sd = sd_trend)
      )
}

