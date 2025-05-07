fixed_effects_list <- function(habitat_var = "HABITAT_GROUP") {
  list(
    full = paste0(habitat_var, " + N_POINTS_avg + HII_focal + H_fine + mu_SR"),
    habitat_controlled = paste0(habitat_var, "+ N_POINTS_avg"),
    interacting = paste0(habitat_var, "* (HII_focal + mu_SR + H_fine) + N_POINTS_avg"),
    no_landscape = paste0(habitat_var, " + mu_SR + N_POINTS_avg"),
    no_habitat = "N_POINTS_avg + HII_focal + H_fine + mu_SR",
    no_ecology = "N_POINTS_avg",
    null = "1"
  )
}

#' infer correct residual structure with REML, then run specified in fixed_effects list
#'
#' @param responses response variables
#' @param families family to use for each response (same length as `responses`)
calibrate_and_run_models <- function(
    data_model,
    fixed_effects,
    responses,
    families,
    full_model = "npoints") {
  if (nrow(data_model) == 0) return(list())

  resid <- responses %>%
    set_names %>%
    map(function(m) {
      data_model %>% check_residual_structure(
        fixed_formula = paste0(m, " ~ ", fixed_effects[[full_model]])
      )
    })

  models <- pmap(
    list(
      met = responses,
      res = resid,
      fam = families
    ),
    function(met, res, fam) {
      flist <- build_formulas(met, fixed_effects, suffix = res$formulas_suffix)

      mods <- list()
      mods <- data_model %>%
        glm_wrap(
          flist,
          mods,
          refit = F,
          data = .,
          family = fam
        )
      return(mods)
    }
  ) %>% set_names(responses)

  return(list(models = models))
}


run_spatial_models <- function(
    data_model,
    responses = c("sd_r_average_log", "abs_trend_average_log"),
    fixed_effects = fixed_effects_list("category"),
    family = gaussian(link = "identity"),
    spde_cutoff = 10,
    random_effects = "",
    quiet = T
) {
  data_model_spat <- data_model %>% # coordinates to km
    mutate(
      across(c(X, Y), ~ .x / 1000)
    )

  if (str_detect(random_effects, "SITE2")) {
    data_model_spat <- data_model_spat %>%
      mutate(SITE2 = as.factor(SITE2))
  }

  mesh <- data_model_spat %>%
    make_mesh(xy_cols = c("X", "Y"), cutoff = spde_cutoff)

  models <- responses %>%
    set_names %>%
    map(function(r) {
      if (!quiet) message("Response ", r)
      formulas <- build_formulas(r, fixed_effects, suffix = random_effects)

      formulas %>% map(function(f) {
        if (!quiet) message("formula ", f)
        sdmTMB::sdmTMB(
          formula = as.formula(f),
          family = family,
          mesh = mesh,
          spatial = "on",
          data = data_model_spat
        )
      })
    })

  return(list(models = models))
}

simulate_residuals_from_spatial_model <- function(out_spatial) {
  c(variability = "sd_r_average_log", trend = "abs_trend_average_log") %>%
    map(function(r) {
      out_spatial$models[[r]]$full %>%
        simulate(nsim = 2000, type = "mle-mvn", re_form = NULL) %>%
        dharma_residuals(out_spatial$models[[r]]$full, return_DHARMa = T)
    })
}

extract_estimates_from_spatial_model <- function(out_spatial) {
  c(variability = "sd_r_average_log", trend = "abs_trend_average_log") %>%
    map(function(r) {
      tidy(
        out_spatial$models[[r]]$full,
        component = "cond",
        effects = "fixed",
        conf.int = T
      ) %>%
        mutate(response = r)
    }) %>% do.call(bind_rows, .)
}
