fixed_effects_list <- function(habitat_var = "HABITAT_GROUP") {
  list(
    full = paste0(habitat_var, " + N_POINTS_avg + HII_focal + H_fine + mu_SR"),
    no_npoints = paste0(habitat_var, " + HII_focal + H_fine + mu_SR"),
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
