
glm_wrap <- function(formula_list, models, refit, ...) {
  if (!refit) {
    required_names <- names(formula_list)
    old_names <- names(models)

    # remove the old ones that are not required
    to_remove = old_names[which(!(old_names %in% required_names))]
    to_keep = old_names[which(!(old_names %in% to_remove))]

    # required names that are not in old names will have to be refit
    to_fit = required_names[which(!(required_names %in% old_names))]

    new_models = glm_list(formula_list[to_fit], ...)

    return(c(new_models, models[to_keep]))
  }

  glm_list(
    formula_list,
    ...
  )
}


build_formulas <- function(response, base_list, suffix = "") {
  #  paste response
  base_structure <- map(base_list, \(b) paste(response, b, sep = " ~ "))

  map(
    base_structure, \(f) paste0(f, suffix)
  )
}



check_residual_structure <- function(
    df,
    fixed_formula = "sd_r_average_log ~ hii_mu + mean_ab_average_log+ hii_beta + mu_SR",
    test_structures = list(
      site_re = "+ (1|SITE2)"
    ),
    quiet = T
) {
  flist <- map(test_structures, \(f) paste0(fixed_formula, f))

  if (!("no_re" %in% names(flist))) {
    flist <- append(flist, list(no_re = fixed_formula))
  }

  mods_re <- list()
  mods_re <- df %>%
    glm_wrap(
      flist,
      mods_re,
      refit = F,
      REML = T,
      data = .
    )

  mod_comparison <- anova_list(mods_re) %>%
    as.data.frame %>%
    arrange(AIC) %>%
    rename(
      pval = `Pr(>Chisq)`
    )

  best_mod <- mod_comparison %>%
    slice_best_model %>%
    rownames

  if (best_mod == "no_re") {
    txt_retype <- "no random effects"
    formulas_suffix <- ""
    model_function <- lm
    args_sup <- function(data){list(
      data = data
    )}
  } else if (best_mod == "site_re") {
    txt_retype <- "a square-specific random intercept"
    formulas_suffix <- "+ (1|SITE2)"
    model_function <- glmmTMB::glmmTMB
    args_sup <- function(data) {list(
      data = data,
      family = "gaussian"
    )}
  }

  if (!quiet) message("Best REML model includes ", txt_retype)

  return(list(
    txt = txt_retype,
    formulas_suffix = formulas_suffix,
    model_function = model_function,
    args_sup = args_sup
  ))
}

glm_list <- function(formulalist, ...) {
  lapply(formulalist, function(f) {
    glmmTMB(
      formula = as.formula(f),
      ...
    )
  })
}

anova_list <- function(model_list) {
  with(model_list, do.call(anova, lapply(names(model_list), as.name)))
}

AIC_list <- function(model_list) {
  with(model_list, do.call(AIC, map(names(model_list), as.name)))
}


#' slice the best model of a df sorted by ascending information criterion
slice_best_model <- function(df) {
  if (is.na(df$pval[1]) || nrow(df) == 1) {
    return(df %>% slice(1))
  }

  if (df$pval[1] < 0.05) {
    return(df %>% slice(1))
  } else {
    return(slice_best_model(df[-1,]))
  }
}

#### visreg : Extract partial residuals from (re) fitted models ####


#' Refit a model and compute visreg residuals
#'
#' looks like visreg or glmmtmb need formula and data to be defined in the environment
#' to be able to call the correct predict() methods. Quick fix : refit the model
#'
#' @param ... parameters to pass to [visreg::visreg]
#' @param model fitted model to refit
#' @param xvar providing a character vector will return a list of visreg outputs
#'
#' @examples
#' \dontrun{
#' extract_visreg_residuals(fit, xvar = c("x1", "x2"), partial = T, plot = F)
#' }
extract_visreg_residuals <- function(model, xvar, ...) {
  form = model$call$formula
  original_frame <- model$frame

  model_fit <- glmmTMB::glmmTMB(
    formula = form,
    data = original_frame,
    family = glmmTMB::lognormal(link = "log"),
    zi = ~0,
    disp = ~1
  )

  if (length(xvar) == 1) {
    visreg::visreg(model_fit, xvar = xvar, ...)
  } else {
    xvar %>%
      set_names %>%
      map(\(x) visreg::visreg(model_fit, xvar = x, ...))
  }

}

#' Call [extract_visreg_residuals] for a model wrapper from the pipeline and format results
#'
#' Returns a long dataframe of [visreg::visreg] partial residuals and fit by covariable used for partials
#'
#' @param metric Response variable
#' @param covariables vector of character explanatory variables
#' @param model_wrapper a model-wrapping nested list : wrapper$models$metric$modelname is a glmmTMB object
partials_single_response <- function(metric, covariables, model_wrapper) {
  model_wrapper$models[[metric]]$full %>%
    extract_visreg_residuals(xvar = covariables, scale = "response", plot = F, partial = T, re.form = ~0) %>%
    map2(., names(.), function(visreg_output, covariable) {

      # prepare a dataframe with following columns :
      # covariable_var, HABITAT_GROUP, covariable_name, visreg_type, visreg_value
      # one for fitted values and one for residuals
      # then bind_rows and return

      fits <- visreg_output$fit %>% # extract fitted values
        select(c(!!sym(covariable), HABITAT_GROUP, visregFit, visregLwr, visregUpr)) %>%
        rename(covariable_value := !!sym(covariable)) %>%
        pivot_longer(matches("visreg"), names_to = "visregOut", values_to = "visregVal") %>%
        mutate(
          covariable_name = covariable
        )

      visreg_output$res %>% # extract residuals
        rename(
          covariable_value := !!sym(covariable),
          visregVal = visregRes
        ) %>%
        select(
          covariable_value, visregVal, HABITAT_GROUP # keep habitat to check what was used for prediction
        ) %>%
        mutate(
          covariable_name = covariable,
          visregOut = "visregRes"
        ) %>%
        bind_rows(fits)
    }) %>%
    do.call(bind_rows, .)
}

#' Formatting wrapper around [partials_single_response]
#'
#' Return separate data frames from data extracted from visreg residuals
#' and from model summary : the partial residuals by covariables,
#' the partial prediction by covariable with information on the significance
#' of each effect, and the significance information alone
partials_fitted_and_signif <- function(response, covariables, mods) {
  out <- partials_single_response(response, covariables, mods)

  residuals <- out %>% filter(visregOut == "visregRes") %>%
    mutate(
      HABITAT_true = rep(mods$models[[response]]$full$frame$HABITAT_GROUP, 4)
    )

  significance <- mods$models[[response]]$full %>%
    broom.mixed::tidy(component = "cond", effects = "fixed") %>%
    mutate(
      p_adj = p.adjust(p.value, method = "BH"),
      sig = ifelse(p_adj < 0.05, "yes", "no")
    ) %>%
    select(term, p.value, p_adj, sig)

  fitted <- out %>% filter(
    visregOut != "visregRes"
  ) %>%
    left_join(
      significance,
      by = join_by(covariable_name == term)
    ) %>%
    pivot_wider(names_from = visregOut, values_from = visregVal)

  list(
    partials = residuals,
    fitted = fitted,
    signif = significance
  )
}
