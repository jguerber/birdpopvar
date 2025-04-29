
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

  model_fit <- glmmTMB(
    formula = form,
    data = original_frame,
    family = "lognormal",
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
