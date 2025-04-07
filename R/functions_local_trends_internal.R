#' for a single species at a given point,
#'
#' computes local trends, picks the best (no warnings, convergence, no errors)
#' model and returns parameters
single_series_local_trend <- function(
    df,
    response_var = "ABUNDANCE",
    lm_covariables = c("SAMPLING"),
    offset = "N_POINTS",
    try_families = c(
      "gaussian", "nbinom2_zi", "nbinom2", "poisson_zi", "poisson"
    ),
    control_num_levels = T
) {
  # default return values in case things fail
  output <- NULL
  form <- NULL
  was_corrected <- NULL

  N_absences <- df %>%
    filter(!!sym(response_var) == 0) %>%
    nrow

  if (nrow(df) > 2 && (nrow(df) - N_absences) > 2) { # more than 3 points: building a trend is possible

    building_correction <- build_formula(df, response_var, lm_covariables, offset, control_num_levels)
    form <- building_correction$form
    local_c <- building_correction$local_c

    all_outputs <- try_safe(
      fit_models,
      try_families,
      form,
      df
    ) # try all model families

    if (is.null(all_outputs)) {
      return(empty_summary(nrow(df), N_absences))
    }

    # remove errored models as they would throw errors later
    indices_models_ok <- sapply(
      all_outputs,
      function(l) {
        length(l$error) == 0
      }
    )

    output <- all_outputs[indices_models_ok] # model outputs that are not errored
  }

  return(list(
    outputs = output,
    formula = form,
    was_corrected = local_c
  ))
}


build_formula <- function(df, abundance_stamp, lm_covariables, offset = NULL, control_num_levels = T) {
  # optionally based on control_num_levels,
  # don't add covariables that don't vary for this site
  covars_ok <- sapply(
    lm_covariables,
    function(v){
      if (!control_num_levels) {
        return(TRUE)
      }

      n_levels <- df %>%
        pull(!!sym(v)) %>%
        unique %>%
        length

      if (n_levels > 1) {
        return(TRUE)
      }

      return(FALSE)
    }
  )

  covariables <- lm_covariables[covars_ok]

  if (length(covariables > 0)) {
    form <- paste(abundance_stamp, "~ YEAR +", paste(covariables, collapse = "+"))
    local_c <- T
  } else {
    form <- paste(abundance_stamp, "~ YEAR")
    local_c <- F
  }

  if (!is.null(offset)) {
    offset_terms <- map(offset, \(t) paste0("offset(", t, ")"))
    form <- paste(form, paste(offset_terms, collapse = "+"), sep = " + ")
  }

  return(list(
    form = form,
    local_c = local_c
  ))
}

empty_summary <- function(N, N_absences) {
  tibble(
    model_name = NA,
    trend = NA,
    se = NA,
    p = NA,
    corrected = NA,
    convProblems = NA,
    N = N,
    N_ab = N_absences,
    dAIC = NA
  )
}

#### Fit the model(s) for a single time series ####

#' Fit several model types on the same data, same formula
fit_models <- function(model_types, form, data){

  sapply(
    model_types,
    fit_single_modeltype,
    form = form,
    data = data,
    simplify = F
  )
}

#' fit a single model type while catching warnings and errors
fit_single_modeltype <- function(type, form, data) {

  selected_family <- stringr::str_replace(type, "_zi$", "")
  zi_formula <- ifelse(stringr::str_detect(type, "_zi"), "~1", "~0")

  # keep offset only for log-transformed families
  if (selected_family == "gaussian") {
    form <- str_replace(
      form,
      "[+] offset[(](?:log[(])?(([A-Z]+)|([A-Z]+_[A-Z]+))(?:[)])?[)]", # + offset(N_POINTS) or + offset(OFFSETVAR) with optional log
      "+ \\1"
    )
  }

  suppressMessages({glmmTMB::glmmTMB(
    as.formula(form),
    data,
    family = selected_family,
    ziformula = as.formula(zi_formula)
  ) %>%
      catchConditions()
  })
}
