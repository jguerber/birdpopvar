# extracting summary statistics from local trends model outputs

#' For each model family, call predictions_single_model
extract_predictions <- function(outputs, series_name) {
  do.call(dplyr::bind_rows, lapply(
    names(outputs),
    function(m) {
      predictions_single_model(outputs[[m]]$value, m)
    }
  )) %>%
    mutate(series_id = series_name)
}

predictions_single_model <- function(model, model_name) {

  frame <- model$frame
  if (any(str_detect(colnames(model$frame), "offset"))) {
    # rename offset columns (deprecated)
    colnames(frame) <- str_replace(colnames(frame), "offset[(]([A-Z]+|[A-Z]_[A-Z]+)[)]", "\\1")
  }

  cbind(
    frame,
    data.frame(
      fit = predict(model, type = "response"),
      residuals = residuals(model, type = "response"),
      pearson = residuals(model, type = "pearson")
    )) %>%
    mutate(model_name = model_name)
}

summarize_model_fits <- function(outputs, response_var = "ABUNDANCE", threshold_poisson) {

  model_comp <- outputs %>%
    comparison_table %>%
    dplyr::mutate(
      convProblems = warnings + convFail + error,
      threshold_warnings = ifelse(model_name == "gaussian", 0, threshold_poisson)
    )

  best_model_name <- model_comp %>%
    dplyr::filter(
      convProblems <= threshold_warnings # no warnings or errors (converged)
    ) %>%
    dplyr::arrange(dAIC) %>% # lowest aic between these
    head(1) %>%
    dplyr::pull(model_name) # returns name

  if (length(best_model_name) == 0) { # if there is no strictly best model, keep default
    found_best_model <- F
  } else {
    found_best_model <- T
  }

  outputs_summary <- do.call(bind_rows, lapply(
    1:length(outputs),
    function(i, outputs) {
      outputs[[i]]$value %>%
        extract_model_coefs(response_var = response_var) %>%
        dplyr::mutate(
          model_name = names(outputs)[i]
        )
    }, outputs = outputs))

  output <- model_comp %>%
    dplyr::select(model_name, dAIC, convProblems, threshold_warnings) %>% # join AIC and convergence info
    dplyr::left_join(outputs_summary, ., by = "model_name")

  if (found_best_model) {
    output <- output %>%
      mutate(
        best = (model_name == best_model_name)
      )
  } else {
    output <- output %>%
      mutate(best = F)
  }

  return(output)
}


#' compares a list of "caught" models, by aic and warnings
comparison_table <- function(models_catch) {

  AICt <- bbmle::AICtab(
    lapply(
      models_catch,
      function(l){l$value}
    ),
    mnames = names(models_catch)
  ) %>%
    as.data.frame %>%
    tibble::rownames_to_column("model_name")

  warnings <- sapply(
    models_catch,
    function(l){
      c(
        warnings = length(l$warnings),
        convFail = l$value$fit$convergence,
        error = length(l$error)
      )
    },
    simplify = T,
    USE.NAMES = T
  ) %>%
    t %>%
    as.data.frame %>%
    tibble::rownames_to_column("model_name")

  left_join(
    AICt,
    warnings,
    by = "model_name"
  )
}

#' Customized coefficient extraction to handle singular fits and other weird
#' situations
extract_model_coefs <- function(model_output, response_var = "ABUNDANCE") {

  model_summary <- model_output %>%
    summary %>%
    suppressWarnings

  coeffs <- model_summary %>%
    coef

  coeffs <- coeffs$cond # glmmTMB : conditional parameters is what we're looking for

  output_ok <- T
  if (mean(abs(residuals(model_output))) <= 1e-30) { # perfect fit: situation is weird
    output_ok <- F
    if (coeffs["YEAR", "Estimate"] <= 1e-16) { # no trend
      output <- tibble(
        trend = 0,
        se = NA,
        p = NA
      )
    } else { # super weird: all points are on a single line
      output <- tibble(
        trend = coeffs["YEAR", "Estimate"],
        se = NA,
        p = NA
      )
    }

  }


  if (any(is.na(coeffs["YEAR",])) & output_ok){ # if weird things happened return NAs
    output_ok <- F
    output <- tibble(
      trend = NA,
      se = NA,
      p = NA
    )
  }

  if (output_ok) {

    output <- tibble(
      trend = coeffs["YEAR", "Estimate"],
      se = coeffs["YEAR", "Std. Error"],
      p = coeffs["YEAR", "Pr(>|z|)"] # Pr(>|z|) for glmmTMB
    )
  }

  output
}
