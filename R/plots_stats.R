#' Plot model summary as a colored bar plot
summary_plot <- function(model, title = NULL, conf.level = 0.95, plot_type = "col", reorder_terms = T) {
  if (class(model)[1] == "glmmTMB") {
    tidy_model <- model %>%
      broom.mixed::tidy(component = "cond", effects = "fixed")

  } else if (class(model)[1] == "tbl_df") {
    tidy_model <- model
  } else {
    stop("Unsupported model class. Maybe try broom::tidy(model)")
  }

  if ("p.value" %in% colnames(tidy_model)) {
    tidy_model <- tidy_model %>%
      mutate(sig = ifelse(p.value < (1 - conf.level), "yes", "no"))
  } else if ("conf.low" %in% colnames(tidy_model) && "conf.high" %in% colnames(tidy_model)) {
    tidy_model <- tidy_model %>%
      mutate(
        sig = ifelse(
          tidy_model$conf.high < 0 | tidy_model$conf.low > 0,
          "yes",
          "no"
        )
      )
  } else {
    stop("Can't infer significance from provided columns.")
  }

  if (reorder_terms) {
    tidy_model <- tidy_model %>%
      filter(term != "(Intercept)") %>%
      mutate(
        term = factor(term, levels = unique(c(
          sort(str_subset(term, ":")), sort(str_subset(term, ":", negate = T))
        )))
      )
  } else {
    tidy_model <- tidy_model %>%
      filter(term != "(Intercept)")
  }

  p <- tidy_model %>%
    mutate(
      direction = ifelse(estimate > 0, "positive", "negative"),
      across(matches("p.value"), ~ as.character(signif(.x, digits = 3)))
    ) %>%
    ggplot(aes( y = term, alpha = sig)) +
    scale_alpha_manual(values = c(yes = 1, no = 0.4))

  direction_colors = c(positive = "firebrick", negative = "darkblue")
  if (plot_type == "col") {
    p <- p +  geom_col(aes(x = estimate, fill = direction)) +
      scale_fill_manual(values = direction_colors)
  } else if (plot_type == "segment") {
    p <- p +
      geom_point(aes(x = estimate, color = direction)) +
      geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed") +
      geom_segment(aes(x = conf.low, xend = conf.high, color = direction)) +
      scale_color_manual(values = direction_colors)
  }

  if (!is.null(title)) {
    p <- p +
      ggtitle(title)
  }

  p
}
