build_fig_stats1_parameters <- function(out_glm, model_name) {
  estimates <- c(variability = "sd_r_average", trend = "abs_trend_average") %>%
    map(function(r) {
      broom.mixed::tidy(
        out_glm$models[[r]][[model_name]],
        component = "cond",
        effects = "fixed",
        conf.int = T
      ) %>%
        mutate(response = r)
    }) %>% do.call(bind_rows, .)

  estimates %>%
    mutate(
      response_lab = factor(case_match(
        response,
        "abs_trend_average" ~ "Trend",
        "sd_r_average" ~ "Variability"
      ), levels = c("Variability", "Trend")),
      term = factor(
        case_match(
          term,
          "N_POINTS_avg" ~ "n. points",
          "mu_SR" ~ "Richness",
          "HII_focal" ~ "Impact",
          "H_fine" ~ "Complexity",
          "HABITAT_GROUPfarmland" ~ "Farmland habitat\n(vs. woodland)",
          "HABITAT_GROUPbuilt" ~ "Built habitat\n(vs. woodland)"
        ), levels = c("n. points", "Complexity", "Impact", "Richness", "Built habitat\n(vs. woodland)", "Farmland habitat\n(vs. woodland)")
      )
    ) %>%
    summary_plot(plot_type = "segment", reorder_terms = F) +
    facet_wrap(~response_lab, ncol = 2, scales = "free_x") +
    theme_bw() +
    theme(legend.position = "none") +
    labs(
      y = "",
      x = "Estimates (on the log scale)"
    )
}
