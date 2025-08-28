decorate_rules_sem_buffers <- function() {
  list(
    hjust_label_rules = \(label) 0.5,
    hjust_edges_rules = \(idx, scale) case_match(
      idx,
      "1" ~ -4.5*scale,
      "2" ~ -1.5*scale,
      "3" ~ 1.5*scale,
      "4" ~ 3.5*scale,
      "5" ~ -1.5*scale,
      "6" ~ 1.5*scale,
      "7" ~ 4.5*scale,
      "8" ~ -3.5*scale,
      "9" ~ -3*scale,
      "11" ~ 3*scale,
      .default = 0
    ),
    label_pos_rules = \(idx) case_match(
      idx,
      c("9", "11") ~ 0.35,
      c("1", "2", "3", "5", "6", "7") ~ 0.65,
      c("10") ~ 0.55,
      c("4","8") ~ 0.35,
      .default = 0.5
    )
  )
}

decorate_rules_sem_buffers_with_ns <- function() {
  c(
    decorate_rules_sem_buffers(),
    show_ns_coefs = TRUE
  )
}

semplot_buffers <- function(
    category,
    results,
    labs,
    decorate_fn = decorate_rules_sem_buffers
  ) {
  results[[category]]$out_sem %>%
    single_sem_plot(
      "3_drivers",
      labs = labs,
      decorate_args = decorate_fn(),
      plot_args = list(node_label_opacity = 1, font_size = 6),
    ) +
    scale_y_reverse(limits = c(6, 1))
}

build_fig_sem_buffers <- function(results, categories, buffer_size = "10km", annotation_positions = NULL, label_variables = NULL) {

  if (is.null(label_variables)) {
    label_variables <- c(
      sd_r_average_log = "Variability",
      abs_trend_average_log = "Trend",
      HII_focal = "HII (local)",
      HII_buffer = paste0("HII (",buffer_size,")"),
      H_fine = "Land. Comp.",
      mu_SR = "Richness"
    )
  }

  if (is.null(annotation_positions)) {
    annotation_positions <- tibble(
      # cat = c("farmland", "woodland", "built"),
      x = rep(0.15, 3),
      y = c(0.69, 0.37, 0.05),
      colour = c("#FFB200", "#119C1E", "#D85B88"),
      fontface = rep("bold", 3),
      hjust = 0.5,
      vjust = 0
    )
  }

  results %>%
    unpack_values %>%
    generic_fig_sems(
      categories,
      label_variables,
      annotation_positions,
      semplot_buffers
    )
}

