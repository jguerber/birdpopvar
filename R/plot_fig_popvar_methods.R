build_fig_methods_alt <- function(example_coms, table_summary, debug = T) {

  # metrics summary plot (bottom panel)
  p <- table_summary %>%
    tibble::rownames_to_column("y") %>%
    mutate(
      y = str_replace(y, "average", "mean"),
      y = str_replace(y, "Average", "Mean")
    ) %>%
    mutate(
      y = factor(
        y,
        levels = c(
          "Weighted mean\npopulation variability",
          "Mean absolute trend",
          "Mean detrended\npopulation variability"
        )
      )
    ) %>%
    mutate(
      bg = case_match(
        y,
        "Weighted mean\npopulation variability" ~ "dark",
        .default = "light"
      )
    ) %>%
    pivot_longer(matches("trend"), names_to = "metric", values_to = "val") %>%
    group_by(
      y
    ) %>%
    mutate(
      higher = ifelse(val == max(val), "higher", "lower")
    ) %>%
    mutate(
      facet = ifelse(metric == "trended", "Community B", "Community A"),
      facet_lab = ifelse(metric == "trended", "B", "A")
    ) %>%
    ggplot(aes(y = val, x = facet)) +
    facet_wrap(~y, scales = "free_y", strip.position = "left", ncol = 1) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = bg))+
    # geom_hline(aes(yintercept = val), color = "gray20", linetype = "dashed") +
    geom_col(color = "black", aes(fill = higher), width = 0.2) +
    scale_fill_manual(
      values = c(
        dark = "darkgray",
        light = "lightgray",
        higher = "gray20",
        lower = "gray80"
      )
    ) +
    cowplot::theme_cowplot(font_size = 12) +
    theme(
      strip.placement = "outside",
      strip.background.x = element_blank(),
      strip.background.y = element_rect(fill = "white"),
      strip.text.y.left = element_text(angle = 0),
      legend.position = "none"
    ) +
    labs(
      y = "",
      x = ""
    )

  p <- cowplot::plot_grid(
    plot_example_communities(example_coms, font_size = 12), # communities plot (top panel)
    p,
    ncol = 1,
    labels = c("(a)", "(b)"),
    label_size = 14,
    axis = "lr",
    align = "v"
  )

  if (debug) {
    p <- draw_debug_grid(p)
  }

  p
}


plot_example_communities <- function(example_data, ...) {
  example_data %>%
    mutate(
      com_label = case_match(
        comm,
        "trended" ~ "Community B",
        "untrended" ~ "Community A"
      )
    ) %>%
    select(-c(model, coefs, beta, residuals, sdr, cv, mu)) %>%
    unnest(c(data, fit)) %>%
    ggplot(
      aes(x = t, color = species)
    ) +
    geom_line(aes(y = y, group = species)) +
    geom_point(aes(y =y), size = 0.5) +
    geom_line(aes(y = fit), linewidth = 1) +
    facet_wrap(~com_label) +
    cowplot::theme_cowplot(...) +
    theme(
      legend.position = "none",
      strip.background = element_blank()
    ) +
    labs(
      x = "Time",
      y = "Abundance"
    )

}


build_fig_methods_alt2 <- function(sims, table, font_size = 14, ...) {
  metrics <- table %>%
    tibble::rownames_to_column("y") %>%
    mutate(
      y = str_replace(y, "average", "mean"),
      y = str_replace(y, "Average", "Mean")
    ) %>%
    mutate(
      y = factor(
        y,
        levels = c(
          "Weighted mean\npopulation variability",
          "Mean of\nabsolute trends",
          "Mean detrended\npopulation variability"
        )
      )
    )

  # Bottom part : barplot of component values by community
  p_compare <- metrics %>%
    pivot_longer(
      cols = matches("trended"),
      names_to = "comm",
      values_to = "value"
    ) %>%
    mutate(
      comm = case_match(
        comm,
        "trended" ~ "B",
        "untrended" ~ "A"
      )
    ) %>%
    ggplot(aes(x = comm, y = value)) +
    geom_col(fill = "lightgray", color = "black", width = 0.5) +
    facet_wrap(~y, scales = "free_y") +
    cowplot::theme_cowplot(font_size = font_size) +
    theme(
      strip.background = element_blank()
    ) +
    labs(
      x = "Community",
      y = ""
    )

  # Top row : plot two communities with a colorblind friendly scale
  p <- plot_example_communities(sims, font_size = font_size) +
    scale_color_viridis_d(end = 0.75)

  cowplot::plot_grid(
    p,
    p_compare,
    scale = c(1, 1),
    rel_heights = c(1.11,1),
    align = "v",
    axis = "lb",
    ncol = 1,
    labels = c("(a)", "(b)")
  )

}


build_fig_methods_single_species <- function() {

  single_species <- generate_species_trend(0.1, -0.025, 3.1, seed = 15) %>%
    mutate(
      community = "Community C"
    ) %>%
    group_by(community) %>%
    nest() %>%
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

  single_species_series <- single_species %>%
    select(-c(model, beta, coefs, sdr, cv, mu)) %>%
    unnest(c(data, fit ,residuals)) %>%
    ungroup


  col_points <- scales::viridis_pal()(1)
  p_series <- single_species_series %>%
    ggplot(aes(x = t, y =y)) +
    geom_line(color = "darkgray", linetype = "dashed") +
    geom_point(color = col_points) +
    geom_line(aes(y = fit)) +
    cowplot::theme_cowplot(font_size = 12) +
    labs(
      y = "Population abundance",
      x=  "Time"
    )

  panel_fit = single_species_series %>%
    ggplot(aes(x = t, y = fit)) +
    geom_line() +
    annotate(
      geom = "segment", x = 9.8, xend = 15, y = exp(3.3), yend = exp(3.3),
      linetype = "dotted", linewidth = 1, lineend = "round"
    ) +
    annotate(
      geom = "segment", x = 15, y = exp(3.3), yend = exp(3.205), linetype = "dotted", linewidth = 1, lineend = "round"
    ) +
    labs(
      x = "",
      y = "Fitted abundance"
    ) +
    scale_y_log10()+
    cowplot::theme_cowplot(font_size = 12)

  panel_resid2 = single_species_series %>%
    mutate(
      fake_facet = "Pearson residuals"
    ) %>%
    ggplot(aes(x = t, y = residuals)) +
    geom_hline(aes(yintercept = 0), color = "darkgray", linetype = "dashed") +
    geom_point(color = col_points) +
    labs(
      x = "Time",
      y= "Pearson residuals"
    ) +
    cowplot::theme_cowplot(font_size = 12) +
    theme(strip.background = element_blank())


  cowplot::plot_grid(
    p_series,
    cowplot::plot_grid(
      panel_fit,
      panel_resid2,
      ncol = 1,
      labels = c("(b)", "(c)"),
      label_x = -0.05,
      label_y = 1.02
    ),
    rel_widths = c(1,1),
    ncol = 2,
    labels = c("(a)", ""),
    label_y = 1.01,
    label_x = -0.02
  )
}
