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
