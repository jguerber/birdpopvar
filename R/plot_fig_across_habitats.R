#' Wrapper around visreg calls and compose_plot_across_habitats
build_fig_across_habitats <- function(store, font_size = 14) {
  # define list of components
  components <- c("sd_r_average", "abs_trend_average") %>%
    set_names

  # read model wrapper from pipeline
  mods <- targets::tar_read(
    models_stab_across_habitats,
    store = store
  )

  # map over ?extract_visreg_residuals
  partials_extraction <- components %>%
    map(function(r) {
      mods$models[[r]]$full %>%
        extract_visreg_residuals(xvar = "HABITAT_GROUP", scale = "response", plot = F, partial = T, re.form = NA)
    })

  # arrange all visreg residuals in the same dataframe
  projected_dat <- components %>%
    map(function(r) {
      mods$models[[r]]$full$frame %>% # original data
        mutate(
          component = str_replace(r, "_average$", "")
        ) %>%
        rename(
          observed = !!sym(r) # standardize column names across different response variables
        ) %>%
        mutate(
          partial_hab = partials_extraction[[r]]$res$visregRes # residuals info
        )
    }) %>%
    do.call(bind_rows, .) %>%
    pivot_longer(
      cols = c(observed, partial_hab),
      names_to = "type",
      values_to = "value"
    ) %>%
    mutate(
      component = factor(component, levels = c("sd_r", "abs_trend")),
      type = factor(type, levels = c("partial_hab", "observed"))
    )

  # arrange all visreg fits in the same dataframe
  model_predictions <- components %>%
    map(function(r) {
      partials_extraction[[r]]$fit %>%
        rename(
          median = !!sym(r)
        ) %>%
        mutate(
          component = str_replace(r, "_average$", "")
        )
    }) %>%
    do.call(bind_rows, .) %>%
    mutate(
      component = factor(component, levels = c("sd_r", "abs_trend"))
    )

  compose_plot_across_habitats(
    projected_dat,
    model_predictions,
    font_size = font_size
  )
}

compose_plot_across_habitats <- function(point_data, visreg_fits, font_size = 14) {
  bottom_plot <- point_data %>%
    filter(type == "partial_hab") %>%
    core_plot( font_size = font_size) +
    facet_wrap(
      ~component,
      ncol = 2,
      scales = "free"
    ) +
    geom_errorbar(
      data = visreg_fits,
      aes(ymin = visregLwr, ymax = visregUpr)
    ) +
    theme(
      strip.text = element_blank()
    ) +
    labs(
      y = "Partial residuals\nfor habitat category",
      x = "Habitat category"
    )

  top_plot <- point_data %>%
    filter(type == "observed") %>%
    core_plot( font_size = font_size) +
    geom_boxplot(aes(y = value), alpha = 0, color = "black") +
    facet_wrap(~component, scales = "free",  ncol = 2, labeller = labeller(
      component = c(sd_r = "Mean detrended population variability", abs_trend = "Mean absolute trend")
    )) +
    theme(
      strip.text = element_text(size = font_size),
      axis.text.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = font_size - 2, unit = "pt"))
    ) +
    labs(
      x = "",
      y = "Observed values"
    ) +
    scale_y_log10(n.breaks = 5)

  cowplot::plot_grid(
    top_plot,
    NULL,
    bottom_plot,
    ncol = 1,
    align = "vh",
    axis = "tblr",
    rel_heights = c(1,-0.2,1)
  )
}

core_plot <- function(df, point_size = 0.75, font_size = 14, hline = F) {

  dat_line <- df %>%
    group_by(component, type) %>%
    summarise(
      med = median(value)
    )

  p <- df %>%
    ggplot(aes(x = HABITAT_GROUP)) +
    ggforce::geom_sina(aes(color = HABITAT_GROUP, y = value), size = point_size) +
    labs(
      y = "",
      x = ""
    ) +
    scale_habitats() +
    cowplot::theme_cowplot(font_size = font_size) +
    theme(
      legend.position = "none",
      strip.placement = "none",
      strip.background = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "plain"),
      axis.title.y = element_text(hjust = 0.5)
    )

  if (hline) {
    p <- p +
      geom_hline(
        data = dat_line,
        aes(yintercept = med),
        linetype = "dotted",
        color = "black",
        linewidth = 1.2
      )
  }

  return(p)
}
