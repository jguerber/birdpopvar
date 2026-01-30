#### emmeans ####

build_fig_across_habitats_emmeans <- function(model_wrapper, model_name, errorbars = TRUE, font_size = 14) {
  # define list of components
  components <- c("sd_r_average", "abs_trend_average") %>%
    set_names

  # map over emmeans %>% cld
  emmeans_mapped <- components %>%
    map(
      function(r) {
        model_wrapper$models[[r]][[model_name]] %>%
          emmeans::emmeans(specs = ~ HABITAT_GROUP) %>%
          multcomp::cld(Letters = letters) %>%
          as.data.frame %>%
          mutate(
            across(c(emmean, matches("CL$")), exp, .names = "{.col}_response"),
            component = str_replace(r, "_average", "")
          )
      }
    ) %>%
    do.call(bind_rows, .) %>%
    mutate(
      component = factor(component, levels = c("sd_r", "abs_trend"))
    )

  original_data <- components %>%
    map(function(r) {
      model_wrapper$models[[r]][[model_name]]$frame %>%
        rename(
          value = !!sym(r)
        ) %>%
        mutate(
          component = str_replace(r, "_average", ""),
          type =  "observed"
        )
    }) %>%
    do.call(bind_rows, .) %>%
    mutate(
      component = factor(component, levels = c("sd_r", "abs_trend"))
    )

  letter_data <- original_data %>%
    group_by(component) %>%
    summarise(
      mn = min(value),
      mx = max(value)
    ) %>%
    right_join(
      select(emmeans_mapped, c(HABITAT_GROUP, .group, component)), by = "component") %>%
    mutate(
      .group = trimws(.group)
    )

  p <- original_data %>%
    core_plot(font_size = font_size) +
    facet_wrap(~component, ncol = 2, strip.position = "top", scales = "free", labeller = labeller_components()) +
    scale_y_log10() +
    labs(
      y = "Stability component"
    ) +
    theme(strip.text = element_text(size= font_size))

  if (errorbars) {
    p <- p +
      geom_text(
        data = letter_data,
        aes(y = 1.1 * mx, label = .group),
        fontface = "bold",
        size = font_size / 2
      ) +
      geom_errorbar(
        data = emmeans_mapped,
        linewidth = 0.75,
        width = 0.75,
        aes(ymin = asymp.LCL_response, ymax= asymp.UCL_response)
      )
  }

  return(p)
}

#### Common ####

#' Core plot : plot geom_sina of several metrics with optional hline
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

labeller_components <- \() labeller(
  component = c(sd_r = "Mean detrended population variability", abs_trend = "Mean of absolute trends")
)

