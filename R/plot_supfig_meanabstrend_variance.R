build_supfig_meanabstrend_variance <- function(
  null_data,
  sd_trend = 0.05,
  plot_abs_mean_trend = FALSE
) {

  null_summary <- null_data %>% 
      group_by(com_id, SR) %>% 
      summarise(
          mean_trend = mean(trend),
          mean_abs_trend = mean(abs(trend)),
          abs_mean_trend = abs(mean(trend)),
          .groups = "drop"
      )
  
  if (plot_abs_mean_trend) {
    vars <- c("mean_abs_trend", "abs_mean_trend")
  } else {
    vars <- c("mean_abs_trend")
  }

  lines <- null_summary %>% 
      select(
          SR
      ) %>% 
      unique %>% 
      mutate(
          mean_abs_trend = sd_trend * sqrt(2/pi),
          abs_mean_trend = sd_trend * sqrt(2/(pi*SR))
      )%>% 
      pivot_longer(
          cols = all_of(vars),
          names_to = "metric",
          values_to = "value"
      )
  
  p <- null_summary %>% 
    pivot_longer(
        cols = all_of(vars),
        names_to = "metric",
        values_to = "value"
    ) %>% 
    ggplot(aes(x = SR, y= value)) +
    geom_point() +
    geom_line(aes(x = SR, y= value), data = lines) +
    scale_x_log10() +
    scale_y_log10()  +
    cowplot::theme_cowplot()

  if (plot_abs_mean_trend) {
    p <- p +
    facet_wrap(~metric, scales = "free", labeller = labeller(
        metric = c(
            mean_abs_trend = "Mean of absolute trends",
            abs_mean_trend = "Absolute value of mean trend"
        )
    )) +
    labs(
        x = "Species richness",
        y = "Community value"
    )
  } else {
    p <- p + labs(
      x = "Species richness",
      y = "Mean absolute trend"
    )
  }

  p
}