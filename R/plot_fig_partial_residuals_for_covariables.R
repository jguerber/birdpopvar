#' Plot outputs of [partials_fitted_and_signif] with sensible defaults
plot_partials_and_fit <- function(
    partials,
    fitted,
    signif,
    title = "default",
    labels_covariables = c(
      HII_focal = "Impact",
      H_fine = "Complexity",
      mu_SR = "Richness",
      N_POINTS_avg = "n. points"
    )
) {
  partials %>%
    ggplot(aes(x = covariable_value, y= visregVal)) +
    geom_point(aes(color = HABITAT_true), alpha = 0.8, size = 0.75) +
    geom_line(
      data =fitted, aes(y = visregFit, alpha =sig)
    ) +
    geom_line(
      data = fitted, aes(y = visregLwr, alpha = sig), linetype = "dotted"
    ) +
    geom_line(
      data = fitted, aes(y = visregUpr, alpha = sig), linetype = "dotted"
    ) +
    facet_wrap(
      ~covariable_name,
      ncol = 1,
      labeller = labeller(covariable_name = labels_covariables),
      scales = "free",
      strip.position = "top"
    ) +
    scale_alpha_manual(values = c(yes = 1, no = 0)) +
    scale_habitats() +
    ggtitle(
      title
    ) +
    cowplot::theme_cowplot() +
    theme(
      legend.position = "none",
      # strip.background = element_rect(color = "black", fill = "white"),
      strip.background = element_blank(),
      strip.placement = "outside",
      plot.title = element_text(hjust = 0.5, face = "plain", size = 14)
    ) +
    labs(
      y = "Partial residuals for variable",
      x = "Scaled explanatory variable"
    )
}
