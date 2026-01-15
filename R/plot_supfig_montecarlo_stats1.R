build_supfig_montecarlo_stats1 <- function(unpropagated_models, propagated_summary) {
  mods_summaries <- c("habitat_controlled", "full") %>% 
    set_names %>% 
    map(
        ~ broom.mixed::tidy(unpropagated_models[[.x]], conf.int = T)
    ) %>% 
    bind_rows(.id = "model") %>% 
    rename(
        lwr = conf.low,
        upr = conf.high
    ) %>% 
    mutate(
        estimation = "ML"
    )

summaries_stats1 <- propagated_summary %>% # MC error only
    mutate(
        estimation = "MC_coef"
    )

infer_mc_propagated <- propagated_summary %>% # MC error on coefs + on model uncertainty
    pivot_wider(
        names_from = variable,
        values_from = c(lwr, upr, sd, med, mean)
    ) %>% 
    mutate(
        lwr = qnorm(0.025, mean = mean_estimate, sd= sqrt(sd_estimate^2 + med_std.error^2 + sd_std.error^2)),
        upr = qnorm(0.975, mean = mean_estimate, sd= sqrt(sd_estimate^2 + med_std.error^2 + sd_std.error^2))
    ) %>%
    filter(term != "(Intercept)") %>% 
    mutate(
        estimation = "MC_full"
    )

all_estimates <- bind_rows(
    filter(summaries_stats1, variable == "estimate"),
    filter(mods_summaries, effect == "fixed"),
    infer_mc_propagated
) %>% 
    filter(term != "(Intercept)")

all_estimates %>%
  mutate(
    term = factor(case_match(
      term,
    "N_POINTS_avg" ~"n. points",
    "mu_SR" ~"Richness",
    "HII_focal" ~"HII",
    "HABITAT_GROUPfarmland" ~"Farm. vs wood.",
    "HABITAT_GROUPbuilt" ~"Built vs wood.",
    "H_fine" ~"LC"
  ), levels = c("n. points", "LC", "HII", "Richness", "Built vs wood.", "Farm. vs wood."))
  ) %>% 
    ggplot(aes(y = term)) +
    geom_segment(aes(x  = lwr, xend = upr, color = estimation), position = position_dodge(width = 0.5, orientation = "y"), linewidth = 1) +
    geom_vline(aes(xintercept = 0), linetype = "dashed") +
    facet_wrap(~model, ncol = 1, scales = "free_y", labeller = labeller(
      model = c(
        full = "Full model",
        habitat_controlled = "Model for Fig. 2"
      )
    )) +
    scale_color_viridis_d(end = 0.9) +
    labs(
        y = "",
        x = "Confidence interval for model coefficients",
        color = "Estimation method"
    ) +
    cowplot::theme_cowplot() +
    theme(
      strip.background = element_rect(fill = "white", color = "black", linewidth = 1)
    )

}