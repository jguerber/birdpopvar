build_supfig_montecarlo_chisq <- function(mc_sem_estimates) {
  chi_estimates <- mc_sem_estimates %>%
    filter(Response == "Fisher.C") %>%
    group_by(HABITAT) %>% 
    summarise_mc_vars(
        vars = c("Std.Estimate"),
        R = 10000
    )

  tibble::tibble(
      HABITAT = "Null hypothesis",
      lwr = qchisq(0.025, df = 2),
      upr = qchisq(0.975, df = 2)
  ) %>% 
    bind_rows(chi_estimates) %>% 
    mutate(
        col = ifelse(HABITAT == "Null hypothesis", "all", HABITAT),
        HABITAT = factor(HABITAT, levels = c("farmland", "woodland", "built", "Null hypothesis"))
    ) %>% 
    ggplot(aes(y = HABITAT)) +
    geom_segment(aes(x = lwr, xend = upr, color = col)) +
    scale_habitats() +
    cowplot::theme_cowplot() +
    theme(
        legend.position = "none"
    ) +
    labs(
        x= "Fisher's C",
        y = ""
    )
}