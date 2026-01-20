build_supfig_montecarlo_chisq <- function(chi_estimates) {

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