build_supfig_montecarlo_sem <- function(
  outputs_ml,
  sem_coef_summaries
) {
      

  coefs_df <- extract_sem_coefficients(outputs_ml, c("woodland", "farmland", "built"), warnings_tol =1) %>% 
      filter(Response != "Fisher.C") %>% 
      mutate(
          Response = ifelse(Response == "abs_trend_average_log", "ata_log", Response),
          mean_Std.Estimate = Std.Estimate,
          lwr = qnorm(0.025, mean = Std.Estimate, sd = Std.Error * Std.Estimate/Estimate),
          upr = qnorm(0.975, mean = Std.Estimate, sd = Std.Error * Std.Estimate/Estimate),
          estimation = "ML",
          path = paste0(Response, " ~ ", Predictor)
      )
  
  coefs_mc <- sem_coef_summaries %>% 
    filter(Response != "Fisher.C") %>% 
    pivot_wider(names_from = variable, values_from = c(mean, lwr, upr, sd, med)) %>% 
    mutate(
        total_error = sd_Std.Estimate^2 + med_error_on_std_scale^2 + sd_error_on_std_scale^2,
        lwr = qnorm(0.025, mean = mean_Std.Estimate, sd=  sqrt(total_error)),
        upr = qnorm(0.975, mean = mean_Std.Estimate, sd = sqrt(total_error))
    )%>% 
    mutate(
        path = paste0(Response, " ~ ", Predictor),
        estimation = "MC"
    )

  bind_rows(
    coefs_mc,
    coefs_df
  ) %>% 
      mutate(
          focus = ifelse(str_detect(path, "ata_log"), "trend", "other"),
          HABITAT = factor(HABITAT, levels = c("farmland", "woodland", "built"))
      ) %>% 
      translate_paths() %>% 
      ggplot(
          aes(y = path)
      )+
      geom_vline(aes(xintercept= 0), linetype = "dashed")+
      geom_pointrange(aes(x = mean_Std.Estimate, xmin = lwr, xmax = upr, color = HABITAT, alpha = focus)) +
      facet_grid(HABITAT~estimation) +
      labs(
          y = "SEM paths",
          x = "Standardized SEM coefficients"
      ) +
      scale_alpha_manual(
          values = c(
              trend = 1,
              other = 0.33
          )
      ) +
      scale_habitats() +
      cowplot::theme_cowplot(font_size = 12) +
      theme(
          legend.position = "none",
          strip.background = element_blank(),
          panel.spacing.y = unit(2, "lines")
      )

}

translate_paths <- function(df) {
  df %>% 
      mutate(
          path = factor(case_match(
              path,
              "sd_r_average_log ~ HII_focal" ~ "Variab. ~ HII",
              "sd_r_average_log ~ H_fine" ~ "Variab. ~ LC",
              "sd_r_average_log ~ mu_SR" ~ "Variab. ~ Richness",
              "ata_log ~ HII_focal" ~ "Trend ~ HII",
              "ata_log ~ H_fine" ~ "Trend ~ LC",
              "ata_log ~ mu_SR" ~ "Trend ~ Richness",
              "mu_SR ~ HII_focal" ~ "Richness ~ HII",
              "mu_SR ~ H_fine" ~ "Richness ~ LC"
          ), levels = rev(c(
              "Variab. ~ HII",
              "Variab. ~ LC",
              "Variab. ~ Richness",
              "Trend ~ HII",
              "Trend ~ LC",
              "Trend ~ Richness",
              "Richness ~ HII",
              "Richness ~ LC"
      )))
  )
}