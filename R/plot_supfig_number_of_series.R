build_supfig_timeseries_number <- function(
  population_variability,
  community_survey,
  trends_outputs
) {
  # ensure we work on the final dataset
  coms_final <- population_variability %>%
    pull(COMMUNITY_ID) %>%
    unique()

  plot_communities <- community_survey %>% 
    filter(COMMUNITY_ID %in% coms_final) %>% 
    select(COMMUNITY_ID,  NYEARS)%>% 
    split_community_id %>% 
    unique %>%
    ggplot(aes(x = NYEARS)) +
    geom_bar(aes(fill = HABITAT_GROUP), width = 0.75) +
    scale_habitats(geom = "fill") +
    cowplot::theme_cowplot(font_size =16) +
    theme(
        legend.position = "none"
    ) +
    labs(
        y = "Communities",
        x = "Years of survey"
    )
  
  plot_timeseries <- trends_outputs %>% 
    split_series_id %>% 
    filter(COMMUNITY_ID %in% coms_final) %>%
    mutate(
        N_present = N - N_ab
    ) %>%
    ggplot(aes(x = N_present)) +
    geom_bar((aes(fill = type_milieu)), width = 0.75) +
    scale_habitats(geom = "fill") +
    cowplot::theme_cowplot(font_size = 16) +
    theme(
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
    ) +
    labs(
        y = "Time series",
        x = "Years without absences",
        fill = "Habitat category"
    )
  
  cowplot::plot_grid(
    plot_communities,
    plot_timeseries,
    ncol = 2,
    align = "hv",
    axis = "btl",
    rel_widths = c(1,1.75)
  )
}