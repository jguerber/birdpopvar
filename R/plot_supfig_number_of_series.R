build_supfig_timeseries_number <- function(
  population_variability,
  community_survey,
  trends_outputs
) {
  # ensure we work on the final dataset
  coms_final <- population_variability %>%
    pull(COMMUNITY_ID) %>%
    unique()

  communities_dat <- community_survey %>% 
    filter(COMMUNITY_ID %in% coms_final) %>% 
    select(COMMUNITY_ID,  NYEARS)%>% 
    split_community_id %>% 
    unique

  series_dat <- trends_outputs %>% 
    split_series_id %>% 
    filter(COMMUNITY_ID %in% coms_final) %>%
    mutate(
        N_present = N - N_ab
    )

  plot_communities <- communities_dat %>% 
    ggplot(aes(x = NYEARS)) +
    geom_bar(aes(fill = HABITAT_GROUP), width = 0.75) +
    scale_habitats(geom = "fill") +
    cowplot::theme_cowplot(font_size =16) +
    theme(
        legend.position = "none"
    ) +
    labs(
        y = "Number of communities",
        x = "Years of survey"
    )
  
  plot_timeseries <- series_dat %>%
    ggplot(aes(x = N_present)) +
    geom_bar((aes(fill = type_milieu)), width = 0.75) +
    scale_habitats(geom = "fill") +
    cowplot::theme_cowplot(font_size = 16) +
    theme(
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
    ) +
    labs(
        y = "Number of time series",
        x = "Years without absences",
        fill = "Habitat category"
    )
  
  plot_both <- cowplot::plot_grid(
    plot_communities,
    plot_timeseries,
    ncol = 2,
    align = "hv",
    axis = "btl",
    rel_widths = c(1,1.75)
  )

  summaries <- tibble::tibble(
    mean = c(
      mean(communities_dat$NYEARS),
      mean(series_dat$N_present)
    ),
    sd = c(
      sd(communities_dat$NYEARS),
      sd(series_dat$N_present)
    )
  ) %>% 
    mutate(
      across(c(mean, sd), ~signif(.x, 3)),
      lab = paste0(mean, "(",sd,")")
    )
  
  return(list(
    plot = plot_both,
    summaries = summaries
  ))
}