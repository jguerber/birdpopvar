source(here::here("scripts/import_dependencies.R"))
devtools::load_all()
library(targets)

font_size = 22
set.seed(1803) # the time
trends <- tar_read(
    all_local_trends_outputs,
    store = store_in_pipeline()
) %>% filter(str_detect(series_id, "010511_farmland|390134_woodland"), str_detect(series_id, "PHYCOL|CORCOR|PARMAJ")) %>% 
    split_series_id %>% 
    mutate(
        lwr = qnorm(0.025, mean = trend, sd = se),
        upr = qnorm(0.975, mean = trend, sd = se)
    ) %>% 
    group_by(series_id, lwr, upr, trend, SPECIES, COMMUNITY_ID) %>% 
    reframe(
        draw = rnorm(n(), mean = trend, sd = se)
    ) %>% 
    ungroup

plot_resampling <- trends %>% 
    mutate(
        alpha_right = ifelse(draw > trend, 1, 0),
        alpha_left = ifelse(draw < trend, 1, 0)
    ) %>% 
    ggplot(aes(y = SPECIES)) +
    geom_vline(aes(xintercept = 0), color = "darkgray", linetype = "dashed") +
    # curvature is not mappable as an aesthetic, but we need to change its value based
    # on the direction of the arrow : two geom_curves, one for each direction
    geom_curve( # arrows to the right
        aes(x = trend, xend = draw, yend = SPECIES, color = COMMUNITY_ID, alpha = alpha_right),
        position = position_nudge(y = 0.15),
        curvature = -0.4,
        arrow = arrow(length = unit(0.1, "cm"), type = "closed")
    ) +
    geom_curve( # arrows to the left
        aes(x = trend, xend = draw, yend = SPECIES, color = COMMUNITY_ID, alpha = alpha_left),
        position = position_nudge(y = 0.15),
        curvature = 0.4,
        arrow = arrow(length = unit(0.1, "cm"), type = "closed")
    ) +
    geom_pointrange(aes(xmin = lwr, xmax = upr, x = trend)) +
    geom_point(aes(x = draw, color = COMMUNITY_ID), size = 2) +
    geom_hline(aes(yintercept = -Inf), linewidth = 1.25) +
    scale_alpha_identity() +
    facet_wrap(~COMMUNITY_ID, ncol = 1, scales = "fixed", labeller = labeller(
        COMMUNITY_ID = c(`010511_farmland` = "Community i", `390134_woodland` = "Community j")
    )) +
    cowplot::theme_cowplot(font_size = font_size) +
    scale_color_viridis_d(begin = 0.3, end = 0.9) +
    theme(
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none"
    ) +
    labs(
        x = "Trend",
        y = "Species"
    )

plot_two_meanabstrend <- trends  %>%
    group_by(COMMUNITY_ID)  %>%
    summarise(
        mean_abs_trend = mean(abs(trend)),
        mean_abs_draw = mean(abs(draw))
    ) %>%
    mutate(
        com_id = factor(c("i", "j"), levels = c("j", "i"))
    ) %>% 
    ggplot(aes(x = mean_abs_draw, y = com_id, color = COMMUNITY_ID)) +
    geom_point(size =10) +
    scale_color_viridis_d(begin = 0.3, end = 0.9) +
    cowplot::theme_cowplot(font_size = font_size) +
    theme(
        legend.position = "none"
    ) +
    xlim(c(0,0.09)) +
    labs(
        y = "Community",
        x = "Resampled mean\n of absolute trends"
    )

resamples <- tar_read(
    mc_meanabstrend_samples,
    store = store_in_pipeline()
) %>% 
    filter(rep_id == 1)

popvar_data <- tar_read(
    population_variability_data,
    store = store_in_pipeline()
)

set.seed(1828)
resamples_subset <- resamples %>% 
    slice_sample(n = 40) 
    
plot_single_model <- resamples_subset %>% 
    left_join(popvar_data, by = "COMMUNITY_ID") %>% 
    ggplot(aes(y = mean_abs_trend_mc, x = mu_SR, color = COMMUNITY_ID)) +
    geom_point(size = 2) +
    geom_smooth(aes(y = mean_abs_trend_mc), color = "gray30", fill = "gray30", method = "lm") +
    scale_y_log10() +
    scale_color_viridis_d(begin = 0.3, end = 0.9) +
    cowplot::theme_cowplot(font_size = font_size) +
    theme(
        # axis.text.x = element_blank(),
        legend.position = "none"
    ) +
    labs(
        x = "Explanatory variable",
        y = "Resampled mean\n of absolute trends"
    )

set.seed(1127)
estimates_subset <- tar_read(
    mc_stats1_estimates,
    store = store_in_pipeline()
) %>%
    filter(term == "mu_SR" & !is.na(estimate) & !is.na(std.error)) %>% 
    mutate(
        lwr = qnorm(0.025, estimate, std.error),
        upr = qnorm(0.975, estimate, std.error)
    ) %>% 
    slice_sample(n = 30)

plot_est <- estimates_subset %>% 
    ggplot(aes(y = as.factor(rep_id))) +
    geom_pointrange(aes(xmin = lwr, xmax= upr, x = estimate), color = "darkgray", size = 0.25)+
    cowplot::theme_cowplot(font_size = font_size) +
    theme(
      axis.text.y = element_blank()
    ) +
    labs(
        x = "Estimate for\n the ecological model",
        y = "MC replicate"
    )

summaries <- tar_read(
    mc_stats1_estimates,
    store = store_in_pipeline()
) %>%
    filter(term == "mu_SR" & !is.na(estimate) & !is.na(std.error) & std.error < 0.02) %>% 
  pivot_longer(
    cols = c(estimate, std.error),
    names_to = "var",
    values_to = "val"
  ) %>% 
    group_by(var) %>% 
    summarise(
      mu = mean(val),
      sd = sd(val)
    )

plot_histograms  <- tar_read(
    mc_stats1_estimates,
    store = store_in_pipeline()
) %>%
    filter(term == "mu_SR" & !is.na(estimate) & !is.na(std.error) & std.error < 0.02) %>% 
  pivot_longer(
    cols = c(estimate, std.error),
    names_to = "var",
    values_to = "val"
  ) %>% 
  ggplot(aes(x = val)) +
  geom_histogram(aes(fill = var, color = var), alpha = 0.3) +
  geom_vline(data =summaries, aes(xintercept = mu, color = var), linetype = "dashed", linewidth = 2) +
  facet_wrap(
    ~var,
    scales = "free_x",
    labeller = labeller(
      var = c(estimate = "Slope", std.error = "Error")
    )
  ) +
  cowplot::theme_cowplot(font_size = font_size) +
  theme(
    strip.background = element_blank(),
    legend.position = "none",
    axis.text = element_text(size = 14)
  ) +
  labs(
    x = "",
    y = "MC replicates"
  ) +
  scale_color_manual(
    values = c(
      estimate = "darkblue",
      std.error = "darkorange"
    )
  )+
  scale_fill_manual(
    values = c(
      estimate = "darkblue",
      std.error = "darkorange"
    )
  )



all_ses <- summaries %>% 
  pivot_longer(
    cols = c(mu, sd),
    names_to = "type",
    values_to = "sd"
  ) %>% 
  filter(!(var == "estimate" & type == "mu")) %>% 
  mutate(
    dummy = 1,
    order = c(3,4,2)
  ) %>% 
  bind_rows(
    tibble::tibble(
      var = "total",
      type = "mu",
      sd = sqrt(sum(pull(.,sd)^2)),
      dummy = 1,
      order = 1
    )
  ) %>% 
  mutate(
    lab = factor(c(
      "MC error for slope",
      "Mean of ML error",
      "MC error for ML error",
      "Total error"
    ), levels = c("Total error", "MC error for slope", "Mean of ML error", "MC error for ML error"))
  )

plot_intervals <- all_ses %>%
  ggplot(aes(y = lab, x = sd, fill = var, color = var, alpha = type)) +
  geom_col() +
  cowplot::theme_cowplot(font_size = font_size) +
  theme(
    legend.position = "none"
  ) +
  labs(
    y = "",
    x = "Standard deviation"
  )+
  scale_alpha_manual(
    values = c(
      sd = 0.4,
      mu = 0.9
    )
  ) +
  scale_color_manual(
    values = c(
      estimate = "darkblue",
      std.error = "darkorange",
      total = "gray40"
    )
  )+
  scale_fill_manual(
    values = c(
      estimate = "darkblue",
      std.error = "darkorange",
      total = "gray40"
    )
  )

plot_step5 <- cowplot::plot_grid(
  plot_histograms,
  NULL,
  plot_intervals,
  rel_heights = c(1,-0.1,1),
  axis = "l",
  align = "v",
  ncol = 1
)

map2(list(plot_est, plot_step5),c("estimates", "errors"), function(p, .n) {
  ggsave(
    p,
    filename = here::here("manuscript/drawing",paste0("bottom-", .n, ".png")),
    bg = "white",
    width = 20,
    height = 14,
    units = "cm",
    dpi = 600
  )
})

aligned_panels_top <- cowplot::align_plots(
  plot_resampling,
  plot_two_meanabstrend,
  plot_single_model,
  axis = "tb",
  align = "h"
) %>% set_names("resample", "meanabstrend", "model")

map2(aligned_panels_top, names(aligned_panels_top), function(p, .n) {
  ggsave(
    p,
    filename = here::here("manuscript/drawing",paste0("top-", .n, ".png")),
    bg = "white",
    width = ifelse(.n == "meanabstrend", 12, 16),
    height = 12,
    units = "cm",
    dpi = 600
  )
})