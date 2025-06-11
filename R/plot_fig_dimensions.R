#' wrapper around tar_read and [component_correlation_plot]
#'
#' @param type "single" or "both" for detrended variability or detrended and
#' classical variability
build_fig_stability_dimensions <- function(trends_output_wrapper, pop_var_data, type = "single", font_size = 14) {
  # compute weighted population CV
  compare <-  trends_output_wrapper %>%
    filter(
      model_name == "poisson",
      residual_type == "pearson",
      convProblems <= threshold_warnings
    ) %>%
    split_series_id %>%
    group_by(COMMUNITY_ID) %>%
    mutate(
      w = mean_ab / sum(mean_ab)
    ) %>%
    summarise(
      w_cvpop = sum(w * raw_cv),
      a_cvpop = mean(raw_cv)
    ) %>% right_join(pop_var_data, by = "COMMUNITY_ID")

  regressions <- compare %>%
    split_community_id %>%
    pivot_longer(cols = c(w_cvpop, sd_r_average), names_to = "metric", values_to = "variability") %>%
    mutate(
      across(c(variability, abs_trend_average), log10),
      metric = factor(metric, levels = c("w_cvpop", "sd_r_average"))
    ) %>%
    group_by(metric) %>%
    group_map(single_metric_variability_regression) %>%
    do.call(bind_rows, .)

  correl_info <- regressions %>%
    select(metric, sp_pval, sp_rho, signif) %>%
    unique

  if (type == "single") {
    y_var <- "sd_r_average"
    y_lab <- "Mean detrended population variability"
  } else if (type == "both") {
    y_var <- c("w_cvpop", "sd_r_average")
    y_lab <- "Mean population variability"
  }

  p <- compare %>%
    split_community_id %>%
    pivot_longer(cols = c(w_cvpop, sd_r_average), names_to = "metric", values_to = "variability") %>%
    mutate(
      HABITAT_GROUP = factor(HABITAT_GROUP, levels = c("woodland", "farmland", "built")),
      metric = factor(metric, levels = c("w_cvpop", "sd_r_average"))
    ) %>%
    component_correlation_plot(
      regression_data = regressions,
      correlation_data = correl_info,
      variability_var = y_var,
      y_lab = y_lab,
      font_size = font_size
    )

  if (type == "both") { # in case of double plot, add an ad-hoc facet
    p <- p +
      facet_wrap(
        ~metric,
        scales = "free",
        ncol = 1,
        strip.position = "left",
        labeller = labeller(metric = c(sd_r_average = "Detrended", w_cvpop = "Not detrended"))
      ) +
      theme(strip.placement = "outside")
  }

  p
}

single_metric_variability_regression <- function(.x, .y) {
  mod <- lm(formula = variability ~ abs_trend_average, data = .x)

  # log10 conserves ranks : no need to back-transform on data scale before Spearman test
  spearman <- cor.test(~ abs_trend_average + variability, data = .x, method = "spearman") %>%
    broom::glance()

  estimates <- mod %>% broom::tidy() %>% filter(term == "abs_trend_average")

  x_range <- seq(from = 0.95*min(.x$abs_trend_average), to = 1.05*max(.x$abs_trend_average), length.out = 100)
  predict(
    mod,
    newdata = tibble(
      abs_trend_average = x_range
    ),
    se.fit = T, interval =  "prediction") %>%
    as.data.frame %>%
    mutate(
      metric = .y$metric,
      abs_trend_average = x_range,
      sp_pval = spearman$p.value,
      sp_rho = spearman$estimate,
      signif = ifelse(sp_pval < 0.05, "yes", "no"),
      reg_est = estimates$estimate,
      reg_se = estimates$std.error
    )
}

#' Plot stability component(s) against mean absolute trend
#'
#' @param variability_var y variables : single character outputs a single plot,
#' character vector of possible values of metric column outputs a facet-able plot
#' (with facet_wrap(~metric))
#' @param correlation_data if provided, will add a label of sp_rho and sp_pval
#'
#' @examples
#' \dontrun{
#' component_correlation_plot( # single plot
#'   point,
#'   reg,
#'   y_lab,
#'   variability_var = "var1"
#'  )
#'
#' component_correlation_plot( # for two variables, add a facet
#'   point,
#'   reg,
#'   y_lab,
#'   variability_var = c("var1", "var2")
#'  ) +
#'   facet_wrap(~metric)
#' }
component_correlation_plot <- function(
    point_data,
    regression_data,
    y_lab,
    correlation_data = NULL,
    variability_var = "sd_r_average",
    font_size = 14
) {
  regressions <- regression_data %>%
    filter(metric %in% variability_var)

  if (!is.null(correlation_data)) {
    if (all(c("sp_pval", "sp_rho") %in% colnames(correlation_data))) {
      correlations <- correlation_data %>%
        filter(metric %in% variability_var) %>%
        mutate(
          p_comp = ifelse(sp_pval <= 0.1, ifelse(sp_pval < 0.05, "p < 0.05", "p < 0.1"), "p > 0.1"),
          txt_clean = paste0("$\\overset{r_s = ", signif(sp_rho, digits = 2), "}{", p_comp,"}$")
          # overset is a workaround to output several lines in latex2exp::TeX, see
          # https://github.com/stefano-meschiari/latex2exp/issues/11#issuecomment-675268189
        )
    }
  }

  lims <- point_data %>%
    filter(metric %in% variability_var) %>%
    group_by(metric) %>%
    summarise(
      mn_x = min(abs_trend_average),
      mx_x = max(abs_trend_average),
      mn_y = min(variability),
      mx_y = min(variability)
    )
  # browser()
  p <- point_data %>%
    filter(metric %in% variability_var) %>%
    ggplot(aes(x = abs_trend_average, y = variability)) +
    geom_point(aes(color = HABITAT_GROUP, shape = HABITAT_GROUP), size = 1.5) +
    geom_line(data = regressions, aes(x = 10^abs_trend_average, y= 10^(fit.fit), alpha = signif)) +
    geom_line(
      data = regressions, aes(x = 10^abs_trend_average, y = 10^fit.lwr, alpha = signif),
      linetype = "dotted"
    ) +
    geom_line(
      data = regressions, aes(x = 10^abs_trend_average, y = 10^fit.upr, alpha = signif),
      linetype = "dotted"
    ) +
    scale_habitats() +
    scale_x_log10() +
    coord_cartesian(xlim = c(0.95*unique(lims$mn_x), 1.4*unique(lims$mx_x))) +
    scale_y_log10() +
    cowplot::theme_cowplot(font_size = font_size) +
    scale_alpha_manual(
      values = c(
        yes = 1,
        no = 0
      )
    ) +
    scale_shape_manual(
      values = c(
        woodland = 17,
        farmland = 4,
        built = 15
      )
    ) +
    theme(
      strip.placement = "outside",
      legend.position = "none",
      strip.background = element_blank()
    ) +
    guides(
      color = guide_legend(override.aes = list(size = 2)),
      alpha = "none"
    ) +
    labs(
      color = "Habitat category",
      shape = "Habitat category",
      y = y_lab,
      x =  "Mean absolute trend"
    )

  # browser()
  if (!is.null(correlation_data)) {
    p <- p +
      geom_label(
        data = left_join(lims, correlations, by = "metric"),
        aes(x = Inf, y = Inf,  label = latex2exp::TeX(txt_clean, output = "character")),
        # enforce label at the top-right to enable axis-based positioning with just
        size = 5,
        vjust = 5.35, # downwards direction (no clue why)
        hjust = 1.05, # leftwards direction
        label.r = unit(0, "mm"),
        parse = T,
        alpha = 0
      )
  }

  p
}
