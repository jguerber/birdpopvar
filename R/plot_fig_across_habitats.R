#### visreg #####

#' Wrapper around visreg calls and compose_plot_across_habitats
#'
#' @param model_name name to use in the model wrapper from the pipeline
build_fig_across_habitats_visreg <- function(store, model_name = "full", font_size = 14) {
  # define list of components
  components <- c("sd_r_average", "abs_trend_average") %>%
    set_names

  # read model wrapper from pipeline
  mods <- targets::tar_read(
    models_stab_across_habitats,
    store = store
  )

  # map over ?extract_visreg_residuals
  partials_extraction <- components %>%
    map(function(r) {
      mods$models[[r]][[model_name]] %>%
        extract_visreg_residuals(xvar = "HABITAT_GROUP", scale = "response", plot = F, partial = T, re.form = NA)
    })

  # arrange all visreg residuals in the same dataframe
  projected_dat <- components %>%
    join_frame_and_extracted_residuals(partials_extraction, mods)

  # arrange all visreg fits in the same dataframe
  model_predictions <- components %>%
    gather_partial_predictions(partials_extraction)

  compose_plot_across_habitats(
    projected_dat,
    model_predictions,
    font_size = font_size
  )
}


#' add extracted partial residuals to data from model frame
join_frame_and_extracted_residuals <- function(components, partials, mods) {
  components %>%
    map(function(r) {
      mods$models[[r]][[model_name]]$frame %>% # original data
        mutate(
          component = str_replace(r, "_average$", "")
        ) %>%
        rename(
          observed = !!sym(r) # standardize column names across different response variables
        ) %>%
        mutate(
          partial_hab = partials[[r]]$res$visregRes # residuals info
        )
    }) %>%
    do.call(bind_rows, .) %>%
    pivot_longer(
      cols = c(observed, partial_hab),
      names_to = "type",
      values_to = "value"
    ) %>%
    mutate(
      component = factor(component, levels = c("sd_r", "abs_trend")),
      type = factor(type, levels = c("partial_hab", "observed"))
    )

}

#' extract fits from list of objects produced extract_visreg_residuals
gather_partial_predictions <- function(components, partials) {
  components %>%
    map(function(r) {
    partials[[r]]$fit %>%
      rename(
        median = !!sym(r)
      ) %>%
      mutate(
        component = str_replace(r, "_average$", "")
      )
    }) %>%
    do.call(bind_rows, .) %>%
    mutate(
      component = factor(component, levels = c("sd_r", "abs_trend"))
    )
}

compose_plot_across_habitats <- function(point_data, visreg_fits, font_size = 14) {
  bottom_plot <- point_data %>%
    filter(type == "partial_hab") %>%
    core_plot( font_size = font_size) +
    facet_wrap(
      ~component,
      ncol = 2,
      scales = "free"
    ) +
    geom_errorbar(
      data = visreg_fits,
      aes(ymin = visregLwr, ymax = visregUpr)
    ) +
    theme(
      strip.text = element_blank()
    ) +
    labs(
      y = "Partial residuals\nfor habitat category",
      x = "Habitat category"
    )

  top_plot <- point_data %>%
    filter(type == "observed") %>%
    core_plot( font_size = font_size) +
    geom_boxplot(aes(y = value), alpha = 0, color = "black") +
    facet_wrap(~component, scales = "free",  ncol = 2, labeller = labeller_components()) +
    theme(
      strip.text = element_text(size = font_size),
      axis.text.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = font_size - 2, unit = "pt"))
    ) +
    labs(
      x = "",
      y = "Observed values"
    ) +
    scale_y_log10(n.breaks = 5)

  cowplot::plot_grid(
    top_plot,
    NULL,
    bottom_plot,
    ncol = 1,
    align = "vh",
    axis = "tblr",
    rel_heights = c(1,-0.2,1)
  )
}

#### emmeans ####

build_fig_across_habitats_emmeans <- function(store, model_name, errorbars = TRUE, font_size = 14) {
  # define list of components
  components <- c("sd_r_average", "abs_trend_average") %>%
    set_names

  # read model wrapper from pipeline
  mods <- targets::tar_read(
    models_stab_across_habitats,
    store = store
  )

  # map over emmeans %>% cld
  emmeans_mapped <- components %>%
    map(
      function(r) {
        mods$models[[r]][[model_name]] %>%
          emmeans::emmeans(specs = ~ HABITAT_GROUP) %>%
          multcomp::cld(Letters = letters) %>%
          as.data.frame %>%
          mutate(
            across(c(emmean, matches("CL$")), exp, .names = "{.col}_response"),
            component = str_replace(r, "_average", "")
          )
      }
    ) %>%
    do.call(bind_rows, .) %>%
    mutate(
      component = factor(component, levels = c("sd_r", "abs_trend"))
    )

  original_data <- components %>%
    map(function(r) {
      mods$models[[r]][[model_name]]$frame %>%
        rename(
          value = !!sym(r)
        ) %>%
        mutate(
          component = str_replace(r, "_average", ""),
          type =  "observed"
        )
    }) %>%
    do.call(bind_rows, .) %>%
    mutate(
      component = factor(component, levels = c("sd_r", "abs_trend"))
    )

  letter_data <- original_data %>%
    group_by(component) %>%
    summarise(
      mn = min(value),
      mx = max(value)
    ) %>%
    right_join(
      select(emmeans_mapped, c(HABITAT_GROUP, .group, component)), by = "component") %>%
    mutate(
      .group = trimws(.group)
    )

  p <- original_data %>%
    core_plot(font_size = font_size) +
    facet_wrap(~component, ncol = 2, strip.position = "top", scales = "free", labeller = labeller_components()) +
    scale_y_log10() +
    labs(
      y = "Stability component"
    ) +
    theme(strip.text = element_text(size= font_size))

  if (errorbars) {
    p <- p +
      geom_text(
        data = letter_data,
        aes(y = 1.1 * mx, label = .group),
        fontface = "bold",
        size = font_size / 2
      ) +
      geom_errorbar(
        data = emmeans_mapped,
        linewidth = 0.75,
        width = 0.75,
        aes(ymin = asymp.LCL_response, ymax= asymp.UCL_response)
      )
  }

  return(p)
}

#### Common ####

#' Core plot : plot geom_sina of several metrics with optional hline
core_plot <- function(df, point_size = 0.75, font_size = 14, hline = F) {

  dat_line <- df %>%
    group_by(component, type) %>%
    summarise(
      med = median(value)
    )

  p <- df %>%
    ggplot(aes(x = HABITAT_GROUP)) +
    ggforce::geom_sina(aes(color = HABITAT_GROUP, y = value), size = point_size) +
    labs(
      y = "",
      x = ""
    ) +
    scale_habitats() +
    cowplot::theme_cowplot(font_size = font_size) +
    theme(
      legend.position = "none",
      strip.placement = "none",
      strip.background = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "plain"),
      axis.title.y = element_text(hjust = 0.5)
    )

  if (hline) {
    p <- p +
      geom_hline(
        data = dat_line,
        aes(yintercept = med),
        linetype = "dotted",
        color = "black",
        linewidth = 1.2
      )
  }

  return(p)
}

labeller_components <- \() labeller(
  component = c(sd_r = "Mean detrended population variability", abs_trend = "Mean absolute trend")
)

