step_build_figures <- function(parameters) {
  figures <- list(
    tar_target(
      fig_popvar_methods,
      build_fig_methods_alt2(
        example_communities_models,
        table_summary,
        debug = F
      )
    ),
    tar_target(
      fig_components,
      build_fig_stability_dimensions(
        all_local_trends_outputs,
        population_variability_data,
        type = "both"
      )
    ),
    tar_target(
      fig_across_hab,
      build_fig_across_habitats_emmeans(
        model_wrapper = models_stab_across_habitats,
        model_name = "habitat_controlled",
        font_size = 18,
        errorbars = TRUE
      )
    ),
    tar_target(
      fig_all_sems,
      build_fig_all_sems(
        spatial_sem_output,
        sem_type = "nlme",
        label_variables = c(
          sd_r_average_log = "Variability",
          abs_trend_average_log = "Trend",
          HII_focal = "Hum. Imp.",
          mu_SR = "Richness",
          H_fine = "Land. Comp."
        ),
        selected_branch = 1,
        categories = c("farmland", "woodland", "built"),
        font_size = 12
      ),
      packages = c(default_dependencies(), "ggraph", "rlang")
    )
  )

  tables <- list(
    tar_target(
      survey_stats,
      fetch_survey_stats(
        aggregate_survey,
        population_variability_data,
        all_local_trends_outputs
      )
    )
  )

  supplementary_figures <- list(
    tar_target(
      supfig_maps,
      build_fig_maps(
        population_variability_data,
        community_coordinates,
        france_shape
      )
    ),
    tar_target(
      supfig_stats1_estimates,
      build_fig_stats1_parameters(
        models_stab_across_habitats,
        model_name = "habitat_controlled"
      ),
      packages = c(default_dependencies(), "glmmTMB")
    ),
    tar_target(
      supfig_stats1b_estimates,
      build_fig_stats1_parameters(
        models_stab_across_habitats,
        model_name = "full"
      ),
      packages = c(default_dependencies(), "glmmTMB")
    ),
    tar_target(
      supfig_sem_buffers,
      build_fig_sem_buffers(
        spatial_sem_buffer_output_10km,
        c("farmland", "woodland", "built")
      ),
      packages = c(default_dependencies(), "ggraph", "rlang")
    )
  )

  supplementary_tables <- list(
    tar_target(
      suptbl_stats1_selection,
      build_stats1_selection_table(
        models_stab_across_habitats,
        model_names = c("full","habitat_controlled", "no_ecology", "null"),
        match_vars = function(model) {
          case_match(
            model,
            "Full" ~ "All",
            "No ecology" ~ "Number of points by community",
            "Habitat controlled" ~ "Habitat category and number of points",
            "Null" ~ "None (intercept only)"
          )
        }
      )
    )
  )

  # dummy target to invalidate build_figures if targets loaded in child documents
  # are invalidated
  combined <- tarchetypes::tar_combine(
    trigger_document_dependencies,
    list(figures, tables, supplementary_figures, supplementary_tables),
    command = list(!!!.x)
  )

  list(
    figures,
    tables,
    supplementary_figures,
    supplementary_tables,
    combined
  )
}

step_build_document <- function(parameters) {

   list(
    tar_target(
      current_pipeline_store,
      targets::tar_config_get("store")
    ),
    tar_quarto(
      build_figs,
      path = here::here("quarto/build_figures"),
      working_directory = here::here()
    )
  )

}
