target_list_all <- function(parameters) {

  data_cleaning <- step_data_cleaning() # A

  build_communities <- step_build_communities() # B

  local_trends <- step_local_trends(parameters) # C

  community_data <- step_community_data(parameters) # D

  stats_and_plots <- step_stats(parameters) # E

  build_figures <- step_build_figures(parameters) # F1

  build_document <- step_build_document(parameters) # F2

  list(
    data_cleaning,
    build_communities,
    local_trends,
    community_data,
    stats_and_plots,
    build_figures,
    build_document
  )
}

target_list_shortcut <- function(parameters) {

  load_targets <- step_load_targets(parameters) # load dependencies of E

  stats_and_plots <- step_stats(parameters) # E

  build_figures <- step_build_figures(parameters) # F1

  build_document <- step_build_document(parameters) # F2

  list(
    load_targets,
    stats_and_plots,
    build_figures,
    build_document
  )

}
