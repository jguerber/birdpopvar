target_list_all <- function(parameters) {

  data_cleaning <- step_data_cleaning()

  build_communities <- step_build_communities()

  local_trends <- step_local_trends(parameters)

  community_data <- step_community_data(parameters)

  stats_and_plots <- step_stats(parameters)

  list(
    data_cleaning,
    build_communities,
    local_trends,
    community_data,
    stats_and_plots
  )
}
