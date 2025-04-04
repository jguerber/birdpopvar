target_list_all <- function() {

  data_cleaning <- step_data_cleaning()

  build_communities <- step_build_communities()

  local_trends <- step_local_trends()

  list(
    data_cleaning,
    build_communities,
    local_trends
  )
}
