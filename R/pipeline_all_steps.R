target_list_all <- function() {

  data_cleaning <- step_data_cleaning()

  build_communities <- step_build_communities()

  list(
    data_cleaning,
    build_communities
  )
}
