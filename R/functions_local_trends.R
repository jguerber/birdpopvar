#' Attribute a batch_id to each community in survey
#'
#' Creates a maximum of 4 times more batches than there are available workers,
#' minimum 2
prepare_trend_batches <- function(survey, parameters) {

  n_workers <- if (is_slurm()) {
    as.integer(parameters$remote_n_tasks)
  } else {
    as.integer(parameters$local_n_cores) - 1
  }
  print(n_workers)
  survey %>%
    select(COMMUNITY_ID) %>%
    unique %>%
    mutate(
      # "stretch" the range 1:n_batches over nrow(.) indexes
      # (found in ?tarchetypes:::tar_group_count_index)
      batch_id = as.integer(
        cut(
          seq_len(n()),
          breaks = max(2, round(n_workers * 4))
        )
      )
    )

}
