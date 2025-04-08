#' Local trends
#'
#' Computing local trends on many time series is computationally intensive. Since
#' targets can be computed in parallel when several workers are available, it would
#' be possible to define a dynamic target branch for each time series or communities.
#' However, this would create several hundreds of targets, which can cause overhead.
#' We instead take advantage of `targets`'s dynamic branches to split communities in batches.
#' Computing all local trends in a batch is then defined as a single dynamic branch,
#' which can all be handled in parallel by different targets, distributed to workers
#' by the `crew` worker system.
#'
#' See the `targets` manual on [dynamic branching](https://books.ropensci.org/targets/dynamic.html)
#' and [batching](https://books.ropensci.org/targets/dynamic.html#performance-and-batching).
step_local_trends <- function(parameters) {
  list(
    tar_target(
      communities_in_batches,
      prepare_trend_batches(aggregate_survey_filtered, parameters)
    ),
    tar_group_by(
      survey_in_batches,
      communities_in_batches %>%
        # slice_sample(n = 3) %>% # debug
        left_join(aggregate_survey_filtered, by = "COMMUNITY_ID"),
      batch_id
    ),
    tar_target(
      trends_output,
      survey_in_batches %>%
        wrap_local_trends(
          type = "species",
          override_arguments = list(try_families = c("gaussian", "poisson"))
        ), # for now, only check that branching works
      pattern = map(survey_in_batches),
      iteration = "list",
      resources = tar_resources( # select the heavy-duty crew controller
        crew = tar_resources_crew(controller_group$names$heavy)
      )
    ),
    tar_target(
      local_trends_summaries,
      do.call(
        bind_rows,
        map(trends_output, \(l) l$summaries)
      )
    ),
    tar_target(
      local_trends_predictions,
      do.call(
        bind_rows,
        map(trends_output, \(l) l$predictions)
      )
    ),
    tar_target(
      all_local_trends_outputs,
      join_summaries_and_predictions(
        local_trends_summaries,
        predictions_metrics(
          local_trends_predictions,
          ab_col = "AB_SUM",
          preferred_family = "poisson"
        ),
        preferred_model = "poisson",
        rtype = "pearson"
      )
    )
  )
}
