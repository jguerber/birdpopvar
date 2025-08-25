#' Build the figure with the sems
#'
#' In calls to single_sem_plot, decorate_args allows to tweak the *graph data passed to ggraph*, while plot_args tweaks
#' more general ggraph behavior
build_fig_all_sems <- function(
    sem_wrapper,
    label_variables,
    sem_type = "lm",
    categories = c("farmland", "woodland", "built"),
    selected_branch = 1,
    font_size = 10
) {
  sem_results <- sem_wrapper %>%
    unpack_values

  annotation_positions <- tibble(
    # cat = c("farmland", "woodland", "built"),
    x = rep(0.15, 3),
    y = c(0.7, 0.382, 0.058),
    colour = c("#FFB200", "#119C1E", "#D85B88"),
    fontface = rep("bold", 3),
    hjust = 0.5,
    vjust = 0
  )

  generic_fig_sems(
    sem_results,
    categories,
    label_variables,
    annotation_positions,
    semplot_fig3,
    font_size = font_size
  )
}

#' Fetch SEM results from the pipeline
fetch_all_sem_results <- function(sem_wrapper, sem_type, selected_branch = 1) {
  targets::tar_read(
    spatial_sem_output,
    store = store
  ) %>%
    unpack_values
}

#' Unpack SEM results from the nested list wrapper
unpack_values <- function(all_results, names_selected = NULL) {
  if (is.null(names_selected)) {
    names_selected <- names(all_results)
  }
  # unpack from divstab::catchConditions() structure
  names_selected %>%
    set_names %>%
    map(
      \(n) all_results[[n]]$value
    )
}

#' Map `fun_single_plot`, a function to plot a single SEM, on several modalities of `categories`
#'
#' @param ... optional arguments passed to function specified by fun_single_plot
generic_fig_sems <- function(unpacked_results, categories, label_variables, annotations, fun_single_plot, ...) {
  all_plots <- map(categories, function(category, results = unpacked_results, labs = label_variables) {
    fun_single_plot(category, results, labs, ...)
  })

  model_info <- extract_sem_fit_info(unpacked_results, categories)

  join_plots_and_labels(all_plots, model_info, annotations)
}

#' Extract goodness-of-fit stats from several SEM outputs
extract_sem_fit_info <- function(sem_results, categories) {
  do.call(bind_rows, map(categories, function(cat) {
    bind_cols(
      data.frame(
        N = nrow(sem_results[[cat]]$out_sem$data),
        category = cat
      ),
      sem_results[[cat]][[2]]$Cstat
    )
  })) %>%
    mutate(
      label_clean = paste0("N = ",N,"\nC = ", signif(Fisher.C, digits = 3), " ; p = ", signif(P.Value, digits = 2))
    )
}

#' Display three SEM plots on top of each other, with goodness-of-fit text info
join_plots_and_labels <- function(all_plots, model_info, annotation_positions) {
  if (nrow(annotation_positions) != length(all_plots)) stop("plot list and annotation table do not match")

  plot_all <- cowplot::plot_grid(
    plotlist = list(all_plots[[1]], NULL, all_plots[[2]], NULL, all_plots[[3]]),
    rel_heights = c(1, -0.1, 1, -0.1, 1),
    ncol = 1,
    labels = c("(a) Farmland communities", "",  "(b) Woodland communities", "", "(c) Built communities"),
    hjust = 0
  )

  plots_annotated <- cowplot::ggdraw() +
    cowplot::draw_plot(plot_all)

  for (i in 1:nrow(annotation_positions)) {

    plots_annotated <- plots_annotated +
      rlang::inject(cowplot::draw_label(
        model_info$label_clean[i], !!!annotation_positions[i,], size = 12
      ))

  }
  plots_annotated
}


#' ad-hoc wrapper around [single_sem_plot]
semplot_fig3 <- function(
    category,
    results,
    label_variables,
    font_size = 14,
    decorate_fn = decorate_args_fig_5nodes
  ) {
  single_sem_plot(
    results[[category]]$out_sem,
    layout_type = "sugiyama",
    labs = label_variables,
    set_stretch_scale = 0.08,
    decorate_args = decorate_fn(),
    plot_args = list(font_size = font_size / 2)
  )  +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.position = "none"
    )
}

#' ad-hoc graphical tweaks for fig-all-sems, passed to [semplot_fig3]
decorate_args_fig_5nodes <- function() {
  list(
    hjust_label_rules = \(label) case_match( # set label justification : 0.5 is center-align, less is shifted to the right
      label,
      "abs_trend_average_log" ~ 0.5,
      "sd_r_average_log" ~ 0.5,
      # "H_fine" ~ 0.2,
      "hii" ~ 0.5,
      .default = 0.5
    ),
    hjust_edges_rules = \(idx, scale_edge_stretch) case_match(
      idx,
      "1" ~ -1.5*scale_edge_stretch, # left top-left arrow
      "2" ~ 0*scale_edge_stretch, # middle top-left arrow
      "3" ~ 1.5*scale_edge_stretch, # right top-left arrow
      "4" ~ 0*scale_edge_stretch, # middle top-right arrow
      "5" ~ 1.5*scale_edge_stretch, # right top-right arrow
      "6" ~ -1.5*scale_edge_stretch, # left top-right arrow
      "7" ~ -scale_edge_stretch, # left middle
      "8" ~ scale_edge_stretch, # right middle
      .default = 0
    ),
    label_pos_rules = \(idx) case_match(
      idx,
      c("7", "8") ~ 0.65,
      .default = 0.5
    )
  )
}

decorate_args_fig_5nodes_with_ns <- function() {
    c(
      decorate_args_fig_5nodes(),
      show_ns_coefs = TRUE
    )
}
