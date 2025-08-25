#' Decorate : add graphical tweaks to data columns read by [plot_sem_output]
#'
#' Uses [tidygraph] API to modify edge and node data with dplyr syntax
decorate_sem_graph_data <- function(
    gr,
    labels_variables,
    scale_edge_stretch = 0.05,
    hjust_label_rules = NULL,
    hjust_edges_rules = NULL,
    label_pos_rules = NULL,
    show_ns_coefs = FALSE
) {

  if (is.null(hjust_label_rules)) {
    hjust_label_rules <- \(label) case_match( # set label justification : 0.5 is center-align, less is shifted to the right
      label,
      "abs_trend_average_log" ~ 0.125,
      "sd_r_average_log" ~ 0.75,
      # "H_fine" ~ 0.2,
      "hii" ~ 1,
      .default = 0.5
    )
  }

  if (is.null(hjust_edges_rules)) {
    hjust_edges_rules <- \(idx, scale_edge_stretch) case_match(
      idx,
      "1" ~ -4*scale_edge_stretch,
      "2" ~ -2*scale_edge_stretch,
      "4" ~ 2*scale_edge_stretch,
      "5" ~ 4*scale_edge_stretch,
      "7" ~ -scale_edge_stretch,
      "8" ~ scale_edge_stretch,
      .default = 0
    )
  }

  if (is.null(label_pos_rules)) {
    label_pos_rules <- \(idx) 0.5
  }
  # browser()
  gr %>%
    tidygraph::activate(edges) %>%
    mutate(
      sign = ifelse(as.numeric(label) > 0, "positive", "negative"),
      color_clean = ifelse(style == "solid", sign, "unsign."),
      color_clean = factor(color_clean, levels = c("unsign.", "negative", "positive")),
      label = handle_sem_labels(color_clean, label, show_ns_coefs),
      idx = 1:length(tidygraph::.E()$label),
      cap_rect_width = case_match(
        as.character(idx),
        "3" ~ 0,
        .default = 0
      ),
      hjust_set = hjust_edges_rules(as.character(idx), scale_edge_stretch),
      nudge_start_x = case_when(
        as.character(idx) %in% c("1", "7", "4") ~ 0,
        as.character(idx) %in% c("2", "8", "5") ~ 0,
        .default = 0
      ),
      from = from + 0.1,
      label_pos = label_pos_rules(as.character(idx))
    ) %>%
    arrange(color_clean) %>%
    tidygraph::activate(nodes) %>%
    mutate(
      label_txt = ifelse(label %in% names(labels_variables), labels_variables[label], label),
      hjust_label = hjust_label_rules(label)
    )
}

handle_sem_labels <- function(color, label, show_ns) {

  converted_digits <- as.character(signif(as.numeric(label), digits = 2))

  if (!show_ns) {
    return(ifelse(color != "unsign.", converted_digits, ""))
  }

  return(converted_digits)
}

#' Wrapper around utility functions for SEM plots
#'
#' In ggraph, data is passed to a layout function, and the layout object
#' is then passed to [ggraph::ggraph], where ggplot-style aes mappings can be
#' added as layers. We tweak the look of the output plot by i) creating a
#' custom graph layout and ii) passing a lot of hacky information as data columns,
#' that can later be picked up by [ggraph::ggraph] aesthetics, hereafter called
#' "decorating" the data.
single_sem_plot <- function(
    out_sem,
    layout_type,
    labs,
    set_stretch_scale = 0.05,
    decorate_args = NULL,
    plot_args = NULL
) {

  # convert sem output to tidygraph
  gr <- out_sem %>%
    piecewiseSEM:::plot.psem(return = T) %>%  # convert to DiagrammeR::dgr_graph
    dgr_to_tbl_graph

  # manipulate data
  gr_clean <- rlang::inject(
    decorate_sem_graph_data(
      gr,
      labels_variables = labs,
      scale_edge_stretch = set_stretch_scale,
      !!!decorate_args
    )
  )
  # browser()
  # create_custom grid layout
  layout_custom <- gr_clean %>%
    create_custom_layout(layout_type)

  rlang::inject(plot_sem_output(
    layout_custom,
    !!!plot_args
  ))
}

#' Customized call to [ggraph::ggraph] with aes mappings to graphical tweaks
plot_sem_output <- function(
    layout,
    color_col = "color_clean",
    font_size = 5,
    edge_width = 2,
    end_cap_geometry = rectangle(25, 12.5, "mm"),
    node_label_opacity = 1
) {

  # browser()
  layout %>%
    ggraph::ggraph() +
    geom_edge_bend_setjust(
      aes(
        color = !!sym(color_col),
        linetype = style,
        label = label,
        start_cap = rectangle(cap_rect_width, 10, "mm"),
        end_nudge_x = hjust_set,
        start_nudge_x = nudge_start_x,
        label_pos = label_pos
      ),
      arrow = arrow(angle = 25, length = unit(4, "mm"), type = "closed"),
      end_cap = end_cap_geometry,
      label_dodge = unit(0.5*font_size, "mm"),
      check_overlap = F,
      edge_width = edge_width,
      angle_calc = "along",
      # vjust = -1,
      strength = 0.55,
      label_size = 0.75*font_size,
    ) +
    geom_node_label(
      aes(label = label_txt, hjust = hjust_label),
      alpha = node_label_opacity,
      label.r = unit(0, "mm"),
      label.size = 0.5,
      size = font_size
    ) +
    scale_edge_linetype_manual(values = c(solid = "solid", dashed = "solid")) +
    scale_edge_color_manual(
      values = c(unsign. = "grey60", positive = "red", negative = "darkblue")
    ) +
    scale_y_reverse() +
    coord_cartesian(clip = "off") +
    theme(
      legend.position = "none",
      plot.margin = margin(12, 25,12,25, "mm")
    ) +
    theme(
      axis.title =element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.position = "none"
    )
}

#' Define several custom calls to [ggraph::create_layout] layout types
create_custom_layout <- function(graph, type, ...) {
  allowed_types <- c(
    "5 nodes",
    "6 nodes",
    "control_6_nodes",
    "sugiyama",
    "3_drivers"
  )
  if (!(type %in% allowed_types)) {
    stop("Unsupported custom layout ", type)
  }

  if (type == "sugiyama") {
    return(ggraph::create_layout(graph, layout = "sugiyama"))
  }

  if (type == "5 nodes") {
    df_layout <- tibble(
      x = c(1,5,3,2,4),
      y = c(5,5,4,1,1)
    )
  } else if (type == "6 nodes") {
    df_layout <- tibble(
      x = c(3,1,5,3,2,4),
      y = c(6,5,5,3,1,1)
    )
  } else if (type == "control_6_nodes") {
    df_layout = tibble(
      x = c(1,5,3,2,4,3),
      y = c(7,7,5,3,3,1)
    )
  } else if (type == "3_drivers") {
    df_layout = tibble(
      x = c(1,5,3,1,3,5),
      y = c(1,1,2,5,5,5)
    )
  }

  ggraph::create_layout(graph, df_layout, ...)
}

#' Convert a DiagrammeR graph to tidygraph via igraph (yes..)
dgr_to_tbl_graph <- function(dgr) {
  dgr %>%
    DiagrammeR::to_igraph() %>%
    tidygraph::as_tbl_graph()
}

#' Convert list of (label => value) pairs to `label ~ value` syntax
translate_list_to_glued_rules <- function(lbls) {
  if (!any(unlist(map(lbls, is.character)))) {
    glue_terms <- '"{lbl}" ~ {val}'
  } else { # quote everything if there is at least one character
    glue_terms <- '"{lbl}" ~ "{val}"'
  }
  glue::glue(glue_terms, lbl = names(lbls), val = lbls)
}

