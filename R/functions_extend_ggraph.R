# hacky extension of ggraph::geom_edge_bend to be able to move ('nudge') edge ends and starts

# file contains StatEdgeBendSetNudge and geom_edge_bend_setjust, plus copies of all
# ggraph required internals that allow to make it work.

# proceed at your own risks

StatEdgeBendSetNudge <- ggplot2::ggproto(
  'StatEdgeBendSetNudge',
  ggforce::StatBezier,
  setup_data = function(data, params) {
    data <- StatFilter$setup_data(data, params)
    data <- remove_loop(data)
    if (nrow(data) == 0) return(data)
    data <- data %>%
      mutate(
        xend = xend + end_nudge_x,
        x = x + start_nudge_x
      )
    data$group <- make_unique(data$group)
    data2 <- data
    data2$x <- data2$xend
    data2$y <- data2$yend
    create_bend(data, data2, params)
  },
  required_aes = c('x', 'y', 'xend', 'yend', 'end_nudge_x', 'start_nudge_x', 'circular'),
  default_aes = ggplot2::aes(filter = TRUE),
  extra_params = c('na.rm', 'flipped', 'n', 'strength')
)

#' Hacky extension of [ggraph::geom_edge_bend]
#'
#' Allows to move the x position of edge starts and ends as `star_nudge_x` and `end_nudge_x`
#' aes mappings.
geom_edge_bend_setjust <- function(mapping = NULL, data = get_edges(),
                                   position = 'identity', arrow = NULL, strength = 1,
                                   flipped = FALSE, n = 100, lineend = 'butt',
                                   linejoin = 'round', linemitre = 1,
                                   label_colour = 'black', label_alpha = 1,
                                   label_parse = FALSE, check_overlap = FALSE,
                                   angle_calc = 'rot', force_flip = TRUE,
                                   label_dodge = NULL, label_push = NULL,
                                   show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes(
    x = x, y = y, xend = xend, yend = yend, end_nudge_x = end_nudge_x, start_nudge_x = start_nudge_x,
    circular = circular, group = edge.id
  ))

  layer(
    data = data, mapping = mapping, stat = StatEdgeBendSetNudge,
    geom = ggraph::GeomEdgePath, position = position, show.legend = show.legend,
    inherit.aes = FALSE,
    params = expand_edge_aes(
      list2(
        arrow = arrow, lineend = "round", linejoin = linejoin,
        linemitre = linemitre, n = n,
        interpolate = FALSE, flipped = flipped, strength = strength,
        label_colour = label_colour, label_alpha = label_alpha,
        label_parse = label_parse, check_overlap = check_overlap,
        angle_calc = angle_calc, force_flip = force_flip,
        label_dodge = label_dodge, label_push = label_push, ...
      )
    )
  )
}

#### Functions imported from ggraph to create geom_edge_bend_hjust

expand_edge_aes <- function(x) {
  short_names <- names(x) %in% c(
    'colour', 'color', 'fill', 'linetype', 'shape', 'size', 'width', 'alpha', 'linewidth'
  )
  names(x)[short_names] <- paste0('edge_', names(x)[short_names])
  if (all(c('edge_linewidth', 'edge_width') %in% names(x) == c(TRUE, FALSE))) {
    names(x)[names(x) == 'edge_linewidth'] <- 'edge_width'
  }
  x
}

aes_intersect <- function(aes1, aes2) {
  aes <- c(as.list(aes1), aes2[!names(aes2) %in% names(aes1)])
  class(aes) <- 'uneval'
  aes
}

complete_edge_aes <- function(aesthetics) {
  if (is.null(aesthetics)) {
    return(aesthetics)
  }
  if (any(names(aesthetics) == 'color')) {
    names(aesthetics)[names(aesthetics) == 'color'] <- 'colour'
  }
  expand_edge_aes(aesthetics)
}

remove_loop <- function(data) {
  if (nrow(data) == 0) return(data)

  data[!(data$x == data$xend & data$y == data$yend), , drop = FALSE]
}



# non-orderaltering version of make.unique
make_unique <- function(x, sep = '.') {
  if (!anyDuplicated(x)) return(x)
  groups <- match(x, unique0(x))
  suffix <- unsplit(lapply(split(x, groups), seq_along), groups)
  max_chars <- nchar(max(suffix))
  suffix_format <- paste0('%0', max_chars, 'd')
  paste0(x, sep, sprintf(suffix_format, suffix))
}

# Wrapping unique0() to accept NULL
unique0 <- function(x, ...) if (is.null(x)) x else vec_unique(x, ...)

create_bend <- function(from, to, params) {
  bezier_start <- seq(1, by = 4, length.out = nrow(from))
  from$index <- bezier_start
  to$index <- bezier_start + 3
  data2 <- from
  data3 <- to
  data2$index <- bezier_start + 1
  data3$index <- bezier_start + 2
  if (any(from$circular)) {
    r0 <- sqrt(from$x[from$circular]^2 + from$y[from$circular]^2)
    r1 <- sqrt(to$x[to$circular]^2 + to$y[to$circular]^2)
    root <- r0 == 0 | r1 == 0

    dotprod <- from$x * to$x + from$y * to$y

    crossing <- r0 * r0 / dotprod
    cross_x <- to$x * crossing
    cross_y <- to$y * crossing

    data2$x[from$circular] <- from$x + (cross_x - from$x) * params$strength
    data2$y[from$circular] <- from$y + (cross_y - from$y) * params$strength
    data3$x[from$circular] <- to$x + (cross_x - to$x) * params$strength
    data3$y[from$circular] <- to$y + (cross_y - to$y) * params$strength

    data2$x[root] <- from$x[root]
    data2$y[root] <- from$y[root]
    data3$x[root] <- to$x[root]
    data3$y[root] <- to$y[root]
  }
  if (any(!from$circular)) {
    if (params$flipped) {
      h_diff <- from$x[!from$circular] - to$x[!from$circular]
      w_diff <- from$y[!from$circular] - to$y[!from$circular]
      data2$y[!from$circular] <- from$y[!from$circular] - w_diff * params$strength
      data3$x[!from$circular] <- to$x[!from$circular] + h_diff * params$strength
    } else {
      h_diff <- from$y[!from$circular] - to$y[!from$circular]
      w_diff <- from$x[!from$circular] - to$x[!from$circular]
      data2$x[!from$circular] <- from$x[!from$circular] - w_diff * params$strength
      data3$y[!from$circular] <- to$y[!from$circular] + h_diff * params$strength
    }
  }
  data <- vctrs::vec_rbind(from, data2, data3, to)
  data[order(data$index), names(data) != 'index']
}
