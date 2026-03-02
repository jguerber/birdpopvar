scale_habitats <- function(
    geom = "color",
    labs =  c(
      farmland = "#FFB200",
      other = "grey40",
      woodland = "#119C1E",
      built = "#D85B88",
      all = "grey10"
    ),
    ...
) {
  if (geom == "color") {
    return(
      scale_color_manual(values = labs, ...)
    )
  }  else if (geom == "fill") {
    return(
      scale_fill_manual(values = labs, ...)
    )
  } else {
    stop("Unmatched geom provided")
  }
}

#' https://arelbundock.com/posts/quarto_figures/index.html
out2fig <- function(out.width, out.width.default = 0.6, fig.width.default = 6) {
  fig.width.default * out.width / out.width.default
}

#' Save a figure then return it
#' 
#' @param fig ggplot2 figure
#' @param filename file path for ggsave
#' @param ... arguments to ggplot2::ggsave. Match chunk parameters with units="in"
#' if you want to reproduce a quarto output
save_and_output <- function(fig, filename, ...) {

  ggsave(
    filename = filename,
    plot = fig,
    ...
  )

  fig
}
