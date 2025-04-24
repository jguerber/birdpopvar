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
