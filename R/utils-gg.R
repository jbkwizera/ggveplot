lightgray <- "#F2F2F2"
blue      <- "#097ABC"
lightblue <- "#9DC3E5"
darkblue  <- "#1B2F55"
green     <- "#8CCC98"
darkgray  <- "#7F7F7F"
darkgreen <- "#49711E"

ve_palette <- list(
  c("#097ABC"),
  c("#097ABC", "#9DC3E5"),
  c("#097ABC", "#9DC3E5", "#1B2F55"),
  c("#097ABC", "#8CCC98", "#9DC3E5", "#1B2F55"),
  c("#097ABC", "#9DC3E5", "#8CCC98", "#49711E", "#1B2F55"))

env_gg <- new.env(parent = emptyenv())
assign("a4w_in", 6.5, env_gg)
assign("color_set", ve_palette, env_gg)

base_vetheme <- ggplot2::theme(
  plot.title = ggplot2::element_text(hjust = 0.5),
  plot.caption = ggplot2::element_text(hjust = 0.5),
  plot.background = ggplot2::element_rect(
    fill = lightgray, color = "lightgray"),
  panel.background = ggplot2::element_rect(fill = lightgray),
  panel.grid = ggplot2::element_blank(),
  axis.ticks.y = ggplot2::element_blank(),
  axis.ticks.x = ggplot2::element_blank(),
  axis.line.x.bottom = ggplot2::element_line(
    size = 0.2, color = "lightgray"))

#' Set a custom palette
#' @param palette A list of vectors of color sets to be used
#' @return The palette as invisible if successful otherwise aborts with error
#'
#' @export
#'
#' @examples
#' palette <- list(c("red"), c("red", "green"), c("red", "green", "blue"))
#' ggveplot::set_palette(palette)
set_palette <- function(palette = ve_palette) {
  stopifnot(all(sapply(palette, function(vec) length(unique(vec)))))
  assign("color_set", palette, env_gg)
  invisible(env_gg$color_set)
}
