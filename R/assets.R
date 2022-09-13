LIGHTGRAY <- c("#F2F2F2")
BLUE      <- c("#097ABC")
LIGHTBLUE <- c("#9DC3E5")
DARKBLUE  <- c("#1B2F55")
GREEN     <- c("#8CCC98")
DARKGRAY  <- c("#7F7F7F")
DARKGREEN <- c("#49711E")

COLOR_SET <- list(
  ONE = c(BLUE),
  TWO = c(BLUE, LIGHTBLUE),
  THREE = c(BLUE, LIGHTBLUE, DARKBLUE),
  FOUR = c(BLUE, GREEN, LIGHTBLUE, DARKBLUE),
  FIVE = c(BLUE, LIGHTBLUE, GREEN, DARKGREEN, DARKBLUE))

MPALETTE  <- c("#04477d", "#1b6595", "#3883ab", "#59a2bf", "#7cc1d3", "#a3e0e8",
               "#a1f0fc", "#74e0fd", "#3fceff", "#00bbff", "#00a6ff", "#008eff")

choose_shades <- function(pal = MPALETTE, n = Inf) {
  return (pal[seq(1, length(pal), length.out = min(n, length(pal)))])
}

A4W_IN <- 6.5

base_vetheme <-
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.caption = ggplot2::element_text(hjust = 0.5),
    plot.background = ggplot2::element_rect(fill = LIGHTGRAY, color = "lightgray"),
    panel.background = ggplot2::element_rect(fill = LIGHTGRAY),
    panel.grid = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.line.x.bottom = ggplot2::element_line(size = 0.2, color = "lightgray"))
