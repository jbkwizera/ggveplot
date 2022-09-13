#' Draw A Vertical Bar Chart with Fill Column
#'
#' @param data A data frame with variables `var_main` and `var_fill`
#' @param var_main The main variable to go on the x-axis
#' @param var_fill The fill variable to go on the y-axis in the bars
#' @param wt The weights for tallying
#' @param percent Plot the percentages
#' @param title The title of the chart
#' @param xlab The label of the x-axis
#' @param ylab The label of the y-axis
#' @param caption Optional caption for information
#'
#' @return A filled (2d) vertical bar chart
#' @export
#'
#' @examples
#' library(ggplot2)
#' geom_vbar_dodge(mpg, "class", "drv")
#' @include utils-pipe.R assets.R utils-data.R
geom_vbar_dodge <- function(data, var_main, var_fill, wt = NULL, percent = FALSE, title = NULL, xlab = NULL, ylab = NULL, caption = NULL) {
  if (is.null(wt)) {
    data$wt <- 1
  } else {
    data$wt <- data[[wt]]
  }

  data <- order_factors_by_count(data, var_main, wt = "wt") %>%
    dplyr::group_by(.data[[var_main]], .data[[var_fill]]) %>%
    dplyr::summarize(n = dplyr::n(), .groups = "drop_last") %>%
    dplyr::mutate(percent = round(100*n/sum(n)))

  (data %>%
      ggplot2::ggplot(
        aes(.data[[var_main]], n, fill = .data[[var_fill]], group = .data[[var_fill]])) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::geom_text(
        aes(label = paste0(round(percent), "%")),
        position = ggplot2::position_dodge(0.9), vjust = -0.5, size = 3) +
      ggplot2::scale_fill_manual(
        values = choose_shades(MPALETTE, length(unique(data[[var_fill]]))),
        aesthetics = c("color", "fill")) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
      ggplot2::ggtitle(title) +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(NULL) +
      ggplot2::labs(fill = NULL) +
      base_vetheme +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        plot.caption = ggplot2::element_text(hjust = 0.5),
        axis.line.y.left = ggplot2::element_line(color = "lightgray", size = 0.2),
        axis.ticks.y.left = ggplot2::element_line(color = "lightgray", size = 0.2),
        legend.margin = ggplot2::margin(2, 2, 2, 2),
        legend.text = ggplot2::element_text(size = 8, vjust = 0.5),
        legend.key.size = ggplot2::unit(8, "pt"),
        legend.direction = "horizontal",
        legend.position = c(0.5, 0.97),
        legend.justification = "center"))
}
