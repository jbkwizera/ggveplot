#' Draw a vertical bar chart
#'
#' @param data Data set with column named `target`
#' @param target The column to put on the bar chart
#' @param title Title of the bar chart
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param caption Caption to go below the x-axis
#'
#' @return The bar chart object
#' @export
#' @include utils-pipe.R assets.R utils-data.R
#'
#' @examples
#' df <- data.frame(x = sample(1:10, 20, replace = TRUE))
#' geom_vbar(df, "x")
geom_vbar <- function(data, target, title = NULL, xlab = NULL, ylab = NULL, caption = NULL) {
  data <- data %>%
    order_factors_by_count(target) %>%
    dplyr::count(.data[[target]])

  (data %>%
      ggplot2::ggplot(ggplot2::aes(.data[[target]], n)) +
      ggplot2::geom_bar(stat = "identity", fill = BLUE, width = 0.6) +
      ggplot2::geom_text(ggplot2::aes(label = n), size = 3, vjust = -0.5) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
      ggplot2::ggtitle(title) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::labs(caption = caption) +
      base_vetheme +
      ggplot2::theme(axis.text.y = ggplot2::element_blank()))
}
