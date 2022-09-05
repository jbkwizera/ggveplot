#' Draw a horizontal bar chart
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
#' @include utils-pipe.R assets.R
#'
#' @examples
#' df <- data.frame(x = sample(1:10, 20, replace = TRUE))
#' geom_hbar(df, "x")
geom_hbar <- function(data, target, title = NULL, xlab = NULL, ylab = NULL, caption = NULL) {
  # TO-DO: figure out a way to access columns of a data frame as strings
  # Ideas: Use aes_string
  data <- data %>%
    dplyr::group_by(get(target)) %>%
    dplyr::summarize(n = dplyr::n())

  if (!is.factor(data$`get(target)`) & !is.ordered(data$`get(target)`)) {
    data <- data %>%
      dplyr::arrange(n) %>%
      dplyr::mutate(`get(target)` = factor(`get(target)`, ordered = TRUE, levels = `get(target)`))
  }

  (data %>%
      ggplot2::ggplot(ggplot2::aes(`get(target)`, n)) +
      ggplot2::geom_bar(stat = "identity", fill = BLUE, width = 0.667) +
      ggplot2::geom_text(ggplot2::aes(label = n), hjust = -0.5, size = 3) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
      ggplot2::annotate(
        "text", x = data$`get(target)`, y = c(rep(min(data$n)/20, nrow(data))), label = data$`get(target)`,
        size = 3, hjust = "left", color = "white") +
      ggplot2::coord_flip() +
      ggplot2::ggtitle(title) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::labs(caption = caption) +
      base_vetheme +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     axis.text.y = ggplot2::element_blank()))
}