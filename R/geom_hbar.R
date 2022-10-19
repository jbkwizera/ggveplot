#' Draw a horizontal bar chart
#'
#' @param data Data set with column named `target`
#' @param target The column to put on the bar chart
#' @param wt The weights for tallying
#' @param percent Plot the percentages
#' @param percent_format Format label as percentage
#' @param dec_places Rounding decimal places
#' @param labels_in Put labels inside or outside the bars
#' @param labels_width The number of characters per line of label text
#' @param title Title of the bar chart
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param caption Caption to go below the x-axis
#'
#' @return The bar chart object
#' @export
#' @include utils-pipe.R utils-gg.R utils-data.R
#'
#' @examples
#' df <- data.frame(x = sample(1:10, 20, replace = TRUE))
#' geom_hbar(df, "x")
geom_hbar <- function(
    data, target, wt = NULL, percent = FALSE, percent_format = FALSE,
    dec_places = 1, labels_in = FALSE, labels_width = 0.9 * getOption("width"),
    title = NULL, xlab = NULL,
    ylab = NULL, caption = NULL) {
  if (is.null(wt)) {
    data$wt <- 1
  } else {
    data$wt <- data[[wt]]
  }

  data <- order_factors_by_count(data, target, wt = "wt") %>%
    dplyr::count(.data[[target]], wt = .data$wt)

  if (percent) {
    data <- data %>%
      dplyr::mutate(n = 100*n/sum(n))
  }
  data$n <- round(data$n, dec_places)

  labels_annotation <- ggplot2::annotate(
    "text", x = data[[target]], y = c(rep(min(data$n)/20, nrow(data))),
    label = data[[target]], size = 3, hjust = "left", color = "white")

  gg_layer <- data %>%
    wrap_label_column(target, width = labels_width) %>%
    ggplot2::ggplot(ggplot2::aes(stats::reorder(.data[[target]], n, rev), n)) +
    ggplot2::geom_bar(
      stat = "identity", fill = env_gg$color_set[[1]], width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(
      label = ifelse(rep(any(c(percent, percent_format)), nrow(data)),
                     paste0(n, "%"), n)),
      hjust = -0.125, size = 3) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::coord_flip() +
    ggplot2::ggtitle(title) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::labs(caption = caption) +
    base_vetheme +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  if (!labels_in) {
    return (gg_layer)
  } else {
    return (gg_layer + labels_annotation + ggplot2::theme(
      axis.text.y = ggplot2::element_blank()))
  }
}
