#' Draw a vertical bar chart
#'
#' @param data Data set with column named `target`
#' @param target The column to put on the bar chart
#' @param wt The weights for tallying
#' @param percent Plot the percentages
#' @param percent_format Format label as percentage
#' @param title Title of the bar chart
#' @param dec_places Rounding decimal places
#' @param labels_width The number of characters per line of label text
#' @param monochrome Use one color
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param caption Caption to go below the x-axis
#'
#' @return The bar chart object
#' @export
#' @include utils-pipe.R utils-gg.R utils-data.R
#'
#' @examples
#' df <- data.frame(x = sample(1:4, 20, replace = TRUE))
#' geom_vbar(df, "x")
geom_vbar <- function(
    data, target, wt = NULL, percent = FALSE, percent_format = FALSE,
    title = NULL, dec_places = 1, labels_width = 0.9 * getOption("width"),
    monochrome = TRUE, xlab = NULL, ylab = NULL, caption = NULL) {
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

  (data %>%
      wrap_label_column(target, width = labels_width) %>%
      ggplot2::ggplot(ggplot2::aes(
        .data[[target]], n, fill = .data[[target]])) +
      ggplot2::geom_bar(
        stat = "identity", width = 0.6, show.legend = FALSE) +
      ggplot2::scale_fill_manual(
        values = env_gg$color_set[[ifelse(monochrome, 1, nrow(data))]] %>%
          rep(ifelse(monochrome, nrow(data), 1)),
        aesthetics = c("color", "fill")) +
      ggplot2::geom_text(ggplot2::aes(
        label = ifelse(rep(any(c(percent, percent_format)), nrow(data)),
                       paste0(n, "%"), n)),
        size = 3, vjust = -0.5) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.1))) +
      ggplot2::ggtitle(title) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::labs(caption = caption) +
      base_vetheme +
      ggplot2::theme(axis.text.y = ggplot2::element_blank()))
}
