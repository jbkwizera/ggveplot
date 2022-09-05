geom_vbar_fill <- function(data, var_main, var_fill, title = NULL, xlab = NULL, ylab = NULL, caption = NULL) {
  data <- data %>%
    dplyr::group_by(.data[[var_main]], .data[[var_fill]]) %>%
    dplyr::summarize(n = dplyr::n(), .groups = "drop_last") %>%
    dplyr::mutate(percent = round(100*n/sum(n)))

  (data %>%
      ggplot2::ggplot(aes(.data[[var_main]], percent, fill = .data[[var_fill]])) +
      ggplot2::geom_bar(stat = "identity", position = "fill", width = 0.6) +
      ggplot2::geom_text(
        aes(label = paste0(round(percent), "%")),
        position = ggplot2::position_fill(vjust = 0.5), size = 3, color = "white") +
      ggplot2::scale_fill_manual(
        values = COLOR_SET[[length(unique(data[[var_fill]]))]], aesthetics = c("color", "fill")) +
      ggplot2::scale_y_continuous(
        labels = scales::percent, expand = ggplot2::expansion(mult = c(0, 0.1))) +
      ggplot2::ggtitle(title) +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(NULL) +
      ggplot2::labs(fill = NULL) +
      base_vetheme +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        plot.caption = ggplot2::element_text(hjust = 0.5),
        axis.text.y = ggplot2::element_blank(),
        legend.margin = ggplot2::margin(2, 2, 2, 2),
        legend.text = ggplot2::element_text(size = 8, vjust = 0.5),
        legend.key.size = ggplot2::unit(8, "pt"),
        legend.direction = "horizontal",
        legend.position = c(0.5, 0.97),
        legend.justification = "center"))
}
