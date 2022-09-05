#' Order a Data Frame by Column Frequency Count
#'
#' @param data A data frame with column named `target`
#' @param target A column to "factorize" and order by frequency of factors
#'
#' @return The data frame with `target` "factorized" and ordered
#' @export
#'
#' @examples
#' df <- data.frame(x = sample(1:10, 20, replace = TRUE))
#' order_factors_by_count(df, "x")
#'
#' @include utils-pipe.R
order_factors_by_count <- function(data, target) {
  if (is.factor(data[[target]]) & is.ordered(data[[target]])) {
    return (data)
  }

  data_ <- data %>%
    dplyr::group_by(.data[[target]]) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::arrange(-n)

  (data %>%
      dplyr::mutate(
        "{target}" := factor(.data[[target]], ordered = TRUE, levels = data_[[target]])))
}
