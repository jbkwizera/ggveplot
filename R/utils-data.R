#' Order a Data Frame by Column Frequency Count
#'
#' @param data A data frame with column named `target`
#' @param target A column to "factorize" and order by frequency of factors
#' @param wt Name of column with weights to calculate frequencies
#'
#' @return The data frame with `target` "factorized" and ordered
#' @export
#'
#' @examples
#' df <- data.frame(x = sample(1:10, 20, replace = TRUE))
#' order_factors_by_count(df, "x")
#'
#' @include utils-pipe.R
order_factors_by_count <- function(data, target, wt = NULL) {
  if (is.null(wt)) {
    data[["wt"]] <- 1
    wt <- "wt"
  }

  if (is.factor(data[[target]]) & is.ordered(data[[target]])) {
    return (data)
  }

  data_ <- data %>%
    dplyr::count(.data[[target]], wt = .data[[wt]]) %>%
    dplyr::arrange(-n)

  (data %>%
      dplyr::mutate(
        "{target}" := factor(.data[[target]], ordered = TRUE, levels = data_[[target]])))
}

#' Break Text Into Lines by Width
#'
#' @param data Data
#' @param target Variable of character type to wrap
#' @param width Line width
#' @keywords internal
#' @return Data with modified target
#' @noRd
wrap_label_column <- function(data, target, width = 0.9 * getOption("width")) {
  (data %>%
     dplyr::mutate("{target}" := sapply(
       data[[target]], function(x) {
         paste(strwrap(x, width), collapse = "\n") })))
}
