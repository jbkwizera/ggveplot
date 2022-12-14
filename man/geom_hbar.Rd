% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_hbar.R
\name{geom_hbar}
\alias{geom_hbar}
\title{Draw a horizontal bar chart}
\usage{
geom_hbar(
  data,
  target,
  wt = NULL,
  percent = FALSE,
  percent_format = FALSE,
  dec_places = 1,
  labels_in = FALSE,
  labels_width = 0.9 * getOption("width"),
  monochrome = TRUE,
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  caption = NULL
)
}
\arguments{
\item{data}{Data set with column named \code{target}}

\item{target}{The column to put on the bar chart}

\item{wt}{The weights for tallying}

\item{percent}{Plot the percentages}

\item{percent_format}{Format label as percentage}

\item{dec_places}{Rounding decimal places}

\item{labels_in}{Put labels inside or outside the bars}

\item{labels_width}{The number of characters per line of label text}

\item{monochrome}{Use one color}

\item{title}{Title of the bar chart}

\item{xlab}{x-axis label}

\item{ylab}{y-axis label}

\item{caption}{Caption to go below the x-axis}
}
\value{
The bar chart object
}
\description{
Draw a horizontal bar chart
}
\examples{
df <- data.frame(x = sample(1:10, 20, replace = TRUE))
geom_hbar(df, "x")
}
