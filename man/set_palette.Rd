% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-gg.R
\name{set_palette}
\alias{set_palette}
\title{Set a custom palette}
\usage{
set_palette(palette = ve_palette)
}
\arguments{
\item{palette}{A list of vectors of color sets to be used}
}
\value{
The palette as invisible if successful otherwise aborts with error
}
\description{
Set a custom palette
}
\examples{
palette <- list(c("red"), c("red", "green"), c("red", "green", "blue"))
ggveplot::set_palette(palette)
}
