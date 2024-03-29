% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_composition_heatmap.R
\name{draw_composition_heatmap}
\alias{draw_composition_heatmap}
\title{Composition heatmap}
\usage{
draw_composition_heatmap(
  composition,
  cat1,
  cat2,
  categorical_values1,
  categorical_values2
)
}
\arguments{
\item{composition}{Tibble. Table showing the composition of a test.}

\item{cat1}{Character. First dimension selected.}

\item{cat2}{Character. Second dimension selected.}

\item{categorical_values1}{Tibble. Categories associated to the first dimension selected.}

\item{categorical_values2}{Tibble. Categories associated to the second dimension selected.}
}
\value{
A ggplot object ready for rendering.
}
\description{
Function creating heatmap showing the composition of a test.
}
\author{
Nicolas Mangin
}
