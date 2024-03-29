% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/posterior_distributions.R
\name{plot_posterior_distributions}
\alias{plot_posterior_distributions}
\title{Plot posterior distributions}
\usage{
plot_posterior_distributions(
  data,
  mapping = aes(),
  scale = 1,
  vline_type = 1,
  vline_color = "grey90",
  facet_args = list(),
  ...
)
}
\arguments{
\item{data}{A data frame produced by \code{\link[=make_posterior_data]{make_posterior_data()}}.}

\item{mapping}{Default list of aesthetic mappings to use for plot.
See \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} for details.}

\item{scale}{A scaling factor to scale the height of the ridgelines. See
\code{\link[ggridges:geom_ridgeline]{ggridges::geom_ridgeline()}} for details.}

\item{vline_type}{Line type of the line indicating 0.}

\item{vline_color}{Color of the line indicating 0.}

\item{facet_args}{A named list of parameters and arguments to pass on to
\code{\link[ggh4x:facet_grid2]{ggh4x::facet_grid2()}}.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}}.}
}
\description{
Plot posterior distributions.
}
\examples{
\dontrun{
get_files("path_to_dir", vars = "dtr", elevation_span = 2000) |>
  make_posterior_data(yvar = "exclusion_zone") |>
  plot_posterior_distributions(aes(
    x = x,
    y = exclusion_zone,
    height = y,
    color = exclusion_zone,
    fill = exclusion_zone
  ))
}
}
