% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gis.R
\name{rs_reclass}
\alias{rs_reclass}
\title{Reclassify a raster object}
\usage{
rs_reclass(x, binwidth = 100, right = FALSE, col_name = "zone", ...)
}
\arguments{
\item{x}{A raster object.}

\item{binwidth}{The width of the bins.}

\item{right}{Should bin intervals be closed on the right? See \code{\link[terra:classify]{terra::classify()}}
for details.}

\item{col_name}{Name of the classified variable.}

\item{...}{Other arguments passed to \code{\link[terra:classify]{terra::classify()}}.}
}
\description{
Divide values of a raster object into bins of a given width.
}
