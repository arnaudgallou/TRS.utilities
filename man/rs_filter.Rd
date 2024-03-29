% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gis.R
\name{rs_filter}
\alias{rs_filter}
\title{Subset values from a raster object}
\usage{
rs_filter(x, ...)
}
\arguments{
\item{x}{A raster object.}

\item{...}{Expression that returns a logical value.}
}
\description{
Wrapper around \code{\link[terra:mask]{terra::mask()}} to subset values from a raster
object.
}
