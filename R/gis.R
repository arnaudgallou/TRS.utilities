#' @title Read a raster object
#' @description Alias for [`terra::rast`].
#' @param x A single or collection of file names.
#' @param ... Arguments to pass on to [`terra::rast()`].
#' @export
rs_read <- function(x, ...) {
  UseMethod("rs_read")
}

#' @export
rs_read.character <- function(x, ...) {
  terra::rast(x, ...)
}

#' @export
rs_read.collection <- function(x, ...) {
  class(x) <- "character"
  out <- terra::rast(x, ...)
  if (all(have_name(x))) {
    out <- setNames(out, names(x))
  }
  out
}

#' @title Extract values from a raster object
#' @description Alias for [`terra::extract`].
#' @param ... Arguments to pass on to [`terra::extract()`].
#' @export
rs_extract <- function(...) terra::extract(...)

#' @title Crop a raster object
#' @description Alias for [`terra::crop`].
#' @param ... Arguments to pass on to [`terra::crop()`].
#' @export
rs_crop <- function(...) terra::crop(...)

#' @title Resample a spatial object
#' @description Alias for [`terra::resample`].
#' @param ... Arguments to pass on to [`terra::resample()`].
#' @export
rs_resample <- function(...) terra::resample(...)

#' @title Compute zonal statistics
#' @description Alias for [`terra::zonal`].
#' @param ... Arguments to pass on to [`terra::zonal()`].
#' @export
rs_zonal <- function(...) terra::zonal(...)

#' @title Create spatial vector objects
#' @description Alias for [`terra::vect`].
#' @param ... Arguments to pass on to [`terra::vect()`].
#' @export
rs_vect <- function(...) terra::vect(...)

#' @title Convert a raster extent to a polygon
#' @description Convert a raster extent to a polygon.
#' @param x A raster object.
#' @export
rs_ext_to_polygon <- function(x) {
  x <- terra::ext(x)
  x <- terra::as.polygons(x)
  rs_set_crs(x)
}

#' @title Set the minimum and maximum values of a raster object
#' @description Remove missing values and set the minimum and maximum values of
#'   a raster object.
#' @param x A raster object.
#' @export
rs_set_range <- function(x) {
  terra::NAflag(x) <- -1
  terra::setMinMax(x)
}

#' @title Reclassify a raster object
#' @description Divide values of a raster object into bins of a given width.
#' @param x A raster object.
#' @param binwidth The width of the bins.
#' @param right Should bin intervals be closed on the right? See [`terra::classify()`]
#'   for details.
#' @param col_name Name of the classified variable.
#' @param ... Other arguments passed to [`terra::classify()`].
#' @export
rs_reclass <- function(x, binwidth = 100, right = FALSE, col_name = "zone", ...) {
  min <- round_nearest(terra::minmax(x)[1], -binwidth)
  max <- round_nearest(terra::minmax(x)[2], -binwidth)
  s <- seq(min, max, binwidth)
  new_values <- tibble(lower = s, upper = s + binwidth, new = s)
  x <- terra::classify(x, new_values, right = right, ...)
  setNames(x, col_name)
}

#' @title WGS84 coordinate reference system
#' @description WGS84 coordinate reference system.
#' @export
WGS84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#' @title Set the coordinate reference system of a raster object
#' @description Set the coordinate reference system of a raster object.
#' @param x A aster object.
#' @param proj Projection to set to the raster object.
#' @export
rs_set_crs <- function(x, proj = WGS84) {
  terra::crs(x) <- proj
  x
}

#' @title Subset values from a raster object
#' @description Wrapper around [`terra::mask()`] to subset values from a raster
#'   object.
#' @param x A raster object.
#' @param ... Expression that returns a logical value.
#' @export
rs_filter <- function(x, ...) {
  terra::mask(x, mask = ..., maskvalue = 0)
}
