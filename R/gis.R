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
  if (has_names(x)) {
    out <- setNames(out, names(x))
  }
  out
}


#' @title Get or set the extent of a raster object
#' @description Alias for [`terra::ext`].
#' @param ... Arguments to pass on to [`terra::ext()`].
#' @export
rs_extent <- function(...) terra::ext(...)


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


#' @title Reproject a spatial object
#' @description Alias for [`terra::project`].
#' @param ... Arguments to pass on to [`terra::project()`].
#' @export
rs_project <- function(...) terra::project(...)


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
  x <- rs_extent(x)
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


#' @title Reclassify a DEM object
#' @description Divide values of a DEM object into bins of a given width.
#' @param x A raster object.
#' @param binwidth The width of the bins.
#' @param right Should bin intervals be closed on the right?
#'   See [`terra::classify()`] for details.
#' @export
rs_reclass_dem <- function(x, binwidth = 100, right = FALSE) {
  min <- round_nearest(terra::minmax(x)[1], -binwidth)
  max <- round_nearest(terra::minmax(x)[2], -binwidth)
  s <- seq(min, max, binwidth)
  new_values <- tibble(lower = s, upper = s + binwidth, new = s)
  x <- terra::classify(x, new_values, right = right)
  setNames(x, "zone")
}


#' @title Read a multi-layer raster file
#' @description Read a multi-layer raster file.
#' @param file A file containing a raster stack.
#' @param layer_names A character vector containing the names to assign to each
#'   raster layer.
#' @export
rs_read_stk <- function(file, layer_names = NULL) {
  x <- terra::rast(file)
  setNames(x, layer_names)
}


#' @title Set the coordinate reference system of a raster object
#' @description Set the coordinate reference system of a raster object.
#' @param x A aster object.
#' @param proj Projection to set to the raster object.
#' @export
rs_set_crs <- function(x, proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") {
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
