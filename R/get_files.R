#' @title Fetch files
#' @description
#' Fetch files produced by [`run_jags()`].
#' @param dir Directory fetch files from.
#' @param vars Explanatory variables to use.
#' @param elevation_span Elevation spans to use.
#' @param exclusion_zone Exclusion zones to use.
#' @param std_from Side of standardization to use. One of `top`, `bottom` or
#'   `none`.
#' @param land_type If `TRUE`, select files from analyses performed by land type.
#' @param ... Other arguments passed to [`list_files()`].
#' @returns A vector of file dirs with class `collection`.
#' @examples
#' \dontrun{
#' get_files(
#'   "path_to_dir",
#'   vars = c("dtr", "ts"),
#'   elevation_span = 2000
#' )
#' }
#' @export
get_files <- function(
    dir,
    vars = NULL,
    elevation_span = NULL,
    exclusion_zone = NULL,
    std_from = c("top", "bottom", "none"),
    land_type = FALSE,
    ...
) {
  std_from <- match.arg(std_from)
  pattern <- lf_pattern(vars, land_type, elevation_span, exclusion_zone, std_from)
  list_files(dir, pattern, ...)
}

lf_pattern <- function(vars, land_type, span, excl, std_from, tail) {
  vars <- lfarg(vars)
  span <- lfarg(span)
  excl <- lfarg(excl)
  land_type <- if (land_type) "-land_type" else ""
  tail <- if (std_from != "none") glue("-{std_from}") else ""
  glue("{vars}{land_type}-span_{span}-excl_{excl}{tail}\\.rds")
}

lfarg <- function(x) {
  if (is.null(x)) {
    "[^-]+"
  } else if (length(x) > 1L) {
    sprintf("(?:%s)", paste(x, collapse = "|"))
  } else {
    x
  }
}
