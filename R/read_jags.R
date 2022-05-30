#' @title Read JAGS custom output files
#' @description Read model object files returned by [`run_jags()`]. Requires running `run_jags()` with `save=TRUE` and `default_output=FALSE`.
#' @param x A `.rds` file or a collection of `.rds` files.
#' @param pluck Object to be extracted from a JAGS output.
#' @param vars Columns to select or model parameters to filter from the `sims` or `summary` data, respectively.
#' @param ... Arguments passed on to methods.
#' @param .id Either a string or `NULL`. See [`purrr::map_df()`] for details.
#' @export
read_jags <- function(x, pluck, vars = NULL, ...) {
  UseMethod("read_jags")
}

#' @rdname read_jags
#' @export
read_jags.character <- function(x, pluck, vars = NULL, ...) {
  ReadJAGS(x, pluck, vars)
}

#' @rdname read_jags
#' @export
read_jags.collection <- function(x, pluck, vars = NULL, ..., .id = NULL) {
  map_df(x, ~ {
    .x <- ReadJAGS(.x, pluck, vars)
    tibble::rowid_to_column(.x)
  }, .id = .id)
}


ReadJAGS <- function(x, pluck, vars) {
  if (!is.null(vars) && !(pluck %in% c("summary", "sims"))) {
    stop("'vars' can only be used with 'summary' or 'sims'.")
  }

  x <- read_rds(x)
  x <- x[[pluck]]

  if (!is.null(vars)) {
    y <- if (pluck == "summary") x$parameter else names(x)
    matched <- grepl(vars, y, perl = TRUE)
    x <- if (pluck == "summary") x[matched, ] else x[, matched]
  }

  x
}
