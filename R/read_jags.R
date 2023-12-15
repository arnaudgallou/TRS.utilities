#' @title Read JAGS custom output files
#' @description Read model object files returned by [`run_jags()`]. Requires
#'   running `run_jags()` with `save = TRUE` and `default_output = FALSE`.
#' @param x A `.rds` file or a collection of `.rds` files.
#' @param pluck Object to be extracted from a JAGS output.
#' @param vars Columns to select or model parameters to filter from the `sims`
#'   or `summary` data, respectively.
#' @param ... Arguments passed on to methods.
#' @param names_to Either a string or `NULL`. See [`purrr::list_rbind()`] for
#'   details.
#' @examples
#' \dontrun{
#' get_files("path_to_dir", vars = "dtr") |> read_jags("data")
#' }
#' @export
read_jags <- function(x, pluck, vars = NULL, ...) {
  UseMethod("read_jags")
}

#' @rdname read_jags
#' @export
read_jags.character <- function(x, pluck, vars = NULL, ...) {
  if (!is.null(vars) && !(pluck %in% c("summary", "sims"))) {
    abort("`vars` can only be used with `summary` or `sims`.", call = caller_env())
  }
  x <- read_rds(x)
  x <- x[[pluck]]
  if (!is.null(vars)) {
    y <- if (pluck == "summary") x$parameter else names(x)
    matched <- grepl(vars, y, perl = TRUE)
    x <- if (pluck == "summary") x[matched, ] else x[, matched]
  }
  rowid_to_column(x)
}

#' @rdname read_jags
#' @export
read_jags.collection <- function(x, pluck, vars = NULL, ..., names_to = NULL) {
  out <- map(x, \(file) read_jags(file, pluck, vars))
  list_rbind(out, names_to = names_to)
}
