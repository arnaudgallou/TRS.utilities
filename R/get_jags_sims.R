#' @title Extract `sims` data from a JAGS object
#' @description Extract `sims` data from a model object returned by [`run_jags()`]. Requires running [`run_jags()`] with `save=TRUE` and `default_output=FALSE`.
#' @param x A data frame.
#' @param vars Variables to select.
#' @export
get_jags_sims <- function(x, vars) {
  x <- x$sims
  if (!missing(vars)) {
    x <- x[, grepl(vars, names(x), perl = TRUE)]
  }
  x
}
