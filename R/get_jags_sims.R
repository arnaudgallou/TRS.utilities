#' @title Extract the `sims` data from a JAGS object
#' @description Extract the `sims` data from a model object returned by
#'   [`run_jags()`]. Requires running [`run_jags()`] with `save = TRUE` and
#'   `default_output = FALSE`.
#' @param data A data frame.
#' @param vars Variables to select.
#' @export
get_jags_sims <- function(data, vars) {
  data <- data$sims
  if (!missing(vars)) {
    data <- data[, grepl(vars, names(data), perl = TRUE)]
  }
  data
}
