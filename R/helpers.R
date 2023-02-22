get_mdl_settings <- function(x) {
  settings <- x$settings
  tibble(
    model = string_replace(deparse(settings$formula), "^~", ""),
    elevation_span = settings$elevation_span,
    exclusion_zone = settings$exclusion_zone,
    std_from = settings$std_from
  )
}

discard <- function(x, y) {
  x[!x %in% y]
}

name_suffix <- function(x) {
  replace(seq(x), 1, "")
}

#' @title Round up or down a number to the nearest value
#' @description Round up or down a number to the nearest value.
#' @param x A numeric vector.
#' @param nearest A numeric value indicating the nearest value to round by. If
#'   negative, will round down to the nearest value. If positive, will round up
#'   to the nearest value.
#' @export
round_nearest <- function(x, nearest = -10) {
  assert_that(is.numeric(x))
  nearest <- -nearest
  (x %/% nearest) * nearest
}

#' @title Compute proportion
#' @description Context dependent expression that returns the current proportion
#'   of values that sastify a condition. See [`dplyr::context()`] for details.
#' @param condition Logical vectors.
#' @export
proportion <- function(condition) {
  sum(condition, na.rm = TRUE) * 100 / n()
}

#' @title Center and standardize data
#' @description Center and standardize data.
#' @param x A numeric vector.
#' @export
standardize <- function(x) {
  assert_that(is.numeric(x))
  (x - mean(x, na.rm = TRUE)) / (2 * sd(x, na.rm = TRUE))
}

#' @title Compute the difference between consecutive values
#' @description Compute the difference between the first element of a numeric vector and all consecutive values.
#' @param x Numeric vector.
#' @export
delta <- function(x) {
  assert_that(is.numeric(x))
  x - lag(x, n = length(x), default = x[1])
}

round_num <- function(x, digits = 2) {
  mutate(x, across(vars_select_helpers$where(is.double), round, digits))
}

#' @title Parse formulas
#' @description Extract terms from a formula.
#' @param x A formula.
#' @return A character vector.
#' @export
parse_formula <- function(x) {
  assert_that(is_formula(x))
  string_extract_all(deparse(x), "\\w+", simplify = TRUE)
}

#' @title Parse numeric values from a string
#' @description Wrapper around [`dir.create()`] that creates directories recursively.
#' @param path A character vector containing a single path name.
#' @param ... Other arguments passed on to [`dir.create()`].
#' @export
make_dir <- function(path, ...) {
  dir.create(path, recursive = TRUE, ...)
}

#' @title Create named lists dynamically
#' @description Wrapper around [`dots_list()`][rlang::dots_list] with
#'   `.named = TRUE` as default.
#' @param ... Objects to pass on to [`dots_list()`][rlang::dots_list].
#' @export
make_list <- function(...) {
  dots_list(..., .named = TRUE)
}

remove_file_ext <- function(file) {
  string_remove(file, "\\.[^.]+$")
}

#' @title Check for metacharacters
#' @description Test whether a string contains metacharacters or not.
#' @param x A character string.
#' @return A boolean.
#' @export
has_metachr <- function(x) {
  assert_that(is.string(x))
  grepl(r"{[\\\[\](){}|?$^*+]}", x, perl = TRUE)
}
