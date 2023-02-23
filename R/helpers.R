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
  (x - mean(x, na.rm = TRUE)) / (2 * sd(x, na.rm = TRUE))
}

#' @title Compute the difference between consecutive values
#' @description Compute the difference between the first element of a numeric vector and all consecutive values.
#' @param x Numeric vector.
#' @export
delta <- function(x) {
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
  string_extract_all(deparse(x), "\\w+")
}

#' @title Parse numeric values from a string
#' @description Extract numeric values from a character string.
#' @param x A character string.
#' @param as A character vector describing the type of output values. One of
#'   `numeric`, `integer` or `character`.
#' @param all Should all numeric values be extracted? By default, parses the first
#'   numeric value.
#' @export
parse_num <- function(x, as = c("numeric", "integer", "character"), all = FALSE) {
  as <- match.arg(as)
  as <- switch(as, "numeric" = as.numeric, "integer" = as.integer, NULL)
  extract_args <- list(string = x, pattern = "\\d+(?:[.,]\\d+)?")
  if (isTRUE(all)) {
    extract <- string_extract_all
    extract_args <- c(extract_args, list(simplify = TRUE))
  } else {
    extract <- string_extract
  }
  x <- do.call(extract, extract_args)
  x <- string_replace_all(x, c("," = "."))
  if (is.null(as)) x else as(x)
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
  grepl(r"{[\\\[\](){}|?$^*+]}", x, perl = TRUE)
}

to_chr_class <- function(vec, negate = FALSE) {
  assert_that(is.character(vec))
  neg <- if (isTRUE(negate)) "^" else ""
  x <- paste(vec, collapse = "")
  x <- string_replace(x, r"{([-\\\[\]])}", r"{\\\1}")
  paste0("[", neg, x, "]")
}
