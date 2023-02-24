#' @title List files in a directory
#' @description A wrapper around [`list.files()`] that gives the possibility to
#'   name each file directly.
#' @param path A character vector of full path names.
#' @param target An optional [`regular expression`][regex()] to return only file
#'   names that match the regular expression.
#' @param names A vector of names to assign to each file, a function, formula or
#'   regular expression to extract names from file names.
#' @param full.names Should the full path be returned?
#' @param ... Other arguments passed on to [`list.files()`].
#' @return A character vector of class `collection`.
#' @export
list_files <- function(path = ".", target = NULL, names, full.names = TRUE, ...) {
  if (!missing(names)) {
    if (!(is.function(names) || is.character(names))) {
      abort("`names` must be a function or character vector.")
    }
  }
  out <- list.files(path, target, full.names = full.names, ...)
  if (is_empty(out)) {
    abort("Targeted files could not be found.")
  }
  if (!missing(names)) {
    if (is.function(names)) {
      nm <- names(out)
    } else if (is_string(names) && has_metachr(names)) {
      nm <- string_extract(basename(out), names)
    } else {
      nm <- names
    }
    out <- setNames(out, nm)
  }
  structure(out, class = c("collection", "character"))
}

#' @export
print.collection <- function(x, ...) {
  print(x[seq_along(x)], ...)
}

#' @title Extract file names
#' @description Extract the file name from a full path name.
#' @param path A character vector of path names.
#' @param to_snake_case Should the file name be converted to snake case?
#' @export
extract_file_name <- function(path, to_snake_case = FALSE) {
  file_name <- remove_file_ext(basename(path))
  if (is_true(to_snake_case)) {
    file_name <- to_snake_case(file_name)
  }
  file_name
}
