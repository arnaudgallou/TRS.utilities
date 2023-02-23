#' @title Replace matched patterns in a string
#' @description Vectorised pattern replacement using Perl-compatible regular
#'   expressions.
#' @param string A character vector.
#' @param pattern Pattern to look for. A single element vector if `string_replace`
#'   or a named vector of patterns and replacements if `string_replace_all`.
#' @param replacement A character vector of replacement. Only applies to
#'   `string_replace`.
#' @return A character vector.
#' @export
string_replace <- function(string, pattern, replacement) {
  sub(pattern, replacement, string, perl = TRUE)
}

#' @rdname string_replace
#' @param ignore_case Should letter case be ignored? Only applies to `string_replace_all`.
#' @export
string_replace_all <- function(string, pattern, ignore_case = FALSE) {
  for (i in seq_along(pattern)) {
    x <- pattern[i]
    string <- gsub(names(x), x, string, perl = TRUE, ignore.case = ignore_case)
  }
  string
}

#' @title Replace matching patterns in a string
#' @description Vectorised extraction of matching patterns using Perl-compatible
#'   regular expressions.
#' @param string A character vector.
#' @param pattern A pattern to look for.
#' @return A character vector.
#' @export
string_extract <- function(string, pattern) {
  x <- regexpr(pattern, string, perl = TRUE)
  out <- rep(NA, length(string))
  out[x != -1] <- regmatches(string, x)
  out
}

#' @rdname string_extract
#' @return A character vector.
#' @export
string_extract_all <- function(string, pattern) {
  out <- regmatches(string, gregexpr(pattern, string, perl = TRUE))
  if (length(out) > 1L) {
    return(out)
  }
  unlist(out)
}

string_remove <- function(string, pattern) {
  string_replace(string, pattern, "")
}

#' @title Remove non-letter characters, underscore and repeated whitespace inside
#'   a string
#' @description Remove any non-letter characters from the beginning and end of a
#'   character string as well as underscores and repeated whitespaces inside a
#'   string.
#' @param string A character vector.
#' @return A character vector.
#' @export
string_clean <- function(string) {
  patterns <- c(
    r"{\B_\B|(?<!['\"])(?<=[\p{Pe}\p{Po}])(?=\pL)|(?<=\pL)(?=[\p{Ps}&])}" = " ",
    "(?!\\.)(?:^[^\\pL]+|[^\\pL]+$)|_" = "",
    "\\s+" = " "
  )
  string_replace_all(string, patterns)
}

#' @title Convert a string's first character to uppercase
#' @description Convert a string's first character to uppercase.
#' @param string A character vector.
#' @param to_lower Should other characters be converted to lowercase?
#' @return A character vector.
#' @export
uc_first <- function(string, to_lower = FALSE) {
  case <- if (is_true(to_lower)) "L" else "E"
  string_replace(string, "(\\pL)(.*)", paste0("\\U\\1\\", case, "\\2"))
}

#' @title Extract the first word in a character string
#' @description Extract the first word in a character string.
#' @param string A character vector.
#' @param compound_word If `TRUE`, also captures hyphenated compound word.
#' @return A character vector.
#' @export
first_word <- function(string, compound_word = TRUE) {
  h <- if (is_true(compound_word)) "-" else ""
  string_extract(string, glue(r"{^[^\pL]*\K[\pL{h}]+}"))
}

to_snake_case <- function(x) {
  x <- string_replace_all(x, c(
    r"{[^\pL\pN]+|(?<=\p{Lu})(?=\p{Lu}\p{Ll})|(?<=\p{Ll})(?=\p{Lu}|\pN)|(?<=\pN)(?!\pN)}" = "_",
    "^_|_$" = ""
  ))
  tolower(x)
}
