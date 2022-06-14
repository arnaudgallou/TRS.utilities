get_mdl_settings <- function(x) {
  settings <- x$settings
  tibble(
    model = string_replace(deparse(settings$formula), "^~", ""),
    elevation_span = settings$elevation_span,
    exclusion_zone = settings$exclusion_zone,
    std_from = settings$std_from
  )
}
