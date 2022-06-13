get_mdl_settings <- function(x) {
  settings <- x$settings
  tibble(
    model = str_remove(deparse(settings$formula), "^~"),
    elevation_span = settings$min_elev_grad_len,
    exclusion_zone = settings$exclusion_zone,
    std_from = settings$std_from
  )
}
