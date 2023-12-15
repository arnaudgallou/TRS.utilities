#' @title Prepare data for regression plots
#' @description Structure data for [`plot_regressions()`].
#' @param files Files to get data from.
#' @param by_land_type Should estimates be calculated by land type?
#' @examples
#' \dontrun{
#' get_files("path_to_dir", vars = c("dtr", "ts"), elevation_span = 2000) |>
#'   make_regression_data()
#' }
#' @export
make_regression_data <- function(files, by_land_type = FALSE) {
  items <- c("data", "sims")
  cls <- if (is_true(by_land_type)) "land_types" else "draws"
  out <- map(items, \(item) {
    if (is_true(by_land_type) && item == "sims") {
      return(calc_pred_conf(files))
    }
    read_jags(files, item)
  })
  out <- set_names(out, items)
  structure(out, class = c(cls, "list"))
}

calc_pred_conf <- function(files) {
  out <- map(files, \(file) {
    data <- read_rds(file)
    fit <- get_jags_sims(data, "beta")
    expl_var <- data$settings$terms[[1]]
    settings <- select(
      data$data,
      .data$expl_var:.data$elevation_span,
      any_of("std_from")
    )
    out <- pluck(data, "elev_grad_clim")
    out <- group_by(out, .data$land_type)
    suppressWarnings(
      out <- modelr::data_grid(
        out,
        value = modelr::seq_range(.data[[expl_var]], n = 100),
        distinct(settings)
      )
    )
    out <- ungroup(out)
    add_pred_conf(out, fit, formula = ~ value * land_type)
  })
  list_rbind(out)
}

#' @title Prepare data for plotting posterior distribution
#' @description Structure data for [`posterior_distributions()`].
#' @param files Files to get data from.
#' @param yvar Variable describing the vertical axis.
#' @param prob The probability mass to include in the shaded region.
#' @param prob_outer The probability mass to include in the outer interval.
#' @param scales A scaling factor to scale the height of the ridgelines. See
#'   [`ggridges::geom_ridgeline()`] for details.
#' @param labels A named vector of labels used for plotting. Names must be the
#'   same as the explanatory variables.
#' @param reverse Should the order of posterior distributions be reversed?
#' @examples
#' \dontrun{
#' get_files("path_to_dir", vars = c("dtr", "ts"), elevation_span = 2000) |>
#'   make_posterior_data(yvar = "exclusion_zone")
#' }
#' @export
make_posterior_data <- function(
    files,
    yvar,
    prob = c(.8, .95),
    prob_outer = .99,
    scales = .01,
    labels = NULL,
    reverse = FALSE
) {
  if (length(scales) > 1L && any(!have_name(scales))) {
    abort("`scales` must be a named vector when using more than one scale.")
  }
  if (!is.null(labels) && any(!have_name(labels))) {
    abort("`labels` must be a named vector.")
  }
  out <- read_jags(files, "sims", vars = "span|zone|expl|beta_2")
  out <- group_by(out, .data$expl_var, .data$elevation_span, .data$exclusion_zone)
  out <- add_posterior_density(out, .data$beta_2, prob, prob_outer)
  out <- ungroup(out)
  out <- mutate(
    out,
    across(starts_with("y"), \(x) rescale(x, scales)),
    !!yvar := relevel(yvar, reverse),
  )
  if (!is.null(labels)) {
    out <- mutate(out, expl_var = relabel(labels))
  }
  out
}

rescale <- function(x, scales) {
  if (length(scales) == 1L) {
    return(x * scales)
  }
  data <- pick(.data$expl_var)
  case_when(!!!parse_exprs(
    glue("data$expl_var == '{names(scales)}' ~ x * {scales}")
  ))
}

relevel <- function(x, reverse) {
  data <- pick(.data[[x]])
  levels <- unique(data[[x]])
  if (reverse) {
    levels <- rev(levels)
  }
  factor(data[[x]], levels = levels)
}

relabel <- function(labels) {
  data <- pick(.data$expl_var)
  out <- string_replace_all(data$expl_var, labels)
  factor(out, levels = labels)
}
