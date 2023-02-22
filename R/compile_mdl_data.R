#' @title Compile model data
#' @description Compile data for a JAGS model.
#' @param x A data frame containing data to pass on to a JAGS model.
#' @param clim_data A data frame containing climate data. Only to be specified if
#'   `average=TRUE`.
#' @param elevation_span Minimum length of the elevational gradients to filter.
#' @param exclusion_zone Length of the upper and lower sections of the elevational
#'   gradient to discard species from. E.g. an exclusion zone of 250 will discard
#'   all species found exclusively in the lower and upper 250 m of elevational
#'   gradients. All species with at least one observation outside of the exclusion
#'   zone are kept.
#' @param singleton_thr Maximum proportion of single observations to filter
#'   location with.
#' @param std_elev_grad Should elevational gradients be standardised?
#' @param average Should species range sizes and climate data averaged?
#' @param cols A vector of column names to keep. Variables passed to `expr` will
#'   be automatically kept.
#' @param rename_bioclim If `TRUE` (the default), will rename "bio1", "bio2",
#'   "bio4" and "bio12" as "mat", "dtr", "ts" and "ap", respectively.
#' @param expr A character vector (for the local-scale analysis) or a formula
#'   (for the global-scale analysis) describing the predictor variable(s) to fit
#'   the model.
#' @param std_from Edge on the elevational gradient to standardize from. One of
#'   `c("top", "bottom")`.
#' @return A list of JAGS-compatible data.
#' @export
compile_mdl_data <- function(
    x,
    clim_data = NULL,
    elevation_span,
    exclusion_zone,
    singleton_thr,
    std_elev_grad = FALSE,
    average = FALSE,
    cols = NULL,
    rename_bioclim = TRUE,
    expr = NULL,
    std_from = c("top", "bottom")
) {
  if (missing(clim_data) && is_true(average)) {
    abort("`clim_data` is required when `average = TRUE`.")
  }
  std_from <- if (is_true(std_elev_grad)) match.arg(std_from) else NULL
  if (is_formula(expr)) {
    formula <- expr
    terms <- parse_formula(expr)
  } else {
    formula <- NULL
    terms <- expr
  }

  x <- filter_locations(x, elevation_span, singleton_thr)
  if (is_true(std_elev_grad)) {
    x <- std_elev_grad(x, length = elevation_span, from = std_from)
  }
  if (!missing(exclusion_zone)) {
    x <- filter_species(x, exclusion_zone = exclusion_zone)
  }
  x <- all_data <- drop_na(x, starts_with("bio")) |>
    mutate(sp_range = {
      out <- if_else(.data$sp_range == 0, 10, .data$sp_range)
      log(out)
    })

  cols <- unique(c(cols, "location", "land_type", "sp_range", terms))

  if (is_true(average)) {
    x <- group_by(x, .data$location) |>
      mutate(sp_range = mean(.data$sp_range))

    elev_grad_clim <- get_elev_grad_clim(x, clim_data, keep_vars = cols)

    x <- elev_grad_clim |>
      group_by(.data$location) |>
      mutate(across(starts_with("bio"), mean)) |>
      ungroup() |>
      distinct(.data$location, .keep_all = TRUE)
  }
  if (is_true(rename_bioclim)) {
    x <- rename_bioclim(x)
    if (exists("elev_grad_clim")) {
      elev_grad_clim <- rename_bioclim(elev_grad_clim)
    }
  }

  x <- select(x, any_of(cols))

  if (is.null(formula)) {
    x <- rename(x, term = terms)
  }
  x <- compose_mdl_data(x, formula)
  if (is_true(average)) {
    x <- c(x, list(
      n_obs = nrow(all_data),
      location_obs = factor(all_data$location),
      sp_range_obs = all_data$sp_range
    ))
  }
  settings <- make_list(
    elevation_span,
    exclusion_zone,
    singleton_thr,
    std_elev_grad,
    average,
    cols,
    formula,
    terms,
    std_from
  )
  out <- list(data = x, settings = settings)
  if (exists("elev_grad_clim")) {
    out <- c(out, list(elev_grad_clim = elev_grad_clim))
  }
  out
}

filter_species <- function(x, exclusion_zone) {
  out <- group_by(x, .data$location)
  out <- mutate(
    out,
    upper_exclusion_lim = .data$elev_span_max - exclusion_zone,
    lower_exclusion_lim = .data$elev_span_min + exclusion_zone
  )
  out <- filter(
    out,
    .data$sp_min < .data$upper_exclusion_lim,
    .data$sp_max > .data$lower_exclusion_lim
  )
  ungroup(out)
}

std_elev_grad <- function(x, length, from) {
  if (from == "top") {
    out <- mutate(x, elev_span_min = .data$elev_span_max - length)
    out <- filter(out, .data$sp_max >= .data$elev_span_min)
    out <- mutate(out, sp_min = if_else(
      .data$sp_min < .data$elev_span_min,
      .data$elev_span_min,
      .data$sp_min
    ))
  } else {
    out <- mutate(x, elev_span_max = .data$elev_span_min + length)
    out <- filter(out, .data$sp_min <= .data$elev_span_max)
    out <- mutate(out, sp_max = if_else(
      .data$sp_max > .data$elev_span_max,
      .data$elev_span_max,
      .data$sp_max
    ))
  }
  mutate(
    out,
    elev_span = length,
    sp_mean = (.data$sp_max + .data$sp_min) / 2,
    sp_range = .data$sp_max - .data$sp_min
  )
}

rename_bioclim <- function(x) {
  rename(x, mat = .data$bio1, dtr = .data$bio2, ts = .data$bio4, ap = .data$bio12)
}

get_elev_grad_clim <- function(x, clim_data, keep_vars) {
  out <- distinct(x, .data$location, .keep_all = TRUE)
  keep_vars <- discard(keep_vars, names(clim_data))
  out <- select(out, starts_with("elev_span"), any_of(keep_vars))
  out <- left_join(out, clim_data, by = "location")
  out <- rowwise(out)
  out <- filter(out, between(.data$elev_band, .data$elev_span_min, .data$elev_span_max))
  ungroup(out)
}

filter_locations <- function(x, elevation_span, singleton_thr) {
  filter(x, .data$elev_span >= elevation_span, .data$singleton <= singleton_thr)
}
