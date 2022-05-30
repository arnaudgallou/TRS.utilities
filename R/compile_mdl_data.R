#' @title Compile model data
#' @description Compile data for a JAGS model.
#' @param x A data frame containing data to pass on to a JAGS model.
#' @param clim_data A data frame containing climate data. Only to be specified if `average=TRUE`.
#' @param min_elev_grad_len Minimum length of the elevational gradients to filter.
#' @param singleton_thr Maximum proportion of single observations to filter location with.
#' @param exclusion_zone Length of the upper and lower sections of the elevational gradient to discard species from. E.g. an exclusion zone of 250 will discard all species found exclusively in the lower and upper 250 m of elevational gradients. All species with at least one observation outside of the exclusion zone are kept.
#' @param std_elev_grad Should elevational gradients be standardised?
#' @param average Should species range sizes and climate data averaged?
#' @param cols A vector of column names to select. Variables passed to `matrix_formula` will be automatically kept.
#' @param rename_bioclim If `TRUE` (the default), will rename "bio1", "bio2", "bio4" and "bio12" as "mat", "dtr", "ts" and "ap", respectively.
#' @param expr A character vector (for the local-scale analysis) or a formula (for the global-scale analysis) describing the predictor variable(s) to fit the model.
#' @param std_from Edge on the elevational gradient to standardize from. One of `c("top", "bottom")`.
#' @return A list of JAGS-compatible data.
#' @export
compile_mdl_data <- function(
  x,
  clim_data = NULL,
  min_elev_grad_len,
  singleton_thr,
  exclusion_zone,
  std_elev_grad = FALSE,
  average = FALSE,
  cols = c(),
  rename_bioclim = TRUE,
  expr = NULL,
  std_from = c("top", "bottom")
) {
  if (missing(clim_data) && isTRUE(average)) {
    stop("'clim_data' is required when average=TRUE.")
  }

  std_from <- if (isTRUE(std_elev_grad)) match.arg(std_from) else NULL

  if (is_formula(expr)) {
    formula <- expr
    terms <- parse_formula(expr)
  } else {
    formula <- NULL
    terms <- expr
  }

  x <- filter(x, .data$elev_grad_len >= min_elev_grad_len, .data$singleton <= singleton_thr)

  if (isTRUE(std_elev_grad)) {
    x <- std_elev_grad(x, length = min_elev_grad_len, from = std_from)
  }

  if (!missing(exclusion_zone)) {
    x <- filter_species(x, exclusion_zone = exclusion_zone)
  }

  x <- all_data <- drop_na(x, starts_with("bio")) %>%
    mutate(elev_range = if_else(.data$elev_range == 0, 10, .data$elev_range) %>% log())

  if (isTRUE(average)) {
    x <- select(x, -.data$elev_range)

    elev_grad_clim <- get_elev_grad_clim(x, clim_data)

    x <- elev_grad_clim %>%
      group_by(.data$location) %>%
      mutate(across(starts_with("bio"), mean)) %>%
      ungroup() %>%
      distinct(.data$location, .keep_all = TRUE)
  }

  if (isTRUE(rename_bioclim)) {
    x <- rename_bioclim(x)
    if (exists("elev_grad_clim")) {
      elev_grad_clim <- rename_bioclim(elev_grad_clim)
    }
  }

  if (!missing(cols)) {
    cols <- unique(c(cols, terms))
    x <- select(x, any_of(cols))
  }

  if (is.null(formula)) {
    x <- rename(x, bioclim = terms)
  }

  x <- compose_mdl_data(x, formula)

  if (isTRUE(average)) {
    x <- c(x, list(
      n_obs = nrow(all_data),
      location_obs = factor(all_data$location),
      elev_range_obs = all_data$elev_range
    ))
  }

  settings <- make_list(
    min_elev_grad_len,
    singleton_thr,
    exclusion_zone,
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
  group_by(x, .data$location) %>%
    mutate(
      upper_exclusion_lim = .data$elev_grad_max - exclusion_zone,
      lower_exclusion_lim = .data$elev_grad_min + exclusion_zone
    ) %>%
    filter(
      .data$elev_min < .data$upper_exclusion_lim,
      .data$elev_max > .data$lower_exclusion_lim
    ) %>%
    ungroup()
}


std_elev_grad <- function(x, length, from) {
  if (from == "top") {
    x <- x %>%
      mutate(elev_grad_min = .data$elev_grad_max - length) %>%
      filter(.data$elev_max >= .data$elev_grad_min) %>%
      mutate(elev_min = if_else(
        .data$elev_min < .data$elev_grad_min,
        .data$elev_grad_min,
        .data$elev_min
      ))
  } else {
    x <- x %>%
      mutate(elev_grad_max = .data$elev_grad_min + length) %>%
      filter(.data$elev_min <= .data$elev_grad_max) %>%
      mutate(elev_max = if_else(
        .data$elev_max > .data$elev_grad_max,
        .data$elev_grad_max,
        .data$elev_max
      ))
  }
  mutate(
    x,
    elev_grad_len = length,
    elev_mean = (.data$elev_max + .data$elev_min) / 2,
    elev_range = .data$elev_max - .data$elev_min
  )
}


rename_bioclim <- function(x) {
  rename(x, mat = .data$bio1, dtr = .data$bio2, ts = .data$bio4, ap = .data$bio12)
}


get_elev_grad_clim <- function(x, clim_data) {
  x %>%
    distinct(.data$location, .keep_all = TRUE) %>%
    select(matches("^(?:location|land_type|elev_grad|elev_range|past)")) %>%
    left_join(clim_data, by = "location") %>%
    rowwise() %>%
    filter(between(.data$elev_band, .data$elev_grad_min, .data$elev_grad_max)) %>%
    ungroup()
}
