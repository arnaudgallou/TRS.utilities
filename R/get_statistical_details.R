#' @title Compute Bayesian R-squared and the probability of posterior fits to be
#'   lower than 0
#' @description Compute Bayesian R-squared and the probability of posterior fits
#'   to be lower than 0 from a JAGS model output.
#' @param data A model object file returned by [`run_jags()`].
#' @return A [`tibble`][tibble::tibble()].
#' @examples
#' \dontrun{
#' readr::read_rds("path_to_file") |> get_statistical_details()
#' }
#' @export
get_statistical_details <- function(data) {
  posteriors <- get_jags_sims(data, "beta")
  prob_diff_0 <- calc_prob_inf_0(posteriors[, -1])
  settings <- get_mdl_settings(data)
  r2 <- calc_jags_r2(data, posteriors)
  out <- bind_cols(settings, r2, prob_diff_0)
  out <- clean_names(out)
  select(out, -c(.data$term, starts_with("conf")))
}

calc_jags_r2 <- function(x, posteriors) {
  formula <- x$settings$formula
  terms <- parse_formula(formula)
  is_land_type <- any(grepl("land_type", terms))
  if (length(terms) == 1L) {
    formula <- ~ x
  } else if (is_land_type) {
    formula <- ~ x * land_type
  } else {
    formula <- formula
  }
  model_matrix <- model.matrix(formula, x$data)
  fit <- as.matrix(posteriors) %*% t(model_matrix)
  residuals <- sweep(fit, 2, x$stats$mean[[1]])
  fit_var <- apply(fit, 1, var)
  residuals_var <- apply(residuals, 1, var)
  out <- r_squared(fit_var, residuals_var)
  out <- coda::as.mcmc(out)
  out <- broom.mixed::tidyMCMC(out, conf.int = TRUE, conf.method = "HPDinterval")
  rename(out, r_squared = .data$estimate)
}

r_squared <- function(x, y) {
  x / (x + y)
}

# @description Calculate the probability of posterior fits to be lower than 0.
# @param data A data frame containing posterior fits.
calc_prob_inf_0 <- function(data) {
  out <- mutate(data, across(matches("beta"), ~ mean(.x < 0)))
  out <- distinct(out)
  rename_with(out, \(col) string_replace(col, "(?=beta)", "p_lower_0_"))
}
