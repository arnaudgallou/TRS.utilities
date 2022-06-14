#' @title Compute Bayesian R-squared and the probability of posterior fits to be lower than 0
#' @description Compute Bayesian R-squared and the probability of posterior fits to be lower than 0 from a JAGS model output.
#' @param x A model object file returned by [`run_jags()`].
#' @return A [`tibble`][tibble::tibble()].
#' @export
get_statistical_details <- function(x) {
  posteriors <- get_jags_sims(x, "beta")

  r2 <- calc_jags_r2(x, posteriors)
  p_diff_0 <- calc_p_inf_0(posteriors[, -1])
  settings <- get_mdl_settings(x)

  bind_cols(settings, r2, p_diff_0) %>%
    clean_names() %>%
    select(-c(.data$term, starts_with("conf")))
}


calc_jags_r2 <- function(x, posteriors) {
  formula <- x$settings$formula
  terms <- parse_formula(formula)
  is_land_type <- any(grepl("land_type", terms))
  if (length(terms) == 1) {
    formula <- ~ bioclim
  } else if (is_land_type) {
    formula <- ~ bioclim * land_type
  } else {
    formula <- formula
  }

  model_matrix <- model.matrix(formula, x$data)
  fit <- as.matrix(posteriors) %*% t(model_matrix)
  residuals <- sweep(fit, 2, x$stats$mean[[1]])

  fit_var <- apply(fit, 1, var)
  residuals_var <- apply(residuals, 1, var)

  r_squared(fit_var, residuals_var) %>%
    coda::as.mcmc() %>%
    broom.mixed::tidyMCMC(conf.int = TRUE, conf.method = "HPDinterval") %>%
    rename(r_squared = .data$estimate)
}


r_squared <- function(x, y) {
  x / (x + y)
}


#' @title Calculate the probability of posterior fits to be lower than 0
#' @description Calculate the probability of posterior fits to be lower than 0.
#' @param x A data frame containing posterior fits.
#' @return A [`tibble`][tibble::tibble()].
#' @export
calc_p_inf_0 <- function(x) {
  x %>%
    mutate(across(matches("beta"), ~ mean(.x < 0))) %>%
    distinct() %>%
    rename_with(~ str_replace_all(.x, "(?=beta)", "p_lower_0_"))
}
