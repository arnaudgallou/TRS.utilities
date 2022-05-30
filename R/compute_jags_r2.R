#' @title Compute Bayesian R-squared
#' @description Compute Bayesian R-squared from a JAGS model output.
#' @param x A model object file returned by [`run_jags()`].
#' @param formula Formula containing group-level effects to be considered in the prediction.
#' @export
compute_jags_r2 <- function(x, formula) {
  posteriors <- get_jags_sims(x, "beta")
  model_matrix <- model.matrix(formula, x$data)

  fit <- as.matrix(posteriors) %*% t(model_matrix)
  residuals <- sweep(fit, 2, x$stats$mean[[1]])

  fit_var <- apply(fit, 1, var)
  residuals_var <- apply(residuals, 1, var)

  r2(fit_var, residuals_var) %>%
    coda::as.mcmc() %>%
    broom.mixed::tidyMCMC(conf.int = TRUE, conf.method = "HPDinterval") %>%
    clean_names()
}


r2 <- function(x, y) {
  x / (x + y)
}
