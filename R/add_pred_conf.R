#' @title Add predictions from the posterior fit to a data frame
#' @description Add the posterior predictive with standard deviation and confidence
#'   from a JAGS model to a data frame.
#' @param x A data frame.
#' @param fit A JAGS model fit.
#' @param formula Formula containing group-level effects to be considered in the
#'   prediction.
#' @export
add_pred_conf <- function(x, fit, formula) {
  model_matrix <- model.matrix(formula, x)
  new_fit <- as.matrix(fit) %*% t(model_matrix)
  df_new_fit <- broom.mixed::tidyMCMC(new_fit, conf.int = TRUE)
  bind_cols(x, clean_names(df_new_fit))
}
