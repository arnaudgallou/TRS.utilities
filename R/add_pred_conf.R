# Add the posterior predictive with standard deviation and confidence from a
#   JAGS model to a data frame.
# @param data A data frame.
# @param fit A JAGS model fit.
# @param formula Formula containing group-level effects to be considered in the
#   prediction.
add_pred_conf <- function(data, fit, formula) {
  model_matrix <- model.matrix(formula, data)
  new_fit <- as.matrix(fit) %*% t(model_matrix)
  df_new_fit <- broom.mixed::tidyMCMC(new_fit, conf.int = TRUE)
  bind_cols(data, clean_names(df_new_fit))
}
