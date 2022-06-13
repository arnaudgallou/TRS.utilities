#' @importFrom loo waic loo relative_eff
#' @title Evaluate JAGS models
#' @description Compare multiple JAGS models using the WAIC and leave-one-out (LOO) cross-validation methods.
#' @param files Path of files containing a JAGS model object returned by [`run_jags()`].
#' @return A tibble of WAIC and LOO IC estimates with standard error.
#' @export
eval_mdls <- function(files) {
  map_df(files, ~ {
    data <- read_rds(.x)
    loglik_data <- get_jags_sims(data, "loglik")
    matrix <- as.matrix(loglik_data)

    suppressWarnings({
      waic <- waic(matrix)
      waic <- get_ic_estimates(waic)

      r_eff <- relative_eff(exp(matrix), chain_id = data$sims$chain)
      loo <- loo(matrix, r_eff = r_eff)
      loo <- get_ic_estimates(loo, "loo")
    })

    bind_cols(get_mdl_settings(data), waic, loo)
  }) %>%
    arrange(.data$waic, .data$looic) %>%
    mutate(
      delta_waic = delta(.data$waic),
      delta_looic = delta(.data$looic)
    ) %>%
    select(model:std_from, ends_with("waic"), ends_with("looic"))
}


get_ic_estimates <- function(x, method = c("waic", "loo")) {
  ic <- if (match.arg(method) == "waic") "waic" else "looic"
  x <- as_tibble(x$estimates[ic,, drop = FALSE])
  rename(x, !!ic := .data$Estimate, "{ic}_se" := .data$SE)
}
