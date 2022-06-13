#' @title Extend JAGS summary
#' @description Add standardized slope estimates and whether the 95\% credible interval (CI) includes 0 or not to a JAGS summary. A value of 1 indicates that the CI includes 0, a value of 0 indicates that the CI does not include 0. Requires running [`run_jags()`] with `default_output=FALSE`.
#' @param x A data frame.
#' @export
extend_jags_summary <- function(x) {
  mutate(
    x,
    beta_std = .data$mean / .data$sd,
    x95_ci = if_else(.data$x2_5_percent > 0 & .data$mean > 0 | .data$x97_5_percent < 0 & .data$mean < 0, "0", "1")
  )
}


#' @title Compute the proportion of positive and negative slopes
#' @description Compute the proportion of positive and negative slopes in a data frame returned by [`extend_jags_summary()`]. High and low uncertainties indicate that the 95\% CI includes 0 or not, respectively.
#' @param x A data frame.
#' @export
summarize_slopes <- function(x) {
  summarize(
    x,
    positive = proportion(.data$beta_std > 0),
    negative = proportion(.data$beta_std < 0),
    positive_high_uncertainties = proportion(.data$beta_std > 0 & .data$x95_ci == "1"),
    negative_high_uncertainties = proportion(.data$beta_std < 0 & .data$x95_ci == "1"),
    positive_low_uncertainties = proportion(.data$beta_std > 0 & .data$x95_ci == "0"),
    negative_low_uncertainties = proportion(.data$beta_std < 0 & .data$x95_ci == "0"),
    .groups = "drop"
  )
}
