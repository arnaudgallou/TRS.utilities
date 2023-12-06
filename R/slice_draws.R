#' @title Subset rows from a credible interval
#' @description Wrapper around [`dplyr::slice_sample()`] that subsets rows from
#'   a credible interval.
#' @param x A data frame.
#' @param estimates Column to subset the draws from.
#' @param prob The probability mass to subset.
#' @param n Number of draws to subset.
#' @param seed Random [`seed`][set.seed()] to produce identical result.
#' @examples
#' \dontrun{
#' df <- tibble(id = 1:500, value = rnorm(n = 500, mean = 0, sd = 5))
#' slice_draws(df, estimates = value)
#' }
#' @export
slice_draws <- function(x, estimates, prob = .95, n = 200, seed) {
  if (n > nrow(x)) {
    warning("number of draws larger than data.")
  }

  if (!missing(seed)) {
    set.seed(seed)
  }

  x <- filter_credible_interval(x, {{estimates}}, prob)
  slice_sample(x, n = n)
}


# Add the lowest and highest quantile values to a data frame
# @param x A data frame.
# @param var Column of numeric values to compute quantiles from.
# @param prob The probability mass to get the lowest and highest quantiles from.
add_quantiles <- function(x, var, prob = .95) {
  mutate(
    x,
    quantile_low = quantile({{var}}, probs = (1 - prob) / 2),
    quantile_high = quantile({{var}}, probs = (1 + prob) / 2)
  )
}


filter_credible_interval <- function(x, estimates, prob) {
  x <- add_quantiles(x, {{estimates}}, prob)
  filter(x, between({{estimates}}, .data$quantile_low[1], .data$quantile_high[1]))
}
