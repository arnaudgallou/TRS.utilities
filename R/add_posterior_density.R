#' @title Compute the Kernel density estimates and highest density interval of posterior distributions
#' @description Add the Kernel density estimates and highest density interval of posterior distributions to a data frame prior using [`posterior_dist()`].
#' @param x A data frame.
#' @param estimates Column of posterior distributions.
#' @param prob The probability mass to include in the shaded region.
#' @param outer_prob The probability mass to include in the outer interval.
#' @export
add_posterior_density <- function(x, estimates, prob, outer_prob = .99) {
  x <- add_averages(x, {{estimates}})

  groups <- group_vars(x)

  x <- left_join(
    add_hdi(x, {{estimates}}, prob, outer_prob = outer_prob),
    add_density(x, {{estimates}}, median, mean),
    by = groups
  )

  probs <- c(prob, outer_prob)
  x <- map_df(
    probs,
    ~ filter_cred_interval(x, .x),
    .id = "ci_id"
  )

  coords <- x %>%
    group_by(across(all_of(groups))) %>%
    summarize(
      xmin = min(.data$x),
      xmax = max(.data$x),
      ymax = max(.data$y),
      .groups = "drop"
    )

  left_join(x, coords, by = groups)
}


add_averages <- function(x, var) {
  mutate(
    x,
    median = median({{var}}, na.rm = TRUE),
    mean = mean({{var}}, na.rm = TRUE)
  )
}


# ... Variables to keep in the nested tibble.
add_density <- function(x, estimates, ...) {
  x %>%
    group_by(..., .add = TRUE) %>%
    nest() %>%
    mutate(density = map(
      .data$data,
      ~ .x %>%
        pull({{estimates}}) %>%
        density() %>%
        (function(den) {
          tibble(x = den$x, y = den$y)
        })()
    )) %>%
    select(-.data$data) %>%
    unnest(.data$density)
}


add_hdi <- function(x, estimates, prob, outer_prob = .99) {
  probs <- c(prob, outer_prob)
  x %>%
    summarize(
      ci = list(bayestestR::hdi({{estimates}}, ci = probs)),
      .groups = "keep"
    ) %>%
    unnest(.data$ci) %>%
    clean_names()
}


filter_cred_interval <- function(x, cred_interval) {
  x %>%
    rowwise() %>%
    filter(
      .data$ci == cred_interval,
      between(.data$x, .data$ci_low, .data$ci_high)
    ) %>%
    ungroup()
}
