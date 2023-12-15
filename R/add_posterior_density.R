# @description Add the Kernel density estimates and highest density interval of
#   posterior distributions to a data frame before using
#   [`plot_posterior_distributions()`].
# @param data A data frame.
# @param estimates Variable containing posterior distributions.
# @param prob The probability mass to include in the shaded region.
# @param prob_outer The probability mass to include in the outer interval.
add_posterior_density <- function(data, estimates, prob, prob_outer = .99) {
  out <- add_averages(data, {{estimates}})
  groups <- group_vars(out)
  probs <- c(prob, prob_outer)
  out <- left_join(
    add_hdi(out, {{estimates}}, probs),
    add_density(out, {{estimates}}, median, mean),
    by = groups,
    multiple = "all"
  )
  out <- map(probs, \(prob) filter_cred_interval(out, prob))
  out <- list_rbind(out, names_to = "ci_id")
  coords <- summarize(
    out,
    xmin = min(.data$x),
    xmax = max(.data$x),
    ymax = max(.data$y),
    .by = all_of(groups)
  )
  left_join(out, coords, by = groups)
}

add_averages <- function(data, var) {
  mutate(
    data,
    median = median({{var}}, na.rm = TRUE),
    mean = mean({{var}}, na.rm = TRUE)
  )
}

# ... Variables to keep in the nested tibble.
add_density <- function(data, estimates, ...) {
  out <- group_by(data, ..., .add = TRUE)
  out <- nest(out)
  out <- mutate(out, density = map(.data$data, \(.x) {
    this <- pull(.x, {{estimates}})
    this <- density(this)
    tibble(x = this$x, y = this$y)
  }))
  out <- select(out, -.data$data)
  unnest(out, .data$density)
}

add_hdi <- function(data, estimates, probs) {
  out <- summarize(
    data,
    ci = list(bayestestR::hdi({{estimates}}, ci = probs)),
    .groups = "keep"
  )
  out <- unnest(out, .data$ci)
  clean_names(out)
}

filter_cred_interval <- function(data, cred_interval) {
  out <- rowwise(data)
  out <- filter(
    out,
    .data$ci == cred_interval,
    between(.data$x, .data$ci_low, .data$ci_high)
  )
  ungroup(out)
}
