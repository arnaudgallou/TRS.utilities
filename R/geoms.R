#' @title Draw a line at 0
#' @description Draw a vertical or horizontal line at 0.
#' @param axis Axis to draw the line on.
#' @param linetype Line type.
#' @param color Line color.
#' @param size Line size (in mm).
#' @param ... Other arguments passed on to [ggplot2::layer()].
#' @export
line_0 <- function(
    axis = c("x", "y"),
    linetype = "dashed",
    color = "grey50",
    size = .3,
    ...
) {
  axis <- match.arg(axis)
  fun <- if (axis == "x") geom_vline else geom_hline
  args <- list2(
    "{axis}intercept" := 0,
    linetype = linetype,
    color = color,
    size = size,
    ...
  )
  do.call(fun, args)
}

#' @title Posterior predictive checks
#' @description Wrapper around [`bayesplot::ppc_dens_overlay()`] to plot posterior
#'   predictive check from a JAGS model object.
#' @param y Observed data.
#' @param yrep Fitted data.
#' @param n_draws Number of fitted draws to sample.
#' @param ... Optional arguments passed on to [`bayesplot::ppc_dens_overlay()`].
#' @export
ppc <- function(y, yrep, n_draws = 100, ...) {
  samp <- sample(nrow(yrep), size = n_draws)
  yrep <- yrep[samp, ]
  bayesplot::ppc_dens_overlay(y, yrep, ...)
}

#' @title Posterior predictive checks
#' @description More specific variant of [`ppc()`] that will check posteriors
#'   from the global-scale analysis.
#' @inheritParams ppc
#' @param all If `TRUE`, will use all species ranges. Otherwise, will use species
#'   ranges averaged in each location.
#' @export
ppc_ <- function(y, yrep, all = FALSE, ...) {
  if (all) {
    y <- y$data$sp_range_obs
    yrep <- yrep$BUGSoutput$sims.list$sp_range_rep
  } else {
    y <- y$data$sp_range
    yrep <- yrep$BUGSoutput$sims.list$alpha_site_rep
  }
  ppc(y, yrep, adjust = 2, ...)
}

#' @title Trace plots of MCMC draws
#' @description Trace plots of MCMC draws.
#' @param x A data frame.
#' @param mapping Default list of aesthetic mappings to use for plot.
#'   See [`ggplot2::ggplot()`] for details.
#' @param facets Variables or expressions defining faceting groups on the columns
#'   dimension. See [`ggplot2::facet_wrap()`] for details.
#' @export
traceplot <- function(x, mapping = aes(), facets) {
  ggplot(x, mapping) +
    ggh4x::facet_wrap2(
      facets,
      ncol = 1,
      scales = "free_y",
      strip.position = "left",
      axes = "all",
      remove_labels = TRUE
    ) +
    geom_line() +
    scale_colour_brewer(direction = -1) +
    theme_elesic(legend_position = "right") +
    theme(
      axis.title = element_blank(),
      strip.placement = "outside"
    )
}
