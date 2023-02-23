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

#' @title Plot posterior distribution from a bayesian model
#' @description Plot posterior distribution from a bayesian model.
#' @param x A data frame.
#' @param mapping Default list of aesthetic mappings to use for plot.
#'   See [`ggplot2::ggplot()`] for details.
#' @param scale A scaling factor to scale the height of the ridgelines. See
#'   [`ggridges::geom_ridgeline()`] for details.
#' @param vline_type Line type of the line indicating 0.
#' @param vline_color Color of the line indicating 0.
#' @param facet_args A named list of parameters and arguments to pass on to
#'   [`ggplot2::facet_grid()`].
#' @param ... Other arguments passed on to [`ggplot2::ggplot()`].
#' @export
posterior_distributions <- function(
    x,
    mapping = aes(),
    scale = 1,
    vline_type = 1,
    vline_color = "grey90",
    facet_args = list(),
    ...
) {
  den_mass <- unique(x$ci_id)
  n_mass <- length(den_mass)
  facet_dims <- Filter(Negate(is.null), facet_args[c("rows", "cols")])
  yvar <- mapping$y

  df_seg <- distinct(x, !!!unlist(facet_dims), {{yvar}}, .keep_all = TRUE)
  df_seg <- mutate(
    df_seg,
    ymin = as.numeric(factor({{yvar}})),
    ymax = .data$ymax * scale
  )

  plot <- ggplot(x, mapping, ...) +
    line_0(linetype = vline_type, color = vline_color) +
    map2(
      den_mass,
      sort(seq(0, 1, length.out = n_mass), decreasing = TRUE),
      ~ {
        base_rl_args <- list(data = filter(x, ci_id == .x), scale = scale)
        cond_rl_args <- if (.x != den_mass[n_mass]) {
          c(alpha = .y, size = NA)
        } else {
          c(fill = NA, size = .3)
        }
        do.call(ggridges::geom_ridgeline, c(base_rl_args, cond_rl_args))
      }
    ) +
    geom_segment(
      aes(
        x = .data$median,
        xend = .data$median,
        y = .data$ymin,
        yend = .data$ymin + .data$ymax
      ),
      data = df_seg,
      size = 1
    ) +
    geom_segment(
      aes(x = .data$xmin, xend = .data$xmax, yend = .data$ymin),
      data = df_seg,
      size = .3
    ) +
    theme(legend.position = "none")

  if (!is_empty(facet_args)) {
    plot <- plot + do.call(facet_grid, facet_args)
  }

  plot
}

#' @title Add axis lines to facetted plots
#' @description Draw axis lines to facetted plots.
#' @param size Line size (in mm).
#' @export
add_facet_lines <- function(size = 1) {
  list(
    theme(axis.line = element_blank()),
    geom_vline(xintercept = -Inf, size = size),
    geom_hline(yintercept = -Inf, size = size)
  )
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
    facet_wrap(
      facets,
      ncol = 1,
      scales = "free_y",
      strip.position = "left"
    ) +
    geom_line() +
    scale_colour_brewer(direction = -1) +
    theme_elesic(legend_position = "right") +
    theme(
      axis.title = element_blank(),
      strip.placement = "outside"
    )
}
