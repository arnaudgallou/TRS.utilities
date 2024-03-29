#' @title Plot posterior distributions
#' @description Plot posterior distributions.
#' @param data A data frame produced by [`make_posterior_data()`].
#' @param mapping Default list of aesthetic mappings to use for plot.
#'   See [`ggplot2::ggplot()`] for details.
#' @param scale A scaling factor to scale the height of the ridgelines. See
#'   [`ggridges::geom_ridgeline()`] for details.
#' @param vline_type Line type of the line indicating 0.
#' @param vline_color Color of the line indicating 0.
#' @param facet_args A named list of parameters and arguments to pass on to
#'   [`ggh4x::facet_grid2()`].
#' @param ... Other arguments passed on to [`ggplot2::ggplot()`].
#' @examples
#' \dontrun{
#' get_files("path_to_dir", vars = "dtr", elevation_span = 2000) |>
#'   make_posterior_data(yvar = "exclusion_zone") |>
#'   plot_posterior_distributions(aes(
#'     x = x,
#'     y = exclusion_zone,
#'     height = y,
#'     color = exclusion_zone,
#'     fill = exclusion_zone
#'   ))
#' }
#' @export
plot_posterior_distributions <- function(
    data,
    mapping = aes(),
    scale = 1,
    vline_type = 1,
    vline_color = "grey90",
    facet_args = list(),
    ...
) {
  den_mass <- unique(data$ci_id)
  n_mass <- length(den_mass)
  facet_dims <- Filter(Negate(is.null), facet_args[c("rows", "cols")])
  yvar <- mapping$y

  df_seg <- distinct(data, !!!unlist(facet_dims), {{yvar}}, .keep_all = TRUE)
  df_seg <- mutate(
    df_seg,
    ymin = as.numeric(factor({{yvar}})),
    ymax = .data$ymax * scale
  )

  plot <- ggplot(data, mapping, ...) +
    line_0(linetype = vline_type, color = vline_color) +
    map2(
      den_mass,
      sort(seq(0, 1, length.out = n_mass), decreasing = TRUE),
      ~ {
        base_rl_args <- list(data = filter(data, ci_id == .x), scale = scale)
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
    plot <- plot + do.call(ggh4x::facet_grid2, facet_args)
  }

  plot
}
