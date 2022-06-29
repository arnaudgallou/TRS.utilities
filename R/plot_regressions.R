#' @title Plot regression fits
#' @description Plot regression fits.
#' @param x A data frame of observed data.
#' @param y A data frame of model fits.
#' @param facet_by Variable defining faceting groups.
#' @param show_draws Should estimated draws be shown?
#' @param n_draws Number of draws to sample from the 95\\% credible interval.
#' @param draws_prob Probability mass to sample the draws from.
#' @param draws_color Color of the draws.
#' @param draws_size Size of the draws.
#' @param draws_alpha Transparency the of draws.
#' @param mean_draw_color Color of the mean draw.
#' @param point_colors Color of the points data.
#' @param land_type If `TRUE`, will plot regression lines with the 95\\% credible interval for each land type.
#' @param ribbon_colors Color of the ribbons. Only applies when `land_type=TRUE`.
#' @param error_bars Should error bars be drawn?
#' @param point_labels Should point labels be shown?
#' @param label_colors Color of the labels.
#' @param labellers A named character vector of labellers for the x axis.
#' @param legend_position Position of the legend. One of `c("right", "bottom")`.
#' @param text_size A numerical value defining the text size (in pts).
#' @param panel_spacing Spacing between facet panels. See [`ggplot2::theme()`] for details.
#' @param seed Seed value for draw sampling.
#' @export
plot_regressions <- function(
    x,
    y,
    facet_by,
    show_draws = TRUE,
    n_draws = 600,
    draws_prob = .95,
    draws_color = "#b6d3e2",
    draws_size = .25,
    draws_alpha = .1,
    mean_draw_color = "#006699",
    point_colors = c("#737373", "#ffffff"),
    land_type = FALSE,
    ribbon_colors = NULL,
    error_bars = TRUE,
    point_labels = FALSE,
    label_colors = NULL,
    labellers,
    legend_position = c("none", "right", "bottom", "top"),
    text_size = 11,
    panel_spacing = unit(.5, "lines"),
    seed = 130821
) {
  if (isTRUE(point_labels) && isTRUE(land_type)) {
    stop('"point_labels" must be FALSE when land_type=TRUE.')
  }
  if (!is.null(label_colors) && !isTRUE(point_labels)) {
    stop('"point_labels" must be TRUE to assign label colors.')
  }
  if (!is.null(ribbon_colors) && !isTRUE(land_type)) {
    warning('"land_type" must be TRUE to assign ribbon colors.')
  } else {
    ribbon_colors <- c("#f57a5b", "#85a9d6")
  }

  legend_position <- match.arg(legend_position)

  if (missing(labellers)) {
    # default labellers
    labellers <- c(
      "dtr" = "Diurnal temperature range (\u00b0C)",
      "ts" = "Temperature seasonality (\u00b0C)",
      "past_dmat" = "\u0394 mean annual temperature\n(0-1980) (\u00b0C)"
    )
  }

  has_single_label <- length(labellers) == 1
  xy <- make_list(x, y)

  if (!has_single_label) {
    var_order <- names(labellers)
    x_labs <- as_labeller(labellers)

    xy <- map(xy, ~ mutate(.x, !!facet_by := factor(.data[[facet_by]], levels = var_order)))
  }

  if (!land_type) {
    fitted_draws <- xy$y %>%
      group_by(.data[[facet_by]], .data$exclusion_zone) %>%
      slice_draws(.data$beta_2, prob = draws_prob, n = n_draws, seed = seed)

    mean_fitted_draws <- fitted_draws %>%
      summarize(
        mean_beta_1 = mean(.data$beta_1),
        mean_beta_2 = mean(.data$beta_2),
        .groups = "drop"
      )

    fitted_draws <- ungroup(fitted_draws)
  }

  dist_grp <- quos(.data[[facet_by]], .data$exclusion_zone)

  plot <- xy$x %>%
    ggplot(aes(
      x = .data$bioclim,
      y = .data$est_range_mean,
      color = if (point_labels) .data$land_type
    ))

  plot <- plot +
    facet_grid(
      rows = vars(.data$exclusion_zone),
      cols = vars(.data[[facet_by]]),
      scales = "free",
      switch = "x",
      labeller = if (has_single_label) "label_value" else x_labs
    )

  if (land_type) {
    plot <- plot +
      geom_ribbon(
        aes(x = .data$value, y = .data$estimate, ymin = .data$conf_low, ymax = .data$conf_high, fill = .data$land_type),
        data = xy$y,
        alpha = 0.15
      ) +
      geom_line(
        aes(x = .data$value, y = .data$estimate, color = .data$land_type),
        data = xy$y
      ) +
      scale_color_manual(values = ribbon_colors) +
      scale_fill_manual(values = ribbon_colors) +
      ggnewscale::new_scale_fill()
  } else {
    if (show_draws) {
      plot <- plot +
        geom_abline(
          aes(slope = .data$beta_2, intercept = .data$beta_1),
          data = fitted_draws,
          color = draws_color,
          alpha = draws_alpha,
          size = draws_size
        )
    }

    plot <- plot +
      geom_abline(
        aes(slope = .data$mean_beta_2, intercept = .data$mean_beta_1),
        data = mean_fitted_draws,
        colour = if_else(point_labels, "black", mean_draw_color),
        size = if_else(point_labels, .25, 1),
        alpha = if_else(point_labels, .7, 1)
      )
  }

  if (error_bars) {
    plot <- plot +
      geom_errorbar(
        aes(
          ymin = .data$est_range_mean - .data$est_range_sd,
          ymax = .data$est_range_mean + .data$est_range_sd
        ),
        size = .3
      )
  }

  plot <- plot +
    geom_point(
      aes(shape = .data$land_type, fill = .data$land_type),
      alpha = if_else(point_labels, .6, 1)
    ) +
    scale_shape_manual(
      name = "Land type",
      values = c(21, 21),
      labels = c("continent", "island")
    ) +
    scale_fill_manual(
      name = "Land type",
      values = if (point_labels) label_colors else point_colors
    )

  if (point_labels) {
    plot <- plot +
      ggrepel::geom_text_repel(
        aes(label = .data$rowid),
        segment.size = .2,
        min.segment.length = .3,
        show.legend = FALSE
      )
  }

  if (has_single_label) {
    strip_text_x <- element_blank()
  } else {
    strip_text_x <- element_text(margin = margin(t = 0))
  }

  plot <- plot +
    theme_elesic(base_size = text_size, legend_position = legend_position) +
    theme(
      panel.spacing = panel_spacing,
      strip.placement = "outside",
      strip.text.y = element_blank(),
      strip.text.x = strip_text_x,
      axis.title.x = if (has_single_label) element_text() else element_blank(),
      plot.caption = if (point_labels) element_text(hjust = .5, margin = margin(t = 15))
    ) +
    add_facet_lines()

  if (point_labels) {
    plot <- plot + scale_color_manual(name = "Land type", values = label_colors)
  }

  plot <- plot +
    scale_y_continuous(
      name = "Mean species range (m)",
      labels = function(x) round(exp(x)),
      breaks = log(seq(0, 1400, 200))
    )

  if (has_single_label) {
    plot <- plot + xlab(labellers)
  }

  plot
}
