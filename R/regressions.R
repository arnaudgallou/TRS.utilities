#' @title Plot regression fits
#' @description Plot regression fits.
#' @param data A list of data produced by [`regression_data()`].
#' @param labels A character vector used to label the x axis. If `labels` contains
#'   several labels, each label must be named. Names must be the same as the
#'   explanatory variables.
#' @param ... Other arguments passed to methods.
#' @export
regressions <- function(data, labels, ...) {
  colors <- regression_colors()
  has_single_label <- length(labels) == 1L
  if (has_single_label) {
    plot_data <- data
  } else {
    plot_data <- map(data, \(x) {
      mutate(x, expl_var = factor(expl_var, levels = names(labels)))
    })
  }
  plot <- ggplot(plot_data$data, aes(x = .data$x, y = .data$est_range_mean))
  if (has_single_label) {
    plot <- plot + xlab(labels)
  } else {
    plot <- plot + facet_grid(
      rows = vars(.data$exclusion_zone),
      cols = vars(.data$expl_var),
      scales = "free",
      switch = "x",
      labeller = as_labeller(labels)
    )
  }
  UseMethod("regressions")
}

#' @rdname regressions
#' @param n_draws Number of draws to sample from the 95\\% credible interval.
#' @param draws_prob Probability mass to sample the draws from.
#' @param point_labels Should point labels be shown?
#' @param seed Seed value for draw sampling.
#' @export
regressions.draws <- function(
    data,
    labels,
    ...,
    n_draws = 600,
    draws_prob = .95,
    point_labels = FALSE,
    seed = 130821
) {
  draws <- group_by(plot_data$sims, .data$expl_var, .data$exclusion_zone)
  draws <- slice_draws(draws, .data$beta_2, draws_prob, n_draws, seed = seed)
  mean_draws <- summarize(
    draws,
    mean_beta_1 = mean(.data$beta_1),
    mean_beta_2 = mean(.data$beta_2),
    .groups = "drop"
  )
  draws <- ungroup(draws)
  if (!point_labels) {
    plot <- plot +
      geom_abline(
        aes(slope = .data$beta_2, intercept = .data$beta_1),
        data = draws,
        color = colors$draws,
        alpha = .1,
        size = .25
      )
  }
  plot <- plot +
    geom_abline(
      aes(slope = .data$mean_beta_2, intercept = .data$mean_beta_1),
      data = mean_draws,
      colour = colors$mean_draw,
      size = if (point_labels) .25 else 1,
      alpha = if (point_labels) .7 else 1
    )
  if (!point_labels) {
    plot <- plot + geom_errorbar(aes(ymin = se_min, ymax = se_max), size = .3)
  }
  plot <- plot + regression_points(colors, point_labels)
  if (point_labels) {
    plot <- plot +
      ggrepel::geom_text_repel(
        aes(label = .data$rowid, color = .data$land_type),
        segment.size = .2,
        min.segment.length = .3,
        show.legend = FALSE
      )
  }
  plot + regression_theme(has_single_label)
}

#' @rdname regressions
#' @export
regressions.land_types <- function(data, labels, ...) {
  plot +
    geom_ribbon(
      aes(
        x = .data$value,
        y = .data$estimate,
        ymin = .data$conf_low,
        ymax = .data$conf_high,
        fill = .data$land_type
      ),
      data = plot_data$sims,
      alpha = 0.15
    ) +
    geom_line(
      aes(x = .data$value, y = .data$estimate, color = .data$land_type),
      data = plot_data$sims
    ) +
    scale_color_manual(values = colors$ribbons) +
    scale_fill_manual(values = colors$ribbons) +
    ggnewscale::new_scale_fill() +
    geom_errorbar(aes(ymin = se_min, ymax = se_max), size = .3) +
    regression_points(colors) +
    regression_theme(has_single_label)
}

regression_colors <- function() {
  list(
    draws = "#b6d3e2",
    mean_draw = "#006699",
    points = c("#737373", "#ffffff"),
    labels = c("#e34326", "#3176a9"),
    ribbons = c("#f57a5b", "#85a9d6")
  )
}

regression_points <- function(colors, point_labels = FALSE) {
  list(
    geom_point(
      aes(
        shape = .data$land_type,
        fill = .data$land_type,
        color = if (point_labels) .data$land_type
      ),
      alpha = if (point_labels) .6 else 1
    ),
    scale_shape_manual(
      name = "Land type",
      values = c(21, 21),
      labels = c("continent", "island")
    ),
    scale_fill_manual(
      name = "Land type",
      values = if (point_labels) colors$labels else colors$points
    )
  )
}

regression_theme <- function(has_single_label) {
  if (has_single_label) {
    strip.text.x <- element_blank()
    axis.title.x <- element_text()
  } else {
    strip.text.x <- element_text(margin = margin(t = 0))
    axis.title.x <- element_blank()
  }
  list(
    theme_elesic(),
    theme(
      panel.spacing = unit(.5, "lines"),
      strip.placement = "outside",
      strip.text.y = element_blank(),
      strip.text.x = strip.text.x,
      axis.title.x = if (!has_single_label) element_blank()
    ),
    add_facet_lines(),
    scale_y_continuous(
      name = "Mean species range (m)",
      labels = \(x) round(exp(x)),
      breaks = log(seq(0, 1400, 200))
    )
  )
}
