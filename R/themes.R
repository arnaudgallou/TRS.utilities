#' @title Custom ggplot themes
#' @description Custom ggplot themes.
#' @param base_size Base font size, given in pts.
#' @param base_family Base font family.
#' @param base_line_size Base size for line elements.
#' @param base_rect_size Base size for rect elements.
#' @param base_axis_color Base color for axis elements. Applies to `axis.line`,
#'   `axis.ticks` and \code{axis.text}.
#' @param legend_position Set the legend position. See [`ggplot2::theme()`] for
#'   details. Set to \code{"none"} to hide the legend.
#' @name themes
NULL

#' @rdname themes
#' @export
theme_elesic <- function(
    base_size = 11,
    base_family = "",
    base_line_size = .25,
    base_rect_size = base_size / 22,
    base_axis_color = "black",
    legend_position = "none"
) {
  theme_classic(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      legend.position = legend_position,
      strip.background = element_blank(),
      strip.text = element_text(size = base_size),
      axis.title = element_text(size = base_size),
      axis.text = element_text(colour = base_axis_color, size = base_size - 1),
      axis.line = element_line(base_axis_color, size = base_line_size),
      axis.ticks = element_line(base_axis_color, size = base_line_size),
      axis.ticks.length = unit(1.5, "mm"),
      legend.text = element_text(size = base_size - 1),
      legend.title = element_text(size = base_size),
      plot.caption = element_text(size = base_size - 1)
    )
}

#' @rdname themes
#' @export
theme_blank_y <- function(
    base_size = 11,
    base_family = "",
    base_line_size = .25,
    base_rect_size = base_size / 22,
    base_axis_color = "black",
    legend_position = "none"
) {
  theme_elesic(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size,
    base_axis_color = base_axis_color,
    legend_position = legend_position
  ) %+replace%
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
    )
}
