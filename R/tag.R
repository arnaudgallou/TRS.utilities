#' @title Add a tag to a plot
#' @description Add a tag inside the plot region.
#' @param tag Tag to display.
#' @param position Position of the tag. One of `topleft`, `topright`, `bottomleft`,
#'   `bottomright`.
#' @param margin Numeric vector indicating the space between the tag element and
#'   border of the plot region. If two values are passed, the first value defines
#'   the horizontal margin while the second value defines the vertical margin.
#' @param fontface Font face of the tag element.
#' @param geom Name of geom to use for annotation.
#' @param ... Other arguments passed on to [`annotate()`][ggplot2::annotate()].
#' @export
tag <- function(
    tag,
    position = c("topleft", "topright", "bottomleft", "bottomright"),
    margin,
    fontface = 2,
    geom = "text",
    ...
) {
  if (!missing(margin)) {
    if (!is.numeric(margin)) {
      abort("`margin` must be a numeric vector.")
    }
    if (length(margin) > 2) {
      abort("`margin` must be a vector of a single or two elements.")
    }
  }
  position <- match.arg(position)
  is_pos_left <- position %in% c("topleft", "bottomleft")
  is_pos_top <- position %in% c("topleft", "topright")
  if (missing(margin)) {
    margin <- c(
      if (is_pos_left) -.5 else 1,
      if (is_pos_top) 1 else 0
    )
  } else {
    if (length(margin) == 1) {
      margin <- rep(margin, 2)
    }
    margin <- switch(
      position,
      "topleft" = c(-margin[1], 1 + margin[2]),
      "topright" = c(1 + margin[1], 1 + margin[2]),
      "bottomleft" = c(-margin[1], -margin[2]),
      "bottomright" = c(1 + margin[1], -margin[2])
    )
  }
  annotate(
    geom,
    label = tag,
    x = if (is_pos_left) -Inf else Inf,
    y = if (is_pos_top) Inf else -Inf,
    hjust = margin[1],
    vjust = margin[2],
    fontface = fontface,
    ...
  )
}
