#' @title Global- and local-scale analyses
#' @description Classes to plot model estimates and get model statistics.
#' @examples
#' \dontrun{
#'
#' # Global scale analyses
#' ga <- GlobalAnalyses$new(path)
#'
#' ga$regressions(vars = "dtr", elev_span = 2500, excl_zone = 250, labels = "DTR")
#'
#' ga$posterior_distributions(vars = c("dtr", "ts"))
#'
#' # Local scale analyses
#' la <- LocalAnalyses(path)
#'
#' la$slope_histograms()
#' }
#' @name global_local_analyses
NULL


#' @rdname global_local_analyses
#' @description NULL
#' @export
GlobalAnalyses <- R6::R6Class(
  classname = "GlobalAnalyses",
  public = list(
    #' @param path Path to a directory containing the outputs of the global-scale
    #'   analyses.
    initialize = function(path) {
      private$path <- path
    },

    #' @description Plot regression fits.
    #' @param vars A character vector of variables to plot.
    #' @param elev_span Length of elevation spans to use.
    #' @param excl_zone Length of exclusion zones to use.
    #' @param std_from Edge of elevational gradients to use. One of `top`, `bottom`,
    #'   `none`. Use `none` for data that were not standardized.
    #' @param labels A character vector used to label the x axis. See
    #'   [`regressions()`] for details.
    #' @param by_land_type Should regressions be drawn for each land type?
    #' @param point_labels Should point labels be shown?
    regressions = function(
      vars,
      elev_span = NULL,
      excl_zone = NULL,
      std_from = c("top", "bottom", "none"),
      labels = NULL,
      by_land_type = FALSE,
      point_labels = FALSE
    ) {
      fls <- private$get_files(
        vars,
        elev_span,
        excl_zone,
        std_from,
        names = extract_expl_var
      )
      data <- make_regression_data(fls, by_land_type)
      regressions(data, labels, point_labels = point_labels)
    },

    #' @description Plot posterior distribution from a bayesian model.
    #' @param vars A character vector of variables to plot.
    #' @param elev_span Length of elevation spans to use.
    #' @param excl_zone Length of exclusion zones to use.
    #' @param std_from Edge of elevational gradients to use. One of `top`, `bottom`,
    #'   `none`. Use `none` for data that were not standardized.
    #' @param yvar Variable describing the vertical axis.
    #' @param prob The probability mass to include in the shaded region.
    #' @param prob_outer The probability mass to include in the outer interval.
    #' @param scales A scaling factor to scale the height of the ridgelines. See
    #'   [`ggridges::geom_ridgeline()`][] for details.
    #' @param labels A character vector used to label the x axis. See
    #'   [`posterior_distributions()`] for details.
    #' @param reverse Should the order of posterior distributions be reversed?
    #' @param facet Should the plot be facetted?
    #' @param fill A vector of colors to use for filling posterior distributions.
    posterior_distributions = function(
      vars,
      elev_span = NULL,
      excl_zone = NULL,
      std_from = c("top", "bottom", "none"),
      yvar = "exclusion_zone",
      prob = c(.8, .95),
      prob_outer = .99,
      scales = .01,
      labels = NULL,
      reverse = FALSE,
      facet = TRUE,
      fill = NULL
    ) {
      if (is_true(facet)) {
        facet_args <- list(
          rows = vars(.data$elevation_span),
          cols = vars(.data$expl_var),
          scales = "free_x",
          switch = "y",
          labeller = label_parsed
        )
      } else {
        facet_args <- list()
      }
      fls <- private$get_files(vars, elev_span, excl_zone, std_from)
      data <- make_posterior_data(fls, yvar, prob, prob_outer, scales, labels, reverse)
      fill <- fill %||% rep("#5E8CBA", length(unique(data[[yvar]])))
      posterior_distributions(
        data,
        aes(
          .data$x,
          .data[[yvar]],
          height = .data$y,
          color = .data[[yvar]],
          fill = .data[[yvar]]
        ),
        vline_color = "grey70",
        vline_type = 2,
        facet_args = facet_args
      ) +
        scale_fill_manual(values = fill) +
        scale_color_manual(values = darken(fill, .5, "HLS")) +
        xlab("Posterior parameter estimates") +
        theme_blank_y() +
        theme(strip.placement = "outside")
    },

    #' @description Compare multiple JAGS models using the WAIC and leave-one-out
    #'   cross-validation methods.
    #' @param vars A character vector of variables to plot.
    #' @param elev_span Length of elevation spans to use.
    #' @param excl_zone Length of exclusion zones to use.
    #' @param std_from Edge of elevational gradients to use. One of `top`, `bottom`,
    #'   `none`. Use `none` for data that were not standardized.
    #' @return A tibble.
    eval_models = function(
      vars = "all",
      elev_span = 2500,
      excl_zone = 250,
      std_from = c("top", "bottom", "none")
    ) {
      fls <- private$get_files(vars, elev_span, excl_zone, std_from)
      out <- eval_models(fls)
      round_num(out, 2)
    },

    #' @description Compute Bayesian R-squared and the probability of posterior
    #'   fits to be lower than 0
    #' @param vars A character vector of variables to plot.
    #' @param elev_span Length of elevation spans to use.
    #' @param excl_zone Length of exclusion zones to use.
    #' @param std_from Edge of elevational gradients to use. One of `top`, `bottom`,
    #'   `none`. Use `none` for data that were not standardized.
    #' @return A tibble.
    get_statistical_details = function(
      vars = "all",
      elev_span = NULL,
      excl_zone = NULL,
      std_from = c("top", "bottom", "none")
    ) {
      fls <- private$get_files(vars, elev_span, excl_zone, std_from)
      out <- map(fls, \(file) {
        data <- read_rds(file)
        get_statistical_details(data)
      })
      out <- list_rbind(out)
      out <- round_num(out, 2)
      out <- mutate(out, interaction = if_else(grepl("\\*", .data$model), 1, 0))
      out <- arrange(
        out,
        desc(.data$std_from),
        .data$interaction,
        .data$elevation_span,
        .data$exclusion_zone,
        .data$model
      )
      select(out, -interaction)
    }
  ),
  private = list(
    path = NULL,

    get_files = function(
      vars,
      elev_span = NULL,
      excl_zone = NULL,
      std_from = c("top", "bottom", "none"),
      ...
    ) {
      std_from <- match.arg(std_from)
      vars <- if (vars == "all") ".+" else vars
      elev_span <- elev_span %||% "[^-]+"
      excl_zone <- excl_zone %||% "[^-]+"
      vars <- paste(vars, collapse = "|")
      if (std_from == "none") {
        tail <- ""
      } else {
        tail <- glue("-{std_from}")
      }
      fmt <- "(?:{vars})-span_{elev_span}-excl_{excl_zone}{tail}\\.rds"
      list_files(private$path, target = glue(fmt), ...)
    }
  )
)

extract_expl_var = function(x) {
  string_extract(basename(x), "^[^-]+")
}


#' @rdname global_local_analyses
#' @description NULL
#' @export
LocalAnalyses <- R6::R6Class(
  classname = "LocalAnalyses",
  public = list(
    #' @param path Path to a directory containing the outputs of the local-scale
    #'   analyses.
    initialize = function(path) {
      private$read_data(path)
    },

    #' @description Plot histograms summarizing slope estimates in each location.
    #' @param excl_zone Size of exclusion zones to use. By default, make a
    #'   facetted plot using all exclusion zone sizes from the data.
    slope_histograms = function(excl_zone = NULL) {
      out <- private$get_data_summary(excl_zone, extend_summary = TRUE)
      ggplot(out, aes(.data$beta_std, fill = .data$x95_ci)) +
        facet_grid(
          rows = vars(.data$exclusion_zone),
          cols = vars(.data$expl_var),
          scales = "free_y"
        ) +
        geom_histogram(
          position = "identity",
          binwidth = 2.5,
          color = "white",
          size = .3
        ) +
        line_0("x") +
        scale_fill_manual(values = c("#006699", "#D4E5ED")) +
        scale_y_continuous(breaks = seq(0, 12, 3)) +
        labs(x = "Mean slopes / SD slopes", y = "Count") +
        coord_cartesian(expand = FALSE) +
        theme_elesic() +
        add_facet_lines()
    },

    #' @description Plot the influence of elevation span on slope direction within
    #'   mountains.
    #' @param trs Dataset containing elevation spans.
    #' @param excl_zone Size of the exclusion zone to use.
    influence_elev_span = function(trs, excl_zone = 250) {
      location_details <- private$get_location_details(trs)
      out <- private$get_data_summary(excl_zone)
      out <- mutate(
        out,
        parameter = first_word(.data$parameter),
        y = .data$mean / .data$sd
      )
      out <- left_join(out, location_details, by = "rowid")
      n_facets <- length(out$expl_var)
      ggplot(out, aes(
        .data$elev_span,
        .data$y,
        color = .data$expl_var,
        fill = .data$expl_var,
        alpha = .data$expl_var
      )) +
        facet_wrap(vars(.data$expl_var)) +
        line_0("y") +
        geom_smooth(method = "lm", size = .5, color = "#85A9D6", fill = "#85A9D6") +
        scale_alpha_manual(values = rep(.15, n_facets)) +
        labs(
          x = "Elevational gradient length (m)",
          y = "Mean slopes / SD slopes"
        ) +
        theme_elesic() +
        add_facet_lines()
    },

    #' @description Summarize the proportion of positive and negative slopes based
    #'   on the level of uncertainty. Values are given in percentage.
    #' @return A tibble.
    tbl_slope_summary = function() {
      out <- private$get_data_summary(extend_summary = TRUE)
      out <- group_by(out, .data$expl_var, .data$exclusion_zone)
      out <- summarize_slopes(out)
      out <- relocate(out, contains("low"), .before = "positive_high_uncertainties")
      out <- rename(out, model = expl_var)
      round_num(out, 0)
    }
  ),
  private = list(
    data = NULL,

    read_data = function(path) {
      fls <- list_files(path, "\\.rds$")
      private$data <- read_jags(fls, "summary", vars = "beta")
    },

    get_data_summary = function(excl_zone = NULL, extend_summary = FALSE) {
      out <- private$data
      if (!is.null(excl_zone)) {
        out <- filter(out, .data$exclusion_zone == excl_zone)
      }
      if (extend_summary) {
        out <- extend_jags_summary(out)
      }
      out
    },

    get_location_details = function(data) {
      out <- summarize(data, .by = c(location, elev_span))
      rowid_to_column(out)
    }
  )
)
