#' @title Global- and local-scale analyses
#' @description Closures to plot model estimates and get model statistics.
#' @param path Path to a directory containing model objects returned by [`run_jags()`].
#' @name global_local_analyses
NULL


#' @rdname global_local_analyses
#' @export
global_analyses <- function(path) {
  .get_fls = function(vars, elevation_span, exclusion_zone, std_from, ..., parent_frame = 2) {
    params <- enexprs(vars, elevation_span, exclusion_zone, std_from)
    assign_to_missing(!!!params, values = ".+", rep = TRUE, parent_frame = parent_frame)
    target = glue("(?:{vars})-span_{elevation_span}-excl_{exclusion_zone}-{std_from}\\.rds")
    list_files(path, target, ..., err_on_empty = TRUE)
  }

  .get_data = function(.fls, ...) {
    read_jags(.fls, "data", ...)
  }

  .get_sims = function(.fls, ...) {
    read_jags(.fls, "sims", ...)
  }

  .calc_pred_conf = function(.fls) {
    imap_dfr(.fls, ~ {
      data <- read_rds(.x)
      fit <- get_jags_sims(data, "beta")

      data %>%
        pluck("elev_grad_clim") %>%
        group_by(land_type) %>%
        modelr::data_grid(
          value = modelr::seq_range(.data[[.y]], n = 100),
          expl_clim = .y
        ) %>%
        ungroup() %>%
        add_pred_conf(fit, ~value * land_type)
    })
  }

  .get_regression_args = function(type, label) {
    if (isTRUE(label)) {
      label_args <- list(
        show_draws = FALSE,
        error_bars = FALSE,
        point_labels = TRUE,
        label_colors = c("#e34326", "#3176a9")
      )
    }
    if (type == "land_type") {
      args <- list(
        facet_by = "expl_clim",
        land_type = TRUE,
        ribbon_colors = c("#f57a5b", "#85a9d6")
      )
    } else {
      args <- list(facet_by = "expl_var")
    }
    c(args, if (isTRUE(label) && type != "land_type") label_args)
  }

  plot_regressions_ = function(
    vars,
    elevation_span,
    exclusion_zone,
    std_from = c("top", "bottom"),
    type = c("spaghetti", "land_type"),
    point_labels = FALSE,
    ...
  ) {
    type <- match.arg(type)
    is_land_type <- type == "land_type"
    vars <- if (is_land_type) type else vars
    get_nm <- function(x) if (is_land_type) string_replace(basename(x), "_land.+", "") else NULL
    fls <- .get_fls(
      vars,
      elevation_span,
      exclusion_zone,
      match.arg(std_from),
      get_nm,
      parent_frame = 3
    )
    data <- .get_data(fls, .id = if (is_land_type) "expl_clim")
    fits <- if (is_land_type) .calc_pred_conf(fls) else .get_sims(fls)
    args <- .get_regression_args(type, point_labels)
    do.call(plot_regressions, c(list(data, fits), modifyList(args, list(...))))
  }

  .make_posterior_data = function(x, yvar, prob, outer_prob, scales, param_names, reverse) {
    x <- x %>%
      group_by(.data$expl_var, .data$elevation_span, .data$exclusion_zone) %>%
      add_posterior_density(.data$beta_2, prob, outer_prob) %>%
      ungroup()

    if (!is.null(scales)) {
      x <- mutate(x, across(
        starts_with("y"),
        ~ case_when(!!!parse_exprs(
          glue(".data$expl_var == '{names(scales)}' ~ .x * {scales}")
        ))
      ))
    } else {
      x <- mutate(x, across(starts_with("y"), ~ .x * .01))
    }

    x <- x %>% mutate(
      "{yvar}" := {
        yvar_unique <- unique(.data[[yvar]])
        yvar_levels <- if (reverse) rev(yvar_unique) else yvar_unique
        factor(.data[[yvar]], levels = yvar_levels)
      }
    )

    if (!is.null(param_names)) {
      x <- x %>% mutate(
        expl_var = .data$expl_var %>%
          string_replace_all(param_names) %>%
          factor(levels = param_names)
      )
    }

    x
  }

  plot_posterior_distributions_ = function(
    ...,
    yvar,
    prob = c(.8, .95),
    outer_prob = 0.99,
    scales = NULL,
    param_names = NULL,
    reverse = TRUE,
    facet = TRUE
  ) {
    if (!is.null(scales) & !(has_names(scales) & is.vector(scales))) {
      stop("`scales` must be a named vector.")
    }

    if (isTRUE(facet)) {
      facet_args <- list(
        rows = vars(.data$elevation_span),
        cols = vars(.data$expl_var),
        scales = "free_x",
        switch = "y",
        labeller = label_parsed
      )
      fill <- c("#5E8CBA", "#CB624D", "#F5B83D")
      color <- c("#1F4C7A", "#8F3624", "#915F08")
    } else {
      facet_args <- list()
      fill <- rep("#5E8CBA", 3)
      color <- rep("#1F4C7A", 3)
    }

    .get_fls(...) %>%
      .get_sims("span|zone|expl|beta_2") %>%
      .make_posterior_data(yvar, prob, outer_prob, scales, param_names, reverse) %>%
      posterior_dist(
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
      scale_color_manual(values = color) +
      xlab("Posterior parameter estimates") +
      theme_blank_y() +
      theme(strip.placement = "outside")
  }

  eval_models_ = function(...) {
    .get_fls(...) %>%
      eval_models() %>%
      round_num(2)
  }

  get_statistical_details_ = function(...) {
    .get_fls(...) %>%
      map_df(~ read_rds(.x) %>% get_statistical_details()) %>%
      round_num(2) %>%
      mutate(interaction = if_else(grepl("\\*", .data$model), 1, 0)) %>%
      arrange(
        desc(.data$std_from),
        .data$interaction,
        .data$elevation_span,
        .data$exclusion_zone,
        .data$model
      ) %>%
      select(-interaction)
  }

  make_list(
    plot_regressions_,
    plot_posterior_distributions_,
    eval_models_,
    get_statistical_details_
  )
}


#' @rdname global_local_analyses
#' @export
local_analyses <- function(path) {
  data = read_jags(list_files(path, "\\.rds$"), "summary", vars = "beta")

  .get_data = function(excl_zone, extend_summary = FALSE) {
    if (missing(excl_zone)) {
      out <- data
    } else {
      out <- filter(data, .data$exclusion_zone == excl_zone)
    }
    if (extend_summary) {
      out <- extend_jags_summary(out)
    }
    out
  }

  plot_slope_histogram_ = function(exclusion_zone) {
    .get_data(exclusion_zone, extend_summary = TRUE) %>%
      ggplot(aes(.data$beta_std, fill = .data$x95_ci)) +
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
      add_facet_lines()
  }

  # x Dataset containing elevation spans
  plot_influence_elev_span_ = function(x, exclusion_zone = 250) {
    location_elev_span <- x %>%
      group_by(.data$location, .data$elev_span) %>%
      summarize(.groups = "drop") %>%
      rowid_to_column()

    .get_data(exclusion_zone) %>%
      mutate(
        parameter = first_word(.data$parameter),
        y = .data$mean / .data$sd,
        expl_var = toupper(.data$expl_var) %>% factor(c("TS", "DTR"))
      ) %>%
      left_join(location_elev_span, by = "rowid") %>%
      ggplot(aes(
        .data$elev_span,
        .data$y,
        color = .data$expl_var,
        fill = .data$expl_var,
        alpha = .data$expl_var
      )) +
      facet_wrap(vars(.data$expl_var)) +
      line_0("y") +
      geom_smooth(method = "lm", size = .5, color = "#85A9D6", fill = "#85A9D6") +
      scale_alpha_manual(values = c(.15, .15)) +
      labs(
        x = "Elevational gradient length (m)",
        y = "Mean slopes / SD slopes"
      ) +
      add_facet_lines()
  }

  get_slope_summary_ = function() {
    .get_data(extend_summary = TRUE) %>%
      group_by(.data$expl_var, .data$exclusion_zone) %>%
      summarize_slopes() %>%
      relocate(contains("low"), .before = "positive_high_uncertainties") %>%
      rename(model = .data$expl_var) %>%
      round_num(0)
  }

  make_list(
    plot_slope_histogram_,
    plot_influence_elev_span_,
    get_slope_summary_
  )
}
