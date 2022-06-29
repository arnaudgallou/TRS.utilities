#' @title Run a JAGS model
#' @description Run a JAGS model.
#' @param x Data to pass on to a JAGS model. See [`R2jags::jags()`] for details.
#' @param model List containing a JAGS model and parameters.
#' @param ... Other arguments to pass on to [`R2jags::jags()`].
#' @param save Should the JAGS output be saved?
#' @param path Directory to save the file in.
#' @param filename Name (without extension) of the file. Leave `filename=NULL` to use a default file name based on the model settings.
#' @param default_output Should `run_jags()` return the model object returned by [`R2jags::jags()`]?
#' @export
run_jags <- function(x, model, ..., save = FALSE, path, filename = NULL, default_output = FALSE) {
  if (isTRUE(save) && missing(path)) {
    stop('"path" is missing, with no default.')
  }

  suppressWarnings(
    out <- R2jags::jags(x$data, parameters.to.save = model$param, model.file = model$model, DIC = FALSE, ...)
  )

  if (!isTRUE(default_output)) {
    out <- make_jags_output(x, model$name, out)
  }

  if (isTRUE(save)) {
    save_jags(out, x$settings, path, filename)
  } else {
    out
  }
}


make_jags_output <- function(x, mdl_name, fit) {
  vars <- x$settings$terms
  clim_vars <- vars[!grepl("land_type", vars)]
  expl_vars <- paste(vars, collapse = "_")

  sims <- jags_array_to_tibble(fit$BUGSoutput$sims.array)

  tbl_data <- tbl_elev_grad_clim <- NULL

  if (mdl_name == "global_scale_mdl") {
    settings_min <- tibble(
      expl_var = expl_vars,
      elevation_span = x$settings$elevation_span,
      exclusion_zone = x$settings$exclusion_zone,
      std_from = x$settings$std_from
    )

    tbl_data <- tibble(
      settings_min,
      as_tibble(x$data[names(x$data) %in% x$settings$cols]),
      est_range_mean = fit$BUGSoutput$mean$alpha,
      est_range_sd = fit$BUGSoutput$sd$alpha
    )

    if (length(clim_vars) == 1) {
      tbl_data <- rename(tbl_data, bioclim = !!enquo(clim_vars))
    }
  } else {
    settings_min <- tibble(
      expl_var = expl_vars,
      exclusion_zone = x$settings$exclusion_zone
    )
  }

  if (x$settings$average) {
    tbl_elev_grad_clim <- x$elev_grad_clim
  }

  out <- list(
    sims = tibble(settings_min, sims),
    summary = tibble(
      settings_min,
      as_tibble(
        fit$BUGSoutput$summary,
        rownames = "parameter",
        .name_repair = make_clean_names
      )
    ),
    stats = list(
      mean = fit$BUGSoutput$mean,
      median = fit$BUGSoutput$median,
      sd = fit$BUGSoutput$sd
    ),
    data = tbl_data,
    elev_grad_clim = tbl_elev_grad_clim,
    settings = c(
      x$settings,
      list(
        n_chains = fit$BUGSoutput$n.chains,
        n_iter = fit$BUGSoutput$n.iter,
        n_burnin = fit$BUGSoutput$n.burnin,
        n_thin = fit$BUGSoutput$n.thin,
        n_keep = fit$BUGSoutput$n.keep,
        n_sims = fit$BUGSoutput$n.sims
      )
    ),
    saved_parameters = fit$parameters.to.save,
    model = fit$model$model()
  )

  out
}


jags_array_to_tibble <- function(array) {
  array %>%
    as.data.frame.table(base = list(as.character(seq_along(array)))) %>%
    as_tibble() %>%
    rename(iter = .data$Var1, chain = .data$Var2) %>%
    mutate(iter = as.integer(.data$iter)) %>%
    pivot_wider(names_from = .data$Var3, values_from = .data$Freq) %>%
    clean_names()
}


make_filename <- function(x, filename) {
  if (!is.null(filename)) {
    return(glue("{filename}.rds"))
  }
  tail <- if (!is.null(x$std_from)) glue("-{x$std_from}") else ""
  terms <- paste(x$terms, collapse = "_")
  glue("{terms}-span_{x$elevation_span}-excl_{x$exclusion_zone}{tail}.rds")
}


save_jags <- function(x, settings, path, filename) {
  if (!dir.exists(path)) {
    make_dir(path)
  }
  filename <- make_filename(settings, filename)
  file <- glue(path, filename)
  write_rds(x, file)
}
