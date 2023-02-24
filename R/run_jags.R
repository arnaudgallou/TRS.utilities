#' @title Run a JAGS model
#' @description Run a JAGS model.
#' @param x Data to pass on to a JAGS model. See [`R2jags::jags()`] for details.
#' @param model List containing a JAGS model and parameters.
#' @param ... Other arguments to pass on to [`R2jags::jags()`].
#' @param save Should the JAGS output be saved?
#' @param path Directory to save the file in.
#' @param filename Name (without extension) of the file. Leave `filename=NULL`
#'   to use a default file name based on the model settings.
#' @param default_output Should `run_jags()` return the model object returned by
#'   [`R2jags::jags()`]?
#' @export
run_jags <- function(
    x,
    model,
    ...,
    save = FALSE,
    path,
    filename = NULL,
    default_output = FALSE
) {
  if (is_true(save) && missing(path)) {
    stop('"path" is missing, with no default.')
  }
  suppressWarnings(
    out <- R2jags::jags(
      x$data,
      parameters.to.save = model$param,
      model.file = model$model,
      DIC = FALSE,
      ...
    )
  )
  if (is_false(default_output)) {
    out <- make_jags_output(x, model$name, out)
  }
  if (is_false(save)) {
    return(out)
  }
  save_jags(out, x$settings, path, filename)
}

make_jags_output <- function(x, mdl_name, fit) {
  terms <- x$settings$terms
  vars <- terms[!grepl("land_type", terms)]
  expl_vars <- setNames(terms, paste0("expl_var", name_suffix(terms)))
  sims <- jags_array_to_tibble(fit$BUGSoutput$sims.array)
  tbl_data <- tbl_elev_grad_clim <- NULL
  if (mdl_name == "global_scale_mdl") {
    tbl_data_settings <- data_settings(x, expl_vars, "global")
    tbl_data <- tibble(
      tbl_data_settings,
      as_tibble(x$data[names(x$data) %in% x$settings$cols]),
      est_range_mean = fit$BUGSoutput$mean$alpha,
      est_range_sd = fit$BUGSoutput$sd$alpha,
      se_min = .data$est_range_mean - .data$est_range_sd,
      se_max = .data$est_range_mean + .data$est_range_sd
    )
    if (length(vars) == 1L) {
      tbl_data <- rename(tbl_data, x = !!enquo(vars))
    }
  } else {
    tbl_data_settings <- data_settings(x, expl_vars, "local")
  }
  if (x$settings$average) {
    tbl_elev_grad_clim <- x$elev_grad_clim
  }
  list(
    sims = tibble(tbl_data_settings, sims),
    summary = tibble(tbl_data_settings, as_tibble(
      fit$BUGSoutput$summary,
      rownames = "parameter",
      .name_repair = make_clean_names
    )),
    stats = jags_statistics(fit),
    data = tbl_data,
    elev_grad_clim = tbl_elev_grad_clim,
    settings = c(x$settings, jags_settings(fit)),
    saved_parameters = fit$parameters.to.save,
    model = fit$model$model()
  )
}

data_settings <- function(x, expl_vars, scope) {
  out <- tibble(
    as_tibble_row(expl_vars),
    exclusion_zone = x$settings$exclusion_zone
  )
  if (scope == "local") {
    return(out)
  }
  tibble(
    out,
    elevation_span = x$settings$elevation_span,
    std_from = x$settings$std_from
  )
}

jags_statistics <- function(x) {
  out <- x$BUGSoutput
  list(mean = out$mean, median = out$median, sd = out$sd)
}

jags_settings <- function(x) {
  out <- x$BUGSoutput
  list(
    n_chains = out$n.chains,
    n_iter = out$n.iter,
    n_burnin = out$n.burnin,
    n_thin = out$n.thin,
    n_keep = out$n.keep,
    n_sims = out$n.sims
  )
}

jags_array_to_tibble <- function(array) {
  out <- as.data.frame.table(array, base = list(as.character(seq_along(array))))
  out <- as_tibble(out)
  out <- rename(out, iter = .data$Var1, chain = .data$Var2)
  out <- mutate(out, iter = as.integer(.data$iter))
  out <- pivot_wider(out, names_from = .data$Var3, values_from = .data$Freq)
  clean_names(out)
}

make_filename <- function(x, filename) {
  if (!is.null(filename)) {
    return(glue("{filename}.rds"))
  }
  tail <- if (is_true(x$std_elev_grad)) glue("-{x$std_from}") else ""
  terms <- paste(x$terms, collapse = "-")
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
