#' @title Prepare data for regression plots
#' @description Structure data for [`regressions()`].
#' @param files Files to get data from.
#' @param type Type of data to make. One of `c(draws, land_types)`.
#' @export
make_regression_data <- function(files, type = c("draws", "land_types")) {
  type <- match.arg(type)
  items <- c("data", "sims")
  out <- map(items, \(item) {
    if (type == "land_types" && item == "sims") {
      return(calc_pred_conf(files))
    }
    read_jags(files, item, .id = if (type == "land_types") "expl_var")
  })
  out <- set_names(out, items)
  structure(out, class = c(type, "tbl_df"))
}

calc_pred_conf <- function(files) {
  out <- imap(files, \(file, expl_var) {
    data <- read_rds(file)
    fit <- get_jags_sims(data, "beta")
    out <- pluck(data, "elev_grad_clim")
    out <- group_by(out, land_type)
    out <- modelr::data_grid(
      out,
      value = modelr::seq_range(.data[[expl_var]], n = 100),
      expl_var = expl_var
    )
    out <- ungroup(out)
    add_pred_conf(out, fit, formula = ~ value * land_type)
  })
  list_rbind(out)
}
