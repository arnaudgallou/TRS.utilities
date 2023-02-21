#' @title Compose JAGS-compatible data
#' @description Turn a data frame into a list of data to pass on to a JAGS model.
#' @param data A data frame.
#' @param matrix_formula A formula.
#' @return A list of data to pass on to a JAGS model.
#' @export
compose_mdl_data <- function(data, matrix_formula = NULL) {
  data[] <- lapply(data, \(x) if (is.character(x)) as.factor(x) else x)
  data_ls <- as.list(data)

  n_data_ls <- lapply(data_ls, \(x) if (is.factor(x)) length(unique(x)) else NA)
  n_data_ls <- setNames(n_data_ls, paste0("n_", names(n_data_ls)))
  n_data_ls <- Filter(Negate(anyNA), n_data_ls)

  n_data <- length(data_ls[[1]])

  model_matrix_ls <- list()

  if (!is.null(matrix_formula)) {
    model_matrix <- model.matrix(matrix_formula, data)
    model_matrix_ls <- list(
      model_matrix = model_matrix,
      n_param = ncol(model_matrix)
    )
  }

  c(data_ls, n_data_ls, n = n_data, model_matrix_ls)
}
