#' @title Load a JAGS model
#' @description Load a file containing a JAGS model and parameters.
#' @param file File containing a JAGS model.
#' @return A list of model-related data that includes the name of the model, parameters to save, file path of the model and the actual model to run.
#' @export
load_mdl <- function(file) {
  mdl <- mdl_param <- NULL
  source(file, local = TRUE)
  mdl_name <- extract_file_name(file)
  list(model = mdl, param = mdl_param, name = mdl_name, file = file)
}
