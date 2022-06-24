#' @title Load model
#' @description Load the global- or local-analyses model file.
#' @param scope Scope of the model to load. One of `c("global", "local")`.
#' @export
load_model <- function(scope = c("global", "local")) {
  scope <- match.arg(scope)
  path <- glue("R/models/{scope}_scale_mdl.R")
  mdl <- mdl_params <- NULL
  source(path, local = TRUE)
  mdl_name <- extract_file_name(path)
  list(
    model = mdl,
    params = mdl_params,
    name = mdl_name,
    scope = scope
  )
}
