#' @title Create directories for GIS analyses
#' @description Create the required directories to run the GIS analyses in
#' [Diurnal temperature range as a key predictor of plants' elevation ranges globally]
#' (https://github.com/arnaudgallou/TRS)
#' @export
setup_gis <- function() {
  proj <- "TRS.Rproj"
  trs <- "data/trs.csv"

  if (!file.exists(proj)) {
    stop('function "setup_gis" must be executed in ', proj, ".")
  }
  if (!file.exists(trs)) {
    stop(basename(trs), ' could not be found in the "data" directory.')
  }

  x <- read_csv(trs, col_select = "location")
  mountains <- unique(x$location)

  dir_data <- c(
    glue('extracted/{c("past", "present")}'),
    "data/present",
    glue('data/past/{c("base", "var/mean precipitation", "var/mean temperature")}')
  )

  for (i in dir_data) {
    make_dir(glue("gis/clim/{i}"))
  }

  for (i in mountains) {
    make_dir(glue("gis/srtm/{i}"))
  }
}
