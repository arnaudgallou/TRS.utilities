setup_gis <- function () {
  proj <- "TRS.Rproj"

  if (!file.exists(proj)) {
    stop("'setup_gis' must be executed in ", proj, ".")
  }

  x <- read_csv("main_df.csv", col_select = "location")
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
