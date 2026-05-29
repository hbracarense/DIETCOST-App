app_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

setwd(app_dir)

.libPaths(c(
  file.path(app_dir, "R", "library")
))

cat("APP DIR:", app_dir, "\n")
cat("R VERSION:", R.version.string, "\n")
cat("R HOME:", R.home(), "\n")
cat("LIB PATHS:\n")
print(.libPaths())

options(
  shiny.launch.browser = TRUE,
  shiny.host = "127.0.0.1"
)

shiny::runApp(
  appDir = app_dir,
  host = "127.0.0.1",
  launch.browser = TRUE
)