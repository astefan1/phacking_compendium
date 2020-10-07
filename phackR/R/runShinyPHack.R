runShinyPHack <- function() {
  appDir <- system.file("shiny-phack", "ShinyPHack", package = "phackR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `phackR`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}