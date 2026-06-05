library(shiny)

# Entry point when run via shiny::runApp() or launch_ersim_app()
shinyApp(
  ui     = source("ui.R",     local = TRUE)$value,
  server = source("server.R", local = TRUE)$value
)
