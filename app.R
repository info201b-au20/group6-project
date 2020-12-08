# Load Shiny
library("shiny")

# Source the ui and server (set wdir to root first)
source("app_ui.R")
source("app_server.R")

shinyApp(ui = ui, server = server)