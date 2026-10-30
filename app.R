# ==========================================
# GTS Extreme Explorer - Main Application
# ==========================================

library(shiny)
library(bslib)

# Source supporting files and modules
source("R/utils.R")
source("R/modules/map_module.R")
source("R/modules/eva_module.R")

# Load initial global station metadata
stations_df <- get_station_metadata()

# 1. UI Layout
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  title = "GTS Extreme Value Explorer",

  sidebar = sidebar(
    title = "Analysis Controls",
    eva_ui("eva_mod")
  ),

  card(
    full_screen = TRUE,
    card_header("Global Station Map (Click a marker to analyze)"),
    map_ui("map_mod")
  )
)

# 2. Server Logic
server <- function(input, output, session) {
  # Call map module and capture selected station reactive object
  selected_station <- map_server("map_mod", stations_df)

  # Pass selected station into the EVA statistical module
  eva_server("eva_mod", selected_station)
}

# Run App
shinyApp(ui = ui, server = server)
