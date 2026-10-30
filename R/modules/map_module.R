# R/modules/map_module.R

library(shiny)
library(leaflet)

# Module UI
map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("station_map"), height = "500px")
  )
}

# Module Server
map_server <- function(id, stations_df) {
  moduleServer(id, function(input, output, session) {
    output$station_map <- renderLeaflet({
      leaflet(stations_df) %>%
        addProviderTiles("Esri.WorldTopoMap") |>
        #addProviderTiles("CartoDB.DarkMatter") |>
        setView(lng = 10.0, lat = 45.0, zoom = 3) |>
        addMarkers(
          lng = ~lon,
          lat = ~lat,
          popup = ~name,
          layerId = ~id
        )
    })

    # Reactive expression returning the currently selected station row
    reactive({
      click <- input$station_map_marker_click
      if (is.null(click)) {
        stations_df[1, ] # Default fallback to first station
      } else {
        stations_df[stations_df$id == click$id, ]
      }
    })
  })
}
