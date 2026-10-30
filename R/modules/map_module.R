# R/modules/map_module.R
# Dark station map with search and a minimum-record filter.
# Returns a reactive: the selected station (one-row data frame) or NULL.

library(shiny)
library(leaflet)

map_ui <- function(id) {
  ns <- NS(id)
  bslib::as_fillable_container(bslib::as_fill_item(
    div(
      class = "map-wrap",
      leafletOutput(ns("map"), height = "100%"),
      div(
        class = "map-panel",
        selectizeInput(
          ns("search"),
          NULL,
          choices = NULL,
          width = "100%",
          options = list(placeholder = "Search stations by name or WMO number")
        ),
        sliderInput(
          ns("min_years"),
          "Minimum years of record",
          min = 10,
          max = 80,
          value = 20,
          step = 5,
          width = "100%",
          ticks = FALSE
        ),
        textOutput(ns("count"), inline = TRUE)
      )
    )
  ))
}

map_server <- function(id, stations_df) {
  moduleServer(id, function(input, output, session) {
    pal <- colorNumeric(
      c("#46607F", "#5FA8D3", "#A6E3E9", "#FFF4C2"),
      domain = c(10, 100),
      na.color = "#46607F"
    )

    choices <- setNames(
      stations_df$id,
      sprintf(
        "%s (%s), WMO %s",
        stations_df$name,
        stations_df$country,
        stations_df$wmo_index
      )
    )
    updateSelectizeInput(
      session,
      "search",
      choices = choices,
      selected = "",
      server = TRUE
    )

    visible <- reactive({
      stations_df[stations_df$record_years >= input$min_years, , drop = FALSE]
    }) |>
      debounce(250)

    draw_markers <- function(map, df) {
      map |>
        clearGroup("stations") |>
        addCircleMarkers(
          data = df,
          lng = ~lon,
          lat = ~lat,
          layerId = ~id,
          group = "stations",
          radius = 4.5,
          weight = 1,
          color = "#0E1722",
          opacity = 0.9,
          fillColor = ~ pal(pmin(record_years, 100)),
          fillOpacity = 0.9,
          label = ~ sprintf("%s (%s), %d years", name, country, record_years),
          labelOptions = labelOptions(textsize = "13px")
        )
    }

    output$map <- renderLeaflet({
      leaflet(
        options = leafletOptions(
          preferCanvas = TRUE,
          minZoom = 2,
          worldCopyJump = TRUE
        )
      ) |>
        #addProviderTiles(providers$CartoDB.DarkMatter) |>
        addTiles(
          urlTemplate = paste0(
            "https://basemaps.cartocdn.com/rastertiles/dark_all/{z}/{x}/{y}.png?key=",
            Sys.getenv("CARTO_API_KEY")
          ),
          attribution = "&copy; <a href='https://carto.com/attributions'>CARTO</a> &copy; OpenStreetMap contributors",
          options = tileOptions(maxZoom = 20)
        ) |>
        setView(lng = 10, lat = 35, zoom = 2) |>
        draw_markers(isolate(visible())) |>
        addLegend(
          position = "bottomright",
          pal = pal,
          values = c(10, 100),
          title = "Years of record",
          opacity = 0.9,
          labFormat = labelFormat(suffix = "")
        )
    })

    observeEvent(
      visible(),
      {
        leafletProxy("map") |> draw_markers(visible())
      },
      ignoreInit = TRUE
    )

    output$count <- renderText({
      sprintf("%s stations shown", format(nrow(visible()), big.mark = ","))
    })

    selected_id <- reactiveVal(NULL)

    observeEvent(input$map_marker_click, {
      id <- input$map_marker_click$id
      if (!is.null(id) && !identical(id, selected_id())) {
        selected_id(id)
        updateSelectizeInput(
          session,
          "search",
          choices = choices,
          selected = id,
          server = TRUE
        )
      }
    })

    observeEvent(
      input$search,
      {
        req(nzchar(input$search))
        if (!identical(input$search, selected_id())) {
          selected_id(input$search)
          stn <- stations_df[stations_df$id == input$search, ]
          leafletProxy("map") |> flyTo(lng = stn$lon, lat = stn$lat, zoom = 7)
        }
      },
      ignoreInit = TRUE
    )

    # Ring around the selected station
    observeEvent(selected_id(), {
      stn <- stations_df[stations_df$id == selected_id(), ]
      leafletProxy("map") |>
        clearGroup("selected") |>
        addCircleMarkers(
          data = stn,
          lng = ~lon,
          lat = ~lat,
          group = "selected",
          radius = 9,
          weight = 2,
          color = "#FFFFFF",
          fill = FALSE,
          opacity = 1,
          options = pathOptions(interactive = FALSE)
        )
    })

    reactive({
      id <- selected_id()
      if (is.null(id)) {
        NULL
      } else {
        stations_df[stations_df$id == id, , drop = FALSE]
      }
    })
  })
}
