# R/modules/eva_module.R

library(shiny)
library(ggplot2)
library(extRemes)

# Module UI
eva_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("weather_var"),
      "Climate Variable:",
      choices = list(
        "Daily Max Temperature (°C)" = "tmax",
        "Daily Total Precipitation (mm)" = "precip"
      )
    ),
    numericInput(
      ns("return_period"),
      "Target Return Period (Years):",
      value = 100,
      min = 2,
      max = 1000
    ),
    actionButton(
      ns("run_eva"),
      "Run Extreme Value Analysis",
      class = "btn-primary w-100 mt-2 mb-3"
    ),
    hr(),
    verbatimTextOutput(ns("station_summary")),
    plotOutput(ns("eva_plot"), height = "320px")
  )
}

# Module Server
eva_server <- function(id, station_reactive) {
  moduleServer(id, function(input, output, session) {
    # Fetch data reactively when station or variable changes
    station_data <- reactive({
      stn <- station_reactive()
      fetch_station_history(stn$id, input$weather_var)
    })

    # Run EVA model upon button click
    eva_results <- eventReactive(
      input$run_eva,
      {
        df <- station_data()
        fit <- fevd(df$Value, type = "GEV")
        r_periods <- c(2, 5, 10, 25, 50, 100, 200, 500)
        rl <- return.level(fit, return.period = r_periods)
        list(
          fit = fit,
          levels = data.frame(Period = r_periods, Level = as.numeric(rl))
        )
      },
      ignoreNULL = FALSE
    )

    # Station & Model Summary Text
    output$station_summary <- renderPrint({
      stn <- station_reactive()
      res <- eva_results()
      target_p <- input$return_period
      approx_val <- approx(
        res$levels$Period,
        res$levels$Level,
        xout = target_p
      )$y

      cat("Selected Station: ", stn$name, "\n")
      cat(
        "Location ID:      ",
        stn$id,
        " (",
        stn$lat,
        "°N, ",
        stn$lon,
        "°E)\n",
        sep = ""
      )
      cat(sprintf(
        "Estimated %d-Yr Return Level: %.2f units\n",
        target_p,
        approx_val
      ))
    })

    # Plot Output
    output$eva_plot <- renderPlot({
      res <- eva_results()
      df <- res$levels

      ggplot(df, aes(x = Period, y = Level)) +
        geom_line(color = "#0d6efd", linewidth = 1.2) +
        geom_point(color = "#0d6efd", size = 3) +
        scale_x_log10(breaks = df$Period) +
        theme_minimal(base_size = 13) +
        labs(
          title = "Return Level Curve (GEV Distribution)",
          x = "Return Period (Years, Log Scale)",
          y = "Extreme Value Level"
        ) +
        theme(panel.grid.minor = element_blank())
    })
  })
}
