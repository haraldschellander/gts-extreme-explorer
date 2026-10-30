# R/modules/eva_module.R
# Controls (sidebar) and results (main panel) for the extreme value analysis.
# eva_ui() renders the controls, eva_results_ui() the results; one server.

library(shiny)
library(ggplot2)

eva_ui <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(
      ns("weather_var"), "Variable",
      choices = c(
        "Daily maximum temperature" = "tmax",
        "Daily precipitation" = "precip"
      )
    ),
    numericInput(ns("return_period"), "Return period (years)", value = 100, min = 2, max = 1000),
    sliderInput(
      ns("min_cov"), "Minimum data coverage per year (%)",
      min = 50, max = 95, value = 80, step = 5, ticks = FALSE
    )
  )
}

eva_results_ui <- function(id) {
  uiOutput(NS(id)("body"))
}

fmt_lat <- function(x) sprintf("%.2f°%s", abs(x), if (x >= 0) "N" else "S")
fmt_lon <- function(x) sprintf("%.2f°%s", abs(x), if (x >= 0) "E" else "W")

empty_state <- function() {
  div(
    class = "empty",
    h2("Choose a station"),
    p("Click a station on the map or search for it by name. Longer records give narrower confidence intervals, so the colour of each marker shows its years of record.")
  )
}

station_header <- function(stn) {
  elev <- if (is.na(stn$elev)) "" else sprintf(", %.0f m elevation", stn$elev)
  div(
    class = "stn-head",
    h2(stn$name),
    p(
      class = "stn-meta",
      sprintf(
        "%s, WMO %s. %s %s%s. Observations from %d to %d.",
        stn$country, stn$wmo_index, fmt_lat(stn$lat), fmt_lon(stn$lon), elev,
        stn$begin_year, stn$end_year
      )
    )
  )
}

kpi <- function(label, value, sub, primary = FALSE) {
  div(
    class = paste("kpi", if (primary) "primary"),
    div(class = "label", label),
    div(class = "value", value),
    div(class = "sub", sub)
  )
}

eva_server <- function(id, station_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    var_meta <- reactive(EVA_VARIABLES[[input$weather_var]])

    # Download (or read from cache), build annual maxima and fit the GEV.
    analysis <- reactive({
      stn <- station_reactive()
      req(stn)
      variable <- input$weather_var
      coverage <- input$min_cov / 100

      tryCatch(
        {
          hist <- withProgress(message = "Loading daily observations", value = 0, {
            fetch_station_history(stn, variable, coverage, on_progress = function(f) setProgress(f))
          })
          n <- nrow(hist$annual)
          if (n < MIN_ANNUAL_MAXIMA) {
            list(ok = FALSE, msg = sprintf(
              "Only %d years pass the coverage filter; at least %d are needed. Lower the minimum coverage or pick another station.",
              n, MIN_ANNUAL_MAXIMA
            ))
          } else {
            g <- fit_gev(hist$annual$Value)
            list(
              ok = TRUE, hist = hist, gev = g,
              curve = return_levels(g$fit, exp(seq(log(1.5), log(1000), length.out = 60))),
              emp = empirical_levels(hist$annual$Value)
            )
          }
        },
        error = function(e) list(ok = FALSE, msg = conditionMessage(e))
      )
    })

    target <- reactive({
      a <- analysis()
      req(a$ok, is.numeric(input$return_period), input$return_period >= 2, input$return_period <= 1000)
      return_levels(a$gev$fit, input$return_period)
    })

    output$body <- renderUI({
      stn <- station_reactive()
      if (is.null(stn)) {
        return(empty_state())
      }
      div(
        class = "results",
        style = sprintf("--eva-accent: %s;", var_meta()$accent),
        station_header(stn),
        uiOutput(ns("kpis")),
        div(class = "plot-block", h3("Return levels"), plotOutput(ns("rl_plot"), height = "280px")),
        div(class = "plot-block", h3("Annual maxima"), plotOutput(ns("am_plot"), height = "220px")),
        tags$details(
          class = "notes",
          tags$summary("About this analysis"),
          p("Each year's largest daily value is taken from NOAA NCEI Global Summary of the Day (GSOD), keeping only years with enough valid days. A generalised extreme value (GEV) distribution is fitted by maximum likelihood. Confidence intervals use a normal approximation and can be too narrow for short records or heavy tails."),
          p("GSOD is derived from synoptic and hourly reports. The reported daily maximum may not match the calendar day, and daily precipitation totals can be incomplete when only some reports were received.")
        )
      )
    })

    output$kpis <- renderUI({
      a <- analysis()
      if (!a$ok) {
        return(div(class = "notice", a$msg))
      }
      t <- target()
      u <- var_meta()$unit
      yrs <- a$hist$annual$Year
      ci <- if (is.na(t$lower)) "Confidence interval not available" else
        sprintf("95%% CI %.1f to %.1f %s", t$lower, t$upper, u)
      div(
        class = "kpis",
        kpi(sprintf("%d-year return level", input$return_period),
          sprintf("%.1f %s", t$level, u), ci,
          primary = TRUE
        ),
        kpi("Years used", length(yrs),
          sprintf("%d to %d, at least %d%% valid days", min(yrs), max(yrs), input$min_cov)),
        kpi("Shape (ξ)", sprintf("%.2f", a$gev$shape), tail_label(a$gev$shape))
      )
    })

    output$rl_plot <- renderPlot(
      {
        a <- analysis()
        validate(need(a$ok, a$msg))
        t <- target()
        accent <- var_meta()$accent
        ggplot(a$curve, aes(period, level)) +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = accent, alpha = 0.16, na.rm = TRUE) +
          geom_line(colour = accent, linewidth = 1.1) +
          geom_point(data = a$emp, aes(period, value), colour = EVA_COLORS$text, size = 1.8, alpha = 0.85) +
          geom_vline(xintercept = input$return_period, linetype = "dashed", colour = EVA_COLORS$muted, linewidth = 0.4) +
          geom_point(data = t, aes(period, level), colour = accent, size = 3.5) +
          scale_x_log10(breaks = c(2, 5, 10, 20, 50, 100, 200, 500, 1000)) +
          coord_cartesian(xlim = c(1.4, 1000)) +
          labs(x = "Return period (years, log scale)", y = sprintf("Return level (%s)", var_meta()$unit)) +
          theme_eva()
      },
      bg = "transparent", res = 110,
      alt = "Return level curve with confidence band and observed annual maxima"
    )

    output$am_plot <- renderPlot(
      {
        a <- analysis()
        validate(need(a$ok, a$msg))
        am <- a$hist$annual
        accent <- var_meta()$accent
        ggplot(am, aes(Year, Value)) +
          geom_line(colour = accent, alpha = 0.45) +
          geom_point(colour = accent, size = 2) +
          geom_point(data = am[which.max(am$Value), ], shape = 1, size = 4.5, colour = EVA_COLORS$text) +
          scale_x_continuous(breaks = scales::pretty_breaks()) +
          labs(x = NULL, y = sprintf("Annual maximum (%s)", var_meta()$unit)) +
          theme_eva()
      },
      bg = "transparent", res = 110,
      alt = "Annual maximum of the selected variable by year, record year circled"
    )
  })
}
