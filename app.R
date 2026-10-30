# ==========================================
# GTS Extreme Value Explorer - Main Application
# ==========================================

library(shiny)
library(bslib)
library(cli)

source("R/utils.R")
source("R/api_oscar.R")
source("R/api_ncei.R")
source("R/stations.R")
source("R/eva_stats.R")
source("R/modules/map_module.R")
source("R/modules/eva_module.R")

# Station table: data/stations.rds if present, otherwise built from OSCAR + NCEI
cli::cli_alert_info("Getting station meta data")
stations_df <- get_station_metadata()
if (nrow(stations_df > 0)) {
  cli::cli_alert_success("Got {nrow(stations_df)} stations.")
}


# Fonts are bundled locally (local = TRUE) so visitors' browsers do not
# contact Google, which matters for a public app under the GDPR.
eva_theme <- bs_theme(
  version = 5,
  bg = EVA_COLORS$bg,
  fg = EVA_COLORS$text,
  primary = "#5CB8FF",
  secondary = EVA_COLORS$muted,
  base_font = font_google("IBM Plex Sans", local = TRUE),
  heading_font = font_google("IBM Plex Sans", local = TRUE),
  "card-bg" = EVA_COLORS$surface,
  "card-border-color" = EVA_COLORS$line,
  "border-color" = EVA_COLORS$line,
  "font-size-base" = "0.95rem"
)

ui <- page_sidebar(
  theme = eva_theme,
  title = "GTS Extreme Value Explorer",
  fillable = TRUE,
  tags$head(tags$link(rel = "stylesheet", href = "eva.css")),
  sidebar = sidebar(
    width = 290,
    eva_ui("eva_mod"),
    div(
      class = "sidebar-foot",
      p(
        "Station metadata: WMO OSCAR/Surface. Observations: NOAA NCEI Global Summary of the Day."
      )
    )
  ),
  layout_columns(
    col_widths = breakpoints(sm = 12, lg = c(7, 5)),
    card(
      full_screen = TRUE,
      class = "map-card",
      min_height = 420,
      card_body(padding = 0, map_ui("map_mod"))
    ),
    card(
      class = "results-card",
      card_body(eva_results_ui("eva_mod"))
    )
  )
)

server <- function(input, output, session) {
  selected_station <- map_server("map_mod", stations_df)
  eva_server("eva_mod", selected_station)
}

shinyApp(ui = ui, server = server)
