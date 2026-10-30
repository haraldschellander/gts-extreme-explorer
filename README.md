# Extreme Value Analysis of GTS Observations

An open-source R/Shiny web application for on-demand Extreme Value Analysis (EVA) using global weather station data.

Pick a station on the map, choose a variable (daily maximum temperature or daily precipitation) and get return levels from a GEV fit to the annual maxima.

## Data sources

- **Station metadata:** WMO OSCAR/Surface REST API (`https://oscar.wmo.int/surface/rest/api`): names, coordinates and WIGOS/WMO identifiers of fixed land stations.
- **Observations:** NOAA NCEI Global Summary of the Day (GSOD), derived from GTS exchanges. Files are read directly from the NCEI web folder.
- OSCAR stations are matched to GSOD stations through the 5-digit WMO index, and only accepted when the coordinates agree within 30 km.

## Run locally

```r
install.packages(c("shiny", "bslib", "leaflet", "httr2", "ggplot2", "extRemes", "scales"))
shiny::runApp()
```

The first start downloads the station list (a few MB) and caches it. For a public deployment, build the station table once instead:

```sh
Rscript data-raw/build_stations.R   # writes data/stations.rds
```

Set `EVA_CACHE_DIR` to a persistent directory so downloaded daily data are reused across sessions and NCEI is not hit repeatedly: `Sys.setenv(EVA_CACHE_DIR = "data")`.

## Layout

```
app.R                      UI shell and server wiring
R/utils.R                  constants, cache, plot theme
R/api_oscar.R              OSCAR fetcher
R/api_ncei.R               ISD station list, GSOD download, annual maxima
R/stations.R               OSCAR + NCEI join, get_station_metadata()
R/eva_stats.R              GEV fit, return levels
R/modules/map_module.R     leaflet map, search, filter
R/modules/eva_module.R     controls, KPIs, plots
www/eva.css                dark theme details
data-raw/build_stations.R  deploy-time station build
```

## Caveats

GSOD daily maxima may not align with the calendar day, and daily precipitation totals can be incomplete. Confidence intervals are normal approximations. Treat results for short or gappy records with care.

## License

GPL-3. See `LICENSE`.
