# R/utils.R

library(httr2)
library(jsonlite)

# 1. Get metadata for global stations to display on the map
get_station_metadata <- function() {
  data.frame(
    id = c("STN_001", "STN_002", "STN_003", "STN_004"),
    name = c(
      "Station Alpha (Alps)",
      "Station Beta (Paris)",
      "Station Gamma (New York)",
      "Station Delta (Tokyo)"
    ),
    lat = c(46.5, 48.8, 40.7, 35.6),
    lon = c(11.2, 2.3, -74.0, 139.6),
    stringsAsFactors = FALSE
  )
}

# 2. Fetch historical records for a given station ID
fetch_station_history <- function(station_id, variable = "tmax") {
  # Reproducible time series per station based on its ID string
  seed_val <- sum(utf8ToInt(station_id))
  set.seed(seed_val)

  years <- 1975:2025
  base_val <- if (variable == "tmax") 36 else 85
  vals <- rnorm(length(years), mean = base_val, sd = 4.2)

  data.frame(Year = years, Value = vals)
}
