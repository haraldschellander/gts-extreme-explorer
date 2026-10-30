# R/stations.R
# Builds the station table for the map by joining OSCAR (who and where)
# with NCEI's station list (which stations have daily data, and the file id).
#
# Join key: the 5-digit WMO index. In ISD/GSOD the 6-digit USAF number starts
# with the WMO index for stations that report on the GTS. A match is only
# accepted if the two coordinate pairs are within `max_distance_km`.

build_station_table <- function(min_record_years = 10, max_distance_km = 30) {
  cli::cli_alert_info("Getting OSCAR stations")
  oscar <- fetch_oscar_stations()
  cli::cli_alert_success("Got {nrow(oscar)} OSCAR stations.")

  cli::cli_alert_info("Getting ISD stations")
  isd <- fetch_isd_history()
  cli::cli_alert_success("Got {nrow(isd)} ISD stations")

  m <- merge(oscar, isd, by = "wmo_index")
  m$dist_km <- haversine_km(m$lat, m$lon, m$isd_lat, m$isd_lon)
  m <- m[!is.na(m$dist_km) & m$dist_km <= max_distance_km, , drop = FALSE]

  m$record_years <- m$end_year - m$begin_year + 1L
  m <- m[
    !is.na(m$record_years) & m$record_years >= min_record_years,
    ,
    drop = FALSE
  ]

  # One row per OSCAR station (longest record wins), one per GSOD file
  m <- m[order(m$oscar_id, -m$record_years, m$dist_km), ]
  m <- m[!duplicated(m$oscar_id), ]
  m <- m[!duplicated(m$station_id), ]

  out <- data.frame(
    id = m$station_id,
    wmo_index = m$wmo_index,
    wigos_id = m$wigos_id,
    name = ifelse(
      is.na(m$name),
      m$wmo_index,
      tools::toTitleCase(tolower(m$name))
    ),
    country = m$country,
    region = m$region,
    lat = m$lat,
    lon = m$lon,
    elev = m$elev,
    usaf = m$usaf,
    wban = m$wban,
    begin_year = m$begin_year,
    end_year = m$end_year,
    record_years = m$record_years,
    status = m$status,
    stringsAsFactors = FALSE
  )
  out[order(out$name), ]
}

# Order of preference:
#   1. data/stations.rds   (built at deploy time, see data-raw/build_stations.R)
#   2. a fresh disk cache  (< max_age_days old)
#   3. a live build from OSCAR + NCEI (then cached)
#   4. a stale disk cache
STATIONS_SNAPSHOT <- "data/stations.rds"

get_station_metadata <- function(refresh = FALSE, max_age_days = 7) {
  if (!refresh && file.exists(STATIONS_SNAPSHOT)) {
    return(readRDS(STATIONS_SNAPSHOT))
  }
  if (!refresh) {
    fresh <- cache_read("stations", max_age_hours = max_age_days * 24)
    if (!is.null(fresh)) {
      return(fresh)
    }
  }

  built <- tryCatch(build_station_table(), error = function(e) {
    message("Station build failed: ", conditionMessage(e))
    NULL
  })
  if (!is.null(built) && nrow(built) > 0) {
    cache_write("stations", built)
    return(built)
  }

  stale <- cache_read("stations")
  if (!is.null(stale)) {
    return(stale)
  }
  stop(
    "Could not load station metadata from OSCAR/NCEI and no cached copy exists."
  )
}
