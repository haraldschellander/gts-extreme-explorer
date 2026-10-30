# R/api_ncei.R
# NOAA NCEI: station list (ISD history) and Global Summary of the Day (GSOD).
#   Station list : https://www.ncei.noaa.gov/pub/data/noaa/isd-history.csv
#   Daily files  : <GSOD_BASE>/<year>/<USAF><WBAN>.csv
# GSOD reports temperature in Fahrenheit and precipitation in inches.

library(httr2)

ISD_HISTORY_URL <- "https://www.ncei.noaa.gov/pub/data/noaa/isd-history.csv"
GSOD_BASE <- "https://www.ncei.noaa.gov/data/global-summary-of-the-day/access"
GSOD_FIRST_YEAR <- 1929L

fetch_isd_history <- function() {
  txt <- request(ISD_HISTORY_URL) |>
    req_user_agent(EVA_USER_AGENT) |>
    req_timeout(180) |>
    req_retry(max_tries = 3, backoff = ~2) |>
    req_perform() |>
    resp_body_string()

  df <- utils::read.csv(text = txt, colClasses = "character", check.names = FALSE, strip.white = TRUE)
  names(df)[1:11] <- c("usaf", "wban", "isd_name", "ctry", "state", "icao", "isd_lat", "isd_lon", "isd_elev", "begin", "end")

  df$isd_lat <- suppressWarnings(as.numeric(df$isd_lat))
  df$isd_lon <- suppressWarnings(as.numeric(df$isd_lon))
  df <- df[
    !is.na(df$isd_lat) & !is.na(df$isd_lon) &
      abs(df$isd_lat) <= 90 & abs(df$isd_lon) <= 180 &
      !(df$isd_lat == 0 & df$isd_lon == 0) &
      grepl("^[0-9]{6}$", df$usaf) & df$usaf != "999999",
    ,
    drop = FALSE
  ]

  data.frame(
    wmo_index = substr(df$usaf, 1, 5),
    usaf = df$usaf,
    wban = df$wban,
    station_id = paste0(df$usaf, df$wban),
    isd_lat = df$isd_lat,
    isd_lon = df$isd_lon,
    begin_year = suppressWarnings(as.integer(substr(df$begin, 1, 4))),
    end_year = suppressWarnings(as.integer(substr(df$end, 1, 4))),
    stringsAsFactors = FALSE
  )
}

# ---- GSOD daily data --------------------------------------------------------

gsod_request <- function(station_id, year) {
  request(sprintf("%s/%d/%s.csv", GSOD_BASE, year, station_id)) |>
    req_user_agent(EVA_USER_AGENT) |>
    req_timeout(30) |>
    req_error(is_error = function(resp) FALSE) # a 404 just means "no file that year"
}

# Parse one GSOD yearly CSV into date / tmax (°C) / precip (mm).
parse_gsod_csv <- function(txt) {
  df <- utils::read.csv(text = txt, colClasses = "character", na.strings = "", strip.white = TRUE)
  col <- function(name) if (name %in% names(df)) df[[name]] else rep(NA_character_, nrow(df))

  tmax <- suppressWarnings(as.numeric(col("MAX")))
  tmax[!is.na(tmax) & tmax >= 9999] <- NA # 9999.9 = missing

  prcp <- suppressWarnings(as.numeric(col("PRCP")))
  prcp[!is.na(prcp) & prcp >= 99.99] <- NA # 99.99 = missing
  flag <- col("PRCP_ATTRIBUTES")
  # H and I mean the daily total is known to be incomplete or unreported
  prcp[flag %in% c("H", "I")] <- NA

  data.frame(
    date = as.Date(col("DATE")),
    tmax = f_to_c(tmax),
    precip = in_to_mm(prcp),
    stringsAsFactors = FALSE
  )
}

# Download all yearly files for a station (in parallel, in chunks so the UI
# can show progress). Returns a data frame of daily values.
fetch_gsod_daily <- function(station_id, years, on_progress = NULL) {
  chunks <- split(years, ceiling(seq_along(years) / 12))
  out <- list()

  for (i in seq_along(chunks)) {
    yrs <- chunks[[i]]
    reqs <- lapply(yrs, function(y) gsod_request(station_id, y))
    resps <- req_perform_parallel(reqs, on_error = "continue", progress = FALSE, max_active = 8)

    for (j in seq_along(resps)) {
      r <- resps[[j]]
      if (!inherits(r, "httr2_response")) {
        # network error: retry once, sequentially
        r <- tryCatch(req_perform(reqs[[j]]), error = function(e) NULL)
      }
      if (inherits(r, "httr2_response") && resp_status(r) == 200) {
        out[[length(out) + 1]] <- parse_gsod_csv(resp_body_string(r))
      }
    }
    if (!is.null(on_progress)) on_progress(i / length(chunks))
  }

  if (length(out) == 0) {
    stop("NCEI has no daily summaries for this station.")
  }
  do.call(rbind, out)
}

# Daily data for a station, cached on disk for 24 hours.
load_gsod_daily <- function(station, on_progress = NULL) {
  key <- paste0("gsod_", station$id)
  hit <- cache_read(key, max_age_hours = 24)
  if (!is.null(hit)) {
    return(hit)
  }

  this_year <- as.integer(format(Sys.Date(), "%Y"))
  years <- seq(
    max(GSOD_FIRST_YEAR, station$begin_year, na.rm = TRUE),
    min(this_year, station$end_year, na.rm = TRUE)
  )
  daily <- fetch_gsod_daily(station$id, years, on_progress)
  cache_write(key, daily)
  daily
}

# One value per year: the largest daily value, kept only for years with
# enough valid days. The running year is dropped because it is incomplete.
annual_maxima <- function(date, value, min_coverage = 0.8,
                          current_year = as.integer(format(Sys.Date(), "%Y"))) {
  keep <- !is.na(date)
  date <- date[keep]
  value <- value[keep]
  yr <- as.integer(format(date, "%Y"))

  rows <- lapply(split(seq_along(yr), yr), function(ix) {
    y <- yr[ix[1]]
    n_days <- if ((y %% 4 == 0 && y %% 100 != 0) || y %% 400 == 0) 366 else 365
    v <- value[ix]
    n_valid <- sum(!is.na(v))
    if (y < current_year && n_valid > 0 && n_valid / n_days >= min_coverage) {
      data.frame(Year = y, Value = max(v, na.rm = TRUE), n_valid = n_valid)
    }
  })

  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- data.frame(Year = integer(), Value = numeric(), n_valid = integer())
  }
  rownames(out) <- NULL
  out
}

# Main entry point used by the EVA module.
fetch_station_history <- function(station, variable = c("tmax", "precip"),
                                  min_coverage = 0.8, on_progress = NULL) {
  variable <- match.arg(variable)
  daily <- load_gsod_daily(station, on_progress)
  list(
    annual = annual_maxima(daily$date, daily[[variable]], min_coverage),
    n_daily = sum(!is.na(daily[[variable]])),
    variable = variable
  )
}
