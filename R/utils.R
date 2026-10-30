# R/utils.R
# Shared constants, caching helpers and the dark ggplot theme.

library(httr2)
library(ggplot2)

# Identify yourself to the data providers (they ask for this on public services).
EVA_USER_AGENT <- "gts-extreme-explorer (R/Shiny; https://github.com/<your-org>/<your-repo>)"

EVA_COLORS <- list(
  bg      = "#0E1722",
  surface = "#152232",
  raised  = "#1C2D42",
  line    = "#2A3C54",
  text    = "#E6EDF5",
  muted   = "#8EA2B8"
)

# The accent colour follows the selected variable: warm for temperature,
# cool for precipitation.
EVA_VARIABLES <- list(
  tmax = list(label = "Daily maximum temperature", unit = "°C", accent = "#F2A25C"),
  precip = list(label = "Daily precipitation", unit = "mm", accent = "#5CB8FF")
)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Unit conversion (GSOD reports Fahrenheit and inches) -------------------
f_to_c <- function(f) (f - 32) * 5 / 9
in_to_mm <- function(x) x * 25.4

haversine_km <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  dlat <- (lat2 - lat1) * r
  dlon <- (lon2 - lon1) * r
  a <- sin(dlat / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin(dlon / 2)^2
  2 * 6371 * asin(pmin(1, sqrt(a)))
}

# ---- Disk cache -------------------------------------------------------------
# Set EVA_CACHE_DIR to a persistent volume when you deploy.
cache_dir <- function() {
  d <- Sys.getenv("EVA_CACHE_DIR", unset = tools::R_user_dir("gts-eva", "cache"))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

cache_read <- function(key, max_age_hours = Inf) {
  f <- file.path(cache_dir(), paste0(key, ".rds"))
  if (!file.exists(f)) {
    return(NULL)
  }
  age <- as.numeric(difftime(Sys.time(), file.mtime(f), units = "hours"))
  if (age > max_age_hours) {
    return(NULL)
  }
  tryCatch(readRDS(f), error = function(e) NULL)
}

cache_write <- function(key, value) {
  f <- file.path(cache_dir(), paste0(key, ".rds"))
  tmp <- tempfile(tmpdir = dirname(f), fileext = ".tmp")
  saveRDS(value, tmp)
  file.rename(tmp, f)
  invisible(value)
}

# ---- Plot theme -------------------------------------------------------------
theme_eva <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_line(colour = EVA_COLORS$line, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      text = element_text(colour = EVA_COLORS$text),
      axis.text = element_text(colour = EVA_COLORS$muted),
      axis.title = element_text(colour = EVA_COLORS$muted),
      plot.margin = margin(6, 10, 6, 6)
    )
}
