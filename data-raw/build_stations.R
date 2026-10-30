# Build the station table once (at deploy time or from a cron job) so the
# public app does not call OSCAR and NCEI on every start.
#
#   Rscript data-raw/build_stations.R
#
# Writes data/stations.rds, which get_station_metadata() prefers.

source("R/utils.R")
source("R/api_oscar.R")
source("R/api_ncei.R")
source("R/stations.R")

stations <- build_station_table()
dir.create("data", showWarnings = FALSE)
saveRDS(stations, "data/stations.rds", compress = "xz")
message(sprintf("Wrote %d stations to data/stations.rds", nrow(stations)))
