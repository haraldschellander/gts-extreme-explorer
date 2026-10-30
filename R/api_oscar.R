# R/api_oscar.R
# Station metadata from the WMO OSCAR/Surface REST API.
# Search endpoint: <base>/search/station  (public, no token needed for reading)

library(httr2)

OSCAR_BASE <- "https://oscar.wmo.int/surface/rest/api"

oscar_request <- function(path, ...) {
  request(paste0(OSCAR_BASE, path)) |>
    req_user_agent(EVA_USER_AGENT) |>
    req_url_query(...) |>
    req_timeout(120) |>
    req_retry(max_tries = 3, backoff = ~2)
}

# Fetch all fixed land stations. The endpoint is paginated (max 50,000 per
# page); the loop works whether the API counts pages from 0 or from 1.
fetch_oscar_stations <- function(facility_type = "landFixed", items = 50000) {
  fetch_page <- function(page = NULL) {
    req <- oscar_request("/search/station", facilityType = facility_type, items = items)
    if (!is.null(page)) {
      req <- req_url_query(req, page = page)
    }
    req |> req_perform() |> resp_body_json(simplifyVector = FALSE)
  }

  first <- fetch_page()
  # Older API versions returned a bare array
  if (is.null(first$stationSearchResults)) {
    return(parse_oscar_stations(first))
  }

  results <- first$stationSearchResults
  n_pages <- first$pageCount %||% 1
  if (n_pages > 1) {
    first_page <- first$pageNumber %||% 0
    for (p in first_page + seq_len(n_pages - 1)) {
      results <- c(results, fetch_page(p)$stationSearchResults)
    }
  }
  parse_oscar_stations(results)
}

# Flatten the station records into a data frame. Keeps only stations with a
# WMO-style WIGOS id (0-20000-0-NNNNN): that id links to GTS/SYNOP reports
# and to NCEI's station list.
parse_oscar_stations <- function(results) {
  if (length(results) == 0) {
    stop("OSCAR returned no stations.")
  }

  chr <- function(field) {
    vapply(results, function(s) as.character(s[[field]] %||% NA_character_), character(1))
  }
  num <- function(field) {
    vapply(results, function(s) as.numeric(s[[field]] %||% NA_real_), numeric(1))
  }

  wigos <- vapply(
    results,
    function(s) {
      recs <- s$wigosStationIdentifiers %||% list()
      ids <- vapply(recs, function(r) as.character(r$wigosStationIdentifier %||% NA_character_), character(1))
      primary <- vapply(recs, function(r) isTRUE(r$primary), logical(1))
      if (length(ids) == 0) NA_character_ else if (any(primary)) ids[primary][1] else ids[1]
    },
    character(1)
  )

  is_wmo <- grepl("^0-20000-0-[0-9]{5}$", wigos)

  df <- data.frame(
    oscar_id = chr("id"),
    name = chr("name"),
    wigos_id = wigos,
    wmo_index = ifelse(is_wmo, substr(wigos, 11, 15), NA_character_),
    lat = num("latitude"),
    lon = num("longitude"),
    elev = num("elevation"),
    country = chr("territory"),
    region = chr("region"),
    status = chr("declaredStatus"),
    stringsAsFactors = FALSE
  )

  df[!is.na(df$wmo_index) & !is.na(df$lat) & !is.na(df$lon), , drop = FALSE]
}
