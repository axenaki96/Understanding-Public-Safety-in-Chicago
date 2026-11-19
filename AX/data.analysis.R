# ============================================================
# AX analysis: When and where does crime occur in Chicago?
# ============================================================

# ---- Packages ----
library(tidyverse)
library(lubridate)
library(stats)

# ---- Settings ----
start_year <- 2015
end_year   <- 2025

data_dir <- "data/raw"
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Load data (one year at a time, saved after first download) ----
options(timeout = 1200)   # allow up to 20 minutes per download

load_year <- function(yr, tries = 3) {
  file <- file.path(data_dir, paste0("chicago_crimes_", yr, ".rds"))
  if (!file.exists(file)) {
    url <- paste0(
      "https://data.cityofchicago.org/resource/ijzp-q8t2.csv?",
      "$where=year=", yr, "&$limit=1000000"
    )
    tmp <- tempfile(fileext = ".csv")
    for (i in 1:tries) {
      message("Downloading ", yr, " (try ", i, ")...")
      ok <- tryCatch({
        download.file(url, tmp, mode = "wb", quiet = TRUE)
        TRUE
      }, error = function(e) FALSE, warning = function(w) FALSE)
      if (ok) break
      Sys.sleep(10)   # wait 10 seconds before trying again
    }
    if (!ok) stop("Could not download ", yr, ". Run the script again later.")
    d <- read_csv(tmp, show_col_types = FALSE,
                  col_types = cols(.default = col_character()))
    saveRDS(d, file)
    unlink(tmp)
  }
  readRDS(file)
}

# ---- Clean ----
crimes <- crimes_raw |>
  clean_names() |>
  mutate(
    date      = ymd_hms(date),
    year      = year(date),
    month     = month(date, label = TRUE),
    hour      = hour(date),
    weekday   = wday(date, label = TRUE, week_start = 1),
    arrest    = arrest == "true",
    domestic  = domestic == "true",
    latitude  = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) |>
  filter(!is.na(date))

# ---- Check the data ----
nrow(crimes)
count(crimes, year)
count(crimes, primary_type, sort = TRUE)

# ---- Analysis ----

