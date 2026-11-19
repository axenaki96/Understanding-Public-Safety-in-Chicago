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

# ---- Load data
options(timeout = 1200)   

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
      Sys.sleep(10) 
    }
    if (!ok) stop("Could not download ", yr, ". Run the script again later.")
    d <- read_csv(tmp, show_col_types = FALSE,
                  col_types = cols(.default = col_character()))
    saveRDS(d, file)
    unlink(tmp)
  }
  readRDS(file)
}

crimes_raw <- map(start_year:end_year, load_year) |> list_rbind()

# ---- 
list.files("data/raw")
nrow(crimes_raw)

# ---- Clean ----
crimes <- crimes_raw |>
  clean_names() |>
  mutate(
    date = ymd_hms(date),
    year = year(date),
    month = month(date, label = TRUE),
    hour = hour(date),
    weekday = wday(date, label = TRUE, week_start = 1),
    arrest  = arrest == "true",
    domestic = domestic == "true",
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) |>
  filter(!is.na(date))

# ---- Check the data ----
nrow(crimes)
count(crimes, year)
count(crimes, primary_type, sort = TRUE)


#--- data cleaning
n_init <- nrow(crimes_raw)

missing_check <- crimes_raw |>
  summarise(across(everything(), list(
    na = ~ sum(is.na(.x)),
    empty = ~ sum(str_trim(.x) == "", na.rm = TRUE),
    zero = ~ sum(str_trim(.x) == "0", na.rm = TRUE),
    null = ~ sum(str_to_upper(str_trim(.x)) %in% c("NULL", "NA", "N/A", "NONE"))
  ), .names = "{.col}__{.fn}")) |>
  pivot_longer(everything(),
               names_to = c("column", "type"), names_sep = "__",
               values_to = "count") |>
  pivot_wider(names_from = type, values_from = count) |>
  arrange(desc(total_problem))

print(missing_check, n = Inf)


# Q1a How does time vary by hour, day, season, year?
# Q1b Which communities are affected the most 
# Q1a x Q1b
# Q1c which features best explain how much crime happens?