#' Process raw data
#'
#' Processes the raw electricity data used in GEFCom2017. Processing involves
#'
#' * Loading the raw excel files into data frames.
#' * Adding holiday and calendar variables.
#' * Removing daylight saving time hours.
#' * In leap years, shifting day of year back by one for all dates after February 28. This ensures each year has values in the DoY column.
#'
#' Saves a gefcom data_frame containing raw data and calendar variables.
#'
#' Author: Cameron Roach
rm(list = ls())

library(tidyverse)
library(readxl)

root_dir <- system.file("extdata", package = "gefcom2017data")
load_zones <- c("ME", "NH", "VT", "CT", "RI", "SEMASS", "WCMASS", "NEMASSBOST")

# load raw data and then cache gefcom data frame
gefcom <- NULL
files <- list.files(file.path(root_dir, "smd"))

for (iF in files) {
  cat("Reading file", iF, "...\n")
  file_name <- file.path(root_dir, "smd", iF)

  for (iS in load_zones) {
    gefcom <- readxl::read_excel(file_name, sheet = iS) %>%
      dplyr::rename_all(tolower) %>%
      dplyr::mutate(zone = iS,
                    date = as.Date(date)) %>%
      dplyr::select(date, hour, zone, demand, drybulb, dewpnt) %>%
      dplyr::filter(!is.na(demand)) %>%  # for trailing blank rows
      dplyr::bind_rows(gefcom)
  }
}

# Add holidays and calendar variables
holidays <- read.csv(file.path(root_dir, "holidays/holidays.csv"),
                     stringsAsFactors = FALSE) %>%
  dplyr::rename_all(tolower) %>%
  dplyr::rename(holiday_name = holiday) %>%
  dplyr::mutate(date = lubridate::mdy(date))

gefcom <- gefcom %>%
  dplyr::left_join(holidays, by = "date") %>%
  dplyr::mutate(
    holiday = dplyr::if_else(is.na(holiday_name), FALSE, TRUE),
    ts = lubridate::ymd_h(paste(date, hour - 1)),
    year = lubridate::year(ts),
    month = factor(lubridate::month(ts, label = TRUE), ordered = FALSE),
    day_of_week = factor(lubridate::wday(ts, label = TRUE), ordered = FALSE),
    day_of_year = lubridate::yday(ts),
    weekend = dplyr::if_else(day_of_week %in% c("Sat", "Sun"), TRUE, FALSE)
  )

# Remove DST hours
dst_times <- read.csv(file.path(root_dir, "dst_ts.csv")) %>%
  dplyr::mutate(dst_start = lubridate::ymd_hms(dst_start),
                dst_end = lubridate::ymd_hms(dst_end))

gefcom <- gefcom %>%
  dplyr::filter(!(ts %in% dst_times$dst_start)) %>%
  dplyr::filter(!(ts %in% dst_times$dst_end))

# Shift day of year for leap years. Feb 29 has day_of_year == 60
gefcom <- gefcom %>%
  dplyr::mutate(
    day_of_year = dplyr::if_else(
      lubridate::leap_year(year) & day_of_year >= 60, day_of_year - 1,
      day_of_year)
  )

# Create aggregated zones
mass <- gefcom %>%
  dplyr::filter(zone %in% c("SEMASS", "WCMASS", "NEMASSBOST")) %>%
  dplyr::group_by(date, hour, holiday_name, holiday, ts, hour, year, month,
                  day_of_week, day_of_year, weekend) %>%
  dplyr::summarise(demand = sum(demand),
                   drybulb = mean(drybulb),
                   dewpnt = mean(dewpnt)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(zone = "MASS")

total <- gefcom %>%
  dplyr::group_by(date, hour, holiday_name, holiday, ts, hour, year, month,
                  day_of_week, day_of_year, weekend) %>%
  dplyr::summarise(demand = sum(demand),
                   drybulb = mean(drybulb),
                   dewpnt = mean(dewpnt)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(zone = "TOTAL")

gefcom <- dplyr::bind_rows(gefcom, mass, total)

# Add trend for each zone
gefcom <- gefcom %>%
  dplyr::group_by(zone) %>%
  dplyr::mutate(trend = as.numeric(ts - min(ts))/3600) %>%
  dplyr::ungroup()

# Reorder columns
gefcom <- gefcom %>%
  dplyr::select(ts, zone, demand, drybulb, dewpnt, date, year, month, hour,
                day_of_week, day_of_year, weekend, holiday_name, holiday,
                trend) %>%
  dplyr::arrange(zone, ts)

# Save gefcom data frame
usethis::use_data(gefcom, overwrite = TRUE)
