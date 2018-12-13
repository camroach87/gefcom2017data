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

root_dir <- system.file("extdata", package = "gefcom2017data")
load_zones <- c("ME", "NH", "VT", "CT", "RI", "SEMASS", "WCMASS", "NEMASSBOST")

# load raw data and then cache gefcom data frame
gefcom <- NULL
files <- list.files(file.path(root_dir, "smd"))

for (iF in files) {
  cat("Reading file", iF, "...\n")
  file_name <- file.path(root_dir, "smd", iF)

  for (iS in load_zones) {
    tmp <- readxl::read_excel(file_name, sheet = iS) %>%
      dplyr::mutate(Zone = iS,
                    Date = as.Date(Date)) %>%
      dplyr::select(Date, Hour, Zone, Demand = DEMAND, DryBulb, DewPnt) %>%
      dplyr::filter(!is.na(Demand)) # some spreadsheet tabs have trailing blank rows

    gefcom <- dplyr::bind_rows(gefcom, tmp)
  }
}

# Add holidays and calendar variables
holidays <- read.csv(file.path(root_dir, "holidays/holidays.csv"),
                     stringsAsFactors = FALSE) %>%
  dplyr::mutate(Date = lubridate::mdy(Date))

gefcom <- gefcom %>%
  dplyr::left_join(holidays, by = "Date") %>%
  dplyr::mutate(Holiday = dplyr::if_else(is.na(Holiday), "NH", Holiday),
                Holiday_flag = dplyr::if_else(Holiday == "NH", FALSE, TRUE),
                ts = lubridate::ymd_h(paste(Date, Hour - 1)),
                Period = factor(Hour, levels = 1:24, ordered = FALSE),
                Year = lubridate::year(ts),
                Month = factor(lubridate::month(ts, label = TRUE),
                               ordered = FALSE),
                DoW = factor(lubridate::wday(ts, label = TRUE),
                             ordered = FALSE),
                DoY = lubridate::yday(ts),
                Weekend = dplyr::if_else(DoW %in% c("Sat", "Sun"), TRUE,
                                         FALSE))


# Remove DST hours
dst_times <- read.csv(file.path(root_dir, "dst_ts.csv")) %>%
  dplyr::mutate(dst_start = lubridate::ymd_hms(dst_start),
                dst_end = lubridate::ymd_hms(dst_end))

gefcom <- gefcom %>%
  dplyr::filter(!(ts %in% dst_times$dst_start)) %>%
  dplyr::filter(!(ts %in% dst_times$dst_end))

# Shift DoY for leap years. Feb 29 has DoY == 60
gefcom <- gefcom %>%
  dplyr::mutate(DoY = dplyr::if_else(lubridate::leap_year(Year) & DoY >= 60,
                                     DoY - 1, DoY))

# Create aggregated zones
mass <- gefcom %>%
  dplyr::filter(Zone %in% c("SEMASS", "WCMASS", "NEMASSBOST")) %>%
  dplyr::group_by(Date, Hour, Holiday, Holiday_flag, ts, Period, Year, Month,
                  DoW, DoY, Weekend) %>%
  dplyr::summarise(Demand = sum(Demand),
                   DryBulb = mean(DryBulb),
                   DewPnt = mean(DewPnt)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Zone = "MASS")

total <- gefcom %>%
  dplyr::group_by(Date, Hour, Holiday, Holiday_flag, ts, Period, Year, Month,
                  DoW, DoY, Weekend) %>%
  dplyr::summarise(Demand = sum(Demand),
                   DryBulb = mean(DryBulb),
                   DewPnt = mean(DewPnt)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Zone = "TOTAL")

gefcom <- dplyr::bind_rows(gefcom, mass, total)

# Save gefcom data frame
devtools::use_data(gefcom, overwrite = T)
