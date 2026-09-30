# Thermal metrics computation script
# Generated script using GitHub Copilot GPT-5 mini 7/29/26 AHF

library(data.table)
library(lubridate)
library(dplyr)
library(zoo)

# Source metrics functions
fm_path <- 'salmon_river_wq/support_scripts/4_thermal_metrics_functions.R'
if(!file.exists(fm_path)) stop('functions file not found: ', fm_path)
source(fm_path)
cat('Loading functions...\n')

# Read empirical water quality data
qs_path <- 'salmon_river_wq/data/obs_temps.qs2'
if(!file.exists(qs_path)) stop('obs_temps.qs2 not found at ', qs_path)
obs_temps <- qs2::qs_read(qs_path)

# Convert to data.table
obs_dt <- data.table::as.data.table(obs_temps)
obs_dt[, Date := as.Date(Date)]

# compute per-site min/max dates then expand to daily grid per site
range_dt <- obs_dt[, .(minD = min(Date, na.rm = TRUE), maxD = max(Date, na.rm = TRUE)), by = SiteCode]
full_grid <- range_dt[, .(Date = seq(minD, maxD, by = 'day')), by = SiteCode]

# left join original measurements onto full grid (preserves full daily index)
setkey(obs_dt, SiteCode, Date)
obs_full <- merge(full_grid, obs_dt, by = c('SiteCode','Date'), all.x = TRUE)
rm(obs_dt, full_grid, range_dt)
cat('Loaded observed temperature data: nrows =', nrow(obs_full), '\n')

# Life stages and species ----
species <- "Chinook"
periodicity <- fread("salmon_river_wq/data/lifestage_periods.csv")
lhs <- unique(periodicity$LHS_Code)
cat('Life stages to process:', paste(lhs, collapse = ', '), '\n')

# Empirical dataset: select and clean
emp.dt <- obs_full[, .(Date, SiteCode, AvgDailyTemp)]
setorderv(emp.dt, c('SiteCode','Date'))

# Get range of years
year_range <- sort(unique(year(emp.dt$Date)))
cat('Range of years:', paste(range(year_range), collapse = '-'), '\n')

# set threads for data.table (use available physical cores minus 1 when possible)
available_cores <- tryCatch(parallel::detectCores(logical = FALSE), error = function(e) 1)
n_threads <- max(1, available_cores - 1)
cat('Setting data.table threads to', n_threads, '\n')
data.table::setDTthreads(n_threads)

# Compute metrics (fnc_compute_metrics defined in thermal_metrics_functions.R)
# ensure periodicity table and lhs are loaded
periodicity <- fread("salmon_river_wq/data/lifestage_periods.csv")
lhs <- unique(periodicity$LHS_Code)

start <- Sys.time()
emp.out.dt <- fnc_compute_metrics(
  emp.dt = emp.dt,
  periodicity = periodicity,
  LHS_Code = lhs,
  species = species,
  year_range = year_range,
  st_col = "AvgDailyTemp",
  site_col = "SiteCode",
  date_col = "Date",
  days_window = 7,
  sustain_count = 7
)

# ensure it's a data.table
emp.out.dt <- as.data.table(emp.out.dt)

# Normalize column types: dates -> Date, numeric columns -> numeric
date_cols <- intersect(c('date_max','date_min','exceed_1st','Start','End'), names(emp.out.dt))
for (c in date_cols) emp.out.dt[, (c) := as.Date(get(c))]
# coerce remaining non-id columns to numeric where appropriate
id_cols <- c('Species','LHS_Code','SiteCode','Year')
num_cols <- setdiff(names(emp.out.dt), c(id_cols, date_cols))
for (c in num_cols) {
  # skip if already numeric
  if (!is.numeric(emp.out.dt[[c]])) emp.out.dt[, (c) := as.numeric(get(c))]
}
# ensure Year is integer
if ('Year' %in% names(emp.out.dt)) emp.out.dt[, Year := as.integer(Year)]

# Filter to rows with <20% missing data
emp.out.filtered <- emp.out.dt[prop_missing < 0.2]

# Apply some other fixes
emp.out.filtered$exceed_1st_doy <- lubridate::yday(as.Date(emp.out.filtered$exceed_1st))
emp.out.filtered$mean_7d_min[is.infinite(emp.out.filtered$mean_7d_min)] <- NA
emp.out.filtered$mean_7d_max[is.infinite(emp.out.filtered$mean_7d_max)] <- NA

# Filter to only life stages / life history strategies occurring in tributaries (where empirical stream temp data are recorded)
vars <- c("AHT", "AST", "EIT", "PRFT", "PRST", "PRWT")
emp.out.filtered <- emp.out.filtered[emp.out.filtered$LHS_Code %in% vars,]

# ensure output directory exists and write out (use project-relative path)
out_dir <- 'salmon_river_wq/data'
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
out_file <- file.path(out_dir, 'thermal_metrics_empirical_filtered.csv')

fwrite(emp.out.filtered, file = out_file)
cat('Wrote output to', out_file, '\n')
end <- Sys.time()
cat('Elapsed: ', format(end - start), '\n')
#metrics <- fread('salmon_river_wq/data/thermal_metrics_empirical_filtered.csv')
