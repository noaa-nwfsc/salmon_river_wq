# Load data --------------------------------------------------------------------
library(qs2)
library(data.table)

# geospatial data
load("data/salmon_geo.RData")

# sites info
load("data/site_info.RData")

# stream temp data
obs_temps <- qs2::qs_read("data/obs_temps.qs2")
daily_data <- qs2::qs_read("data/daily_data.qs2")

# thermal metrics
lifestages <- read.csv("data/lifestage_periods_v2.csv")
metrics.obs <- read.csv("data/thermal_metrics_empirical_filtered.csv")
metrics.obs <- dplyr::left_join(metrics.obs, sites[, c("SiteCode", "River_km")], by = "SiteCode")


