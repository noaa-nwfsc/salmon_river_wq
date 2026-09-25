# Prepare data for app

library(dplyr)
library(tidyverse)
library(qs2)
library(sf)

# Read in spatial data ----
streams_lg <- sf::st_read("all_data/shp/Rivers_SOgt5_prj.shp") |>
  sf::st_transform('+proj=longlat +datum=WGS84') |>
  sf::st_zm()

streams <- sf::st_read("all_data/shp/SalmonSnake_streams_h12.shp")|>
  sf::st_transform('+proj=longlat +datum=WGS84') |>
  sf::st_zm()

watershed <- sf::st_read("all_data/shp/Salmon_boundary_prj.shp")|>
  sf::st_transform('+proj=longlat +datum=WGS84') |>
  sf::st_zm()

WBD <- sf::st_read("all_data/shp/WBD_SalmonRiver.shp")|>
  sf::st_transform('+proj=longlat +datum=WGS84') |>
  sf::st_zm()

wbd <- as.data.frame(WBD[, c("HUC_8","HUC_10", "HUC_12")]); wbd <- wbd[,which(!colnames(wbd) %in% "geometry")]
wbd$HUC_8 <- as.numeric(wbd$HUC_8); wbd$HUC_10 <- as.numeric(wbd$HUC_10); wbd$HUC_12 <- as.numeric(wbd$HUC_12)
streams <- dplyr::left_join(streams, wbd, by = "HUC_12")

# Read in sites data ----
sites <- read.csv("all_data/IdahoSites.csv")
sites <- sites[, c("COMID", "Code", "Lat", "Lon", "Stream", "MPG", "Location")] 
colnames(sites) <- c("COMID", "SiteCode", "Latitude", "Longitude", "Stream_Name", "MPG", "Location")
# Add Stream order, Pathlength, River_km, HUCs
streamsdf <- as.data.frame(streams[,c("COMID", "StreamOrde", "Pathlength", "TotDASqKM")]); streamsdf <- streamsdf[,-5]
sites <- dplyr::left_join(sites, streamsdf, by = "COMID")
m <- streams[streams$GNIS_NAME %in% "Salmon River",]
cid_mouth <- m$COMID[which.min(m$Pathlength)] # 24938538
pl_mouth <- m$Pathlength[which.min(m$Pathlength)] #811.894
sites$River_km <- round(sites$Pathlength - pl_mouth, 3)
sites <- dplyr::left_join(sites, streams[,c("COMID", "HUC_10", "HUC_12")], by = "COMID")
sites <- as.data.frame(sites); sites <- sites[,-which(colnames(sites) %in% "geometry")]
rownames(sites) <- NULL
sites$Latitude <- round(sites$Latitude, 4)
sites$Longitude <- round(sites$Longitude, 3)
sites$TotDASqKM <- round(sites$TotDASqKM, 3)
sites <- sites[order(sites$Latitude),]
xwalk <- read.csv("all_data/COMID_to_HUC12.csv")


# Read in and prepare empirical data ----
obs_temps_hourly <- readr::read_csv("all_data/salmon_environmental_data.csv")
obs_temps_hourly <- merge(obs_temps_hourly, sites[, c("COMID", "SiteCode", "Stream_Name")], by.x = "Site.name", by.y = "Stream_Name", all.x = T)
obs_temps_hourly$SiteCode [obs_temps_hourly$Site.name == "Sawtooth Hatchery"] <- "STH"
colnames(obs_temps_hourly)[2] <- "Date"
site_codes <- sort(unique(obs_temps_hourly$SiteCode))
orgs <- "NOAA"
cids <- sort(unique(obs_temps_hourly$COMID))

# Aggregate to daily Min/Mean/Max Stream temperature:
  obs_temps <- obs_temps_hourly %>%
    group_by(Date, SiteCode) %>%
    summarise(
      across(
        .cols = Temperature, 
        .fns = list(
          Avg = ~mean(.x, na.rm = T),
          Min = ~min(.x, na.rm = T),
          Max = ~max(.x, na.rm = T)
        ),
       .names = "{.fn}DailyTemp"
      ),
      .groups = "drop"
    )
  
  # Aggregate to daily Min/Mean/Max Depth:
  obs_depths <- obs_temps_hourly %>%
    group_by(Date, SiteCode) %>%
    summarise(
      across(
        .cols = Water.depth, 
        .fns = list(
          Avg = ~mean(.x, na.rm = T),
          Min = ~min(.x, na.rm = T),
          Max = ~max(.x, na.rm = T)
        ),
        .names = "{.fn}WaterDepth"
      ),
      .groups = "drop"
    )
  
  # Add attributes where possible
  obs_temps <- dplyr::left_join(obs_temps, sites[, c("COMID", "SiteCode", "Latitude", "Longitude", "River_km", "Stream_Name")], by = "SiteCode")
  obs_temps <- obs_temps[,c("Date", "SiteCode", "COMID", "AvgDailyTemp", "MinDailyTemp", "MaxDailyTemp", "Latitude", "Longitude", "River_km", "Stream_Name")] 
  obs_temps <- dplyr::left_join(obs_temps, obs_depths, by = c("SiteCode", "Date"))
  obs_temps$year <- lubridate::year(obs_temps$Date)
  
  # Add details for Sawtooth Hatchery data which were not in sites table
  obs_temps$Stream_Name[obs_temps$SiteCode == "STH"] <- "Sawtooth Hatchery"
  obs_temps$Latitude[obs_temps$SiteCode == "STH"] <- 44.148
  obs_temps$Longitude[obs_temps$SiteCode == "STH"] <- -114.887
   
  # Save
  data.table::fwrite(obs_temps, "data/obs_temps.csv")
  
# Quantify amount of data per year/site ----
st_wide <- obs_temps %>% tidyr::pivot_wider(id_cols = "Date", names_from = "SiteCode", values_from = "AvgDailyTemp", values_fill = NA)
st_wide <- as.data.frame(st_wide)
dat <- st_wide[,-1]
dat[!is.na(dat)] <- 1
dat <- cbind("Date" = st_wide$Date, "year" = lubridate::year(st_wide$Date), dat)

data_year_site <- NULL
for(y in min(dat$year):max(dat$year)){
  data_year_site <- rbind(data_year_site, cbind(y, t(colSums(dat[dat$year == y,3:ncol(dat)], na.rm = T))))
}
data_ys1 <- data_year_site
data_ys1[data_ys1 > 7] <- 1 #years with more than a week of data
data_ys1[,1] <- data_year_site[,1]
data_ys2 <- t(data_year_site[,-1]); colnames(data_ys2) <- seq(min(dat$year), max(dat$year))
data_ys2 <- cbind.data.frame("Site" = row.names(data_ys2), data_ys2)

# remove sites with no data
data_ys2$hasdata <- apply(data_ys2[,-1], 1, sum, na.rm = T)
data_ys2 <- data_ys2[data_ys2$hasdata > 0,]
row.names(data_ys2) <- NULL
data_year_site <- data_ys2
colnames(data_year_site)[1] <- "SiteCode"
rm(data_ys1, data_ys2)

# Add 'hasdata' column to sites
sites <- dplyr::left_join(sites, data_year_site, by = "SiteCode")
# reduce set to what's in the obs dataset
#sites <- sites[sites$SiteCode %in% unique(obs_temps$SiteCode),] 

# Save data ----
save(streams, streams_lg, watershed, WBD, file = "data/salmon_geo.RData")
save(sites, xwalk, data_year_site, site_codes, orgs, cids, cid_mouth, file = "data/site_info.RData")

# Separate Marsh and Cape Horn Creek:
obs_temps <- as.data.frame(obs_temps)
obs_temps$Latitude[obs_temps$SiteCode == "CHC"] <- 44.3953
obs_temps$Longitude[obs_temps$SiteCode == "CHC"] <- -115.170

daily_data <- data.table::fread("data/salmon_environmental_data.csv")

qs2::qs_save(obs_temps, file = "data/obs_temps.qs2")
qs2::qs_save(daily_data, file = "data/daily_data.qs2")
