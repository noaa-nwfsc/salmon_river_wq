# Read in and combine empirical data from site sensors

# NOTE: Data files should be .csv files with this format:
# First row: Site name in first cell
# Second row: Column names: Date, Time, Temperature, Water.depth (no extra text, no special symbols or parentheses)
# files stored in a subfolder called 'WQ202X'

# set current year
yr <- 2026


# Read all individual site files
files <- dir(paste0("all_data/SalmonEnvData/WQ", yr))
(files <- files[grep(".csv",files)])

alldata <- NULL
for(f in files){
  dat <- read.csv(paste0("all_data/SalmonEnvData/WQ", yr, "/", f), skip = 1)
  dat$Observe.date <- as.Date(dat$Date, origin = "1970-01-01", format = "%m/%d/%Y") # format date #%H:%M
  dat <- dat[dat$Observe.date > as.Date("1993-01-01"),] # removes some wonky dates at the front end
  nm <- read.csv(paste0("all_data/SalmonEnvData/WQ", yr, "/", f), header = F)[1,1] # get site name from file to append
  dat$Site.name <- nm
  alldata <- rbind(alldata, dat)
}
alldata <- alldata[!is.na(alldata$Temperature), c("Site.name", "Observe.date", "Temperature",  "Water.depth")]
summary(alldata); rm(dat)

## rename so sites match the already compiled dataset
#alldata$Site.name[alldata$Site.name == "Bear Valley/Elk Creek"] <- "Bear Valley Creek"
#alldata$Site.name[alldata$Site.name == "Big Creek (lower)/Rush Creek"] <- "Big Creek (lower)"
#alldata$Site.name[alldata$Site.name == "Taylor Ranch"] <- "Big Creek (lower)"
alldata$Site.name[alldata$Site.name == "South Fork Salmon"] <- "South Fork Salmon River"

# Read in the compiled dataset to date
obs_temps_hourly <- readr::read_csv("all_data/salmon_environmental_data.csv")

# Append any new data (this will not overwrite old data)
for(s in unique(obs_temps_hourly$Site.name)){
  maxdate <- max(obs_temps_hourly$Observe.date[obs_temps_hourly$Site.name == s])
  dat2append <- alldata[alldata$Observe.date > maxdate & alldata$Site.name == s,]
  dat2append <- dat2append[!is.na(dat2append$Temperature),]
  obs_temps_hourly <- rbind(obs_temps_hourly, dat2append)
}
summary(obs_temps_hourly)
readr::write_csv(obs_temps_hourly, "all_data/salmon_environmental_data.csv")
