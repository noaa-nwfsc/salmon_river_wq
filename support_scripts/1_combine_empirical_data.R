# Read in and combine empirical data from site sensors


# Read all individual site files
files <- dir("all_data/SalmonEnvData/WQ2026")
files <- files[grep(".csv",files)]

alldata <- NULL
for(f in files){
  dat <- read.csv(paste0("all_data/SalmonEnvData/", f))
  dat$Observe.date <- as.POSIXlt(dat$Observe.date, origin = "1970-01-01", format = "%m/%d/%Y %H:%M") # format date
  dat <- dat[dat$Observe.date > as.Date("1993-01-01"),] # removes some wonky dates at the front end
  alldata <- rbind(alldata, dat)
}
summary(alldata)
# rename so sites match the already compiled dataset
alldata <- alldata[!is.na(alldata$Temperature),]
alldata$Site.name[alldata$Site.name == "Bear Valley/Elk Creek"] <- "Bear Valley Creek"
alldata$Site.name[alldata$Site.name == "Big Creek (lower)/Rush Creek"] <- "Big Creek (lower)"
alldata$Site.name[alldata$Site.name == "Taylor Ranch"] <- "Big Creek (lower)"
alldata$Site.name[alldata$Site.name == "South Fork Salmon"] <- "South Fork Salmon River"

# Read in the compiled dataset to date
obs_temps_hourly <- readr::read_csv("all_data/salmon_environmental_data.csv")

# Append any new data (this will not overwrite old data)
for(s in unique(obs_temps_hourly$Site.name)){
  maxdate <- max(obs_temps_hourly$Date[obs_temps_hourly$Site.name == s])
  dat2append <- alldata[alldata$Observe.date > maxdate & alldata$Site.name == s,]
  dat2append <- dat2append[!is.na(dat2append$Temperature),]
  obs_temps_hourly <- rbind(obs_temps_hourly, dat2append)
}
summary(obs_temps_hourly)
write.csv(obs_temps_hourly, "all_data/salmon_environmental_data.csv", row.names = F)


# for excel:
#=CONCAT(TEXT(A3,"mm/dd/yyyy")," ",TEXT(B3,"hh:mm"))
#=DATE(YEAR(A3)+N, MONTH(A3), DAY(A3))