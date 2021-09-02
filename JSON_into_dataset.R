# Preparatory script for reading in JSoN data from Google location history
# Peter Lugtig p.lugtig@uu.nl
# 18 June 2019
###########################

library(dplyr)
library(readr)
library(rjson)
library(jsonlite)
library(here)

# download your own data here by replacing the json file "peter.json"
json_data <- fromJSON(paste(readLines("Peter.json"), collapse=""))
saveRDS(json_data, "Peterloc.rds")
d<-as.data.frame(readRDS("PeterLoc.rds"))
names(d) <- c("time","lat","lon","accuracy","altitude","verticalaccuracy",
              "activity","velocity","heading")


data<-d %>% top_n(10000,time) %>% dplyr::select(lat,lon, velocity, accuracy,time)
head(data)

# load test data (if all takes too long or stuff breaks down)
#data <- read.csv("test.csv", header=T,sep=',', quote="") # no quotations
View(data)
names(data)

# handle locations
data$lon <- data$lon/10000000
data$lat <- data$lat/10000000

# handle time
# Second rounding scritp
base<-60
# rounding function
mround <- function(x,base){ 
  base*round(x/base) 
} 

#round by minutes
act.df <- data
act.df$roundTs<-mround(as.numeric(act.df$time),base)
act.df$roundTs<-as.POSIXct(as.numeric(as.character(act.df$roundTs)), origin = "1970-01-01")

#skip this below?
act.df<-act.df %>% mutate(year=strftime(roundTs,"%y"),
                          month=strftime(roundTs,"%m"),
                          day = strftime(roundTs,"%d"),
                          hour = strftime(roundTs,"%H"),
                          minute = strftime(roundTs,"%M"),
                          second = strftime(roundTs,"%S"))

#issue if raster is loaded 
  # calculate distance to previous

# variables starting with X are positions, measured by GPS
# variables starting with Y are movements, measured by accelerometer data


# Shifting vectors for latitude and longitude to include end position
shift.vec <- function(vec, shift){
  if (length(vec) <= abs(shift)){
    rep(NA ,length(vec))
  } else {
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec) - shift)]) }
    else {
      c(vec[(abs(shift) + 1):length(vec)], rep(NA, abs(shift)))
    }
  }
}


##############################
# now continue with reworked time
# actdf or
# data


# shifting vectors to calculate distances
data$lat.p1 <- shift.vec(data$lat, -1)
data$lon.p1 <- shift.vec(data$lon, -1)

library(raster)

data$dist.to.prev <- apply(data, 1, FUN = function(row) {
  pointDistance(c(as.numeric(as.character(row["lat.p1"])),
                  as.numeric(as.character(row["lon.p1"]))),
                c(as.numeric(as.character(row["lat"])), as.numeric(as.character(row["lon"]))),
                lonlat = T) # Parameter 'lonlat' has to be TRUE!
})


##################################################################
# alternative: load data straight away into R (pre-processed)

Peter <-readRDS("Peterloc.rds")
