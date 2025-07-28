# Code to read NOAA meteorological data
# to be used for flux calculations
# https://docs.ropensci.org/rnoaa/articles/rnoaa.html
# Documentation:
# https://www.ncei.noaa.gov/data/global-hourly/doc/isd-format-document.pdf

# Packages and libraries needed --------------------------------------------
# Install packages
install.packages("rnoaa") # For future use, noaaweather package will need to be installed it.
install.packages("leaflet")
install.packages("dplyr")
install.packages("stringr")

# Libraries
{
  library(rnoaa)
  library(leaflet) # to plot ISD stations
  library(dplyr)
  library(stringr) # function str_detect
}

# Read data ---------------------------------------------------------------
# Read all station
stations <- isd_stations()

# Plot stations -----------------------------------------------------------
# Remove incomplete cases, those at 0,0
df <- stations[complete.cases(stations$lat, stations$lon), ]
df <- df[df$lat != 0, ]
# Make plot
leaflet(data = df) %>%
  addTiles() %>%
  addCircles()

# Select station ----------------------------------------------------------
# Select Portland International Airport
PO.station <- stations[stations$usaf == "726980", ]
# Select year
met2018 <- isd(usaf = PO.station$usaf, wban = PO.station$wban, year = 2018)
head(met2018)
# Select month
# Just August 2018
met201808 <- met2018[str_detect(met2018$date, "201808"), ]

# Select meteorological parameters ----------------------------------------
# (i) Temperature
# In Celsius, but needs to be divided by 10, e.g., 240 -> 24.0
# Transform character to number and divide by 10
met201808.tmp <- as.numeric(met201808$temperature)/10
# Remove 999 values
met201808.tmp <- met201808.tmp[!(met201808.tmp > 900)]
# Review data
hist(met201808.tmp)
boxplot(met201808.tmp)
summary(met201808.tmp)
tair.mean <- mean(met201808.tmp)
tair.error <- sd(met201808.tmp)

# (ii) Wind speed
# In m/s, but needs to be divided by 10, e.g., 0024 -> 2.4
# Transform character to number and divide by 10
met201808.ws <- as.numeric(met201808$wind_speed)/10
# Remove 999 values
met201808.ws <- met201808.ws[!(met201808.ws > 900)]
# Review data
hist(met201808.ws)
boxplot(met201808.ws)
summary(met201808.ws)
u.mean <- mean(met201808.ws)
u.error <- sd(met201808.ws)

# (iii) Atmospheric pressure
# In hPa (= mbar), but needs to be divided by 10, e.g., 10001 -> 1000.1
# Transform character to number and divide by 10
met201808.pr <- as.numeric(met201808$air_pressure)/10
# Remove 999 values
met201808.pr <- met201808.pr[!(met201808.pr > 9000)]
# Review data
hist(met201808.pr)
boxplot(met201808.pr)
summary(met201808.pr)
P.mean <- mean(met201808.pr)
P.error <- sd(met201808.pr)
