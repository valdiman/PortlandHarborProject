# Read and retrieve data from USGS

# Install package
install.packages("dataRetrieval")

# Load libraries
library(dataRetrieval) # read data from USGS

# Read data from USGS station
USGSPH <- "14211720" # Station @ WILLAMETTE RIVER AT PORTLAND, OR
# Water temperature in C
# August 2018
Watertemp082018.2 <- readNWISdv(USGSPH, "00010",
                                "2018-08-01", "2018-08-31")
twater.mean <- mean(Watertemp082018.2$X_00010_00003)
twater.error <- sd(Watertemp082018.2$X_00010_00003)

# specific dates
Watertemp082018 <- readNWISdv(USGSPH, "00010",
                              "2018-08-17", "2018-08-28") # 21, 22, 23, 24, 25
Watertemp112018 <- readNWISdv(USGSPH, "00010",
                              "2018-11-24", "2018-12-03") # 27, 28, 30, 01
Watertemp012019 <- readNWISdv(USGSPH, "00010",
                              "2019-01-23", "2019-01-31") # 26, 27
Watertemp022019 <- readNWISdv(USGSPH, "00010",
                              "2019-02-13", "2019-02-22") # 17, 18
Watertemp072022 <- readNWISdv(USGSPH, "00010",
                              "2022-06-01", "2022-09-01")

# Dissolved organic matter fluorescence (fDOM),
# water, in situ, concentration estimated from reference material,
# micrograms per liter as quinine sulfate equivalents (QSE), ug/l QSE
fDOM082018 <- readNWISdv(USGSPH, "32295",
                         "2018-08-20", "2018-09-01")
fDOM112018 <- readNWISdv(USGSPH, "32295",
                         "2018-11-26", "2018-12-02")
fDOM012019 <- readNWISdv(USGSPH, "32295",
                         "2019-01-25", "2019-02-20")
fDOM2022 <- readNWISdv(USGSPH, "32295",
                       "2022-06-01", "2022-09-01")
# Specific conductance, water, unfiltered, microsiemens per centimeter (uS/cm)
# at 25 degrees Celsius
# No need to correct, levels here are around rain (2 to 100 uS/cm)
# seawater ~ 50 mS/cm
WaterConduct082018 <- readNWISdv(USGSPH, "00095",
                                 "2018-08-20", "2018-09-01")
WaterConduct112018 <- readNWISdv(USGSPH, "00095",
                                 "2018-11-26", "2018-12-02")
WaterConduct012019 <- readNWISdv(USGSPH, "00095",
                                 "2019-01-25", "2019-02-20")
WaterConduct2022 <- readNWISdv(USGSPH, "00095",
                               "2022-06-01", "2022-09-01")

# Mean water velocity for discharge computation, feet per second,
# Morrison Bridge
# Not working! (11/16/2022)
WaterVeloc082018 <- readNWISdv(USGSPH, "72255",
                               "2018-08-20", "2018-09-01")
WaterVeloc112018 <- readNWISdv(USGSPH, "72255",
                               "2018-11-26", "2018-12-02")
WaterVeloc012019 <- readNWISdv(USGSPH, "72255",
                               "2019-01-25", "2019-02-20")
WaterVeloc2022 <- readNWISdv(USGSPH, "72255",
                             "2022-06-01", "2022-09-01")
