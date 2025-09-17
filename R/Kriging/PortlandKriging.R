

install.packages(c("gstat", "sp", "sf", "automap", "raster", "mapview"))

{
  library(gstat)    # Kriging functions
  library(sp)       # Spatial data handling
  library(sf)       # Modern spatial data
  library(automap)  # Automatic variogram fitting
  library(raster)   # For spatial grids
  library(mapview)  # Interactive mapping
  library(tidyverse)
}

# Read data ---------------------------------------------------------------
air.raw <- read.csv("Data/AirConc.csv", check.names = FALSE, header = TRUE)
# Select PCB
pcb_data <- air %>%
  select(Latitude, Longitude, PCB11) %>%
  filter(!is.na(PCB11))

# Convert to spatial object
coordinates(pcb_data) <- ~Longitude + Latitude
proj4string(pcb_data) <- CRS(SRS_string = "EPSG:4326")  # WGS84

# Transform to metric system (UTM) for accurate distance calculations
pcb_data_utm <- spTransform(pcb_data, CRS("+init=epsg:32610"))  # UTM zone 10N (Oregon/Washington)

# Exploratory analysis
summary(pcb_data_utm$PCB11)
hist(pcb_data_utm$PCB11, main = "PCB11 Concentration Distribution")

# Check spatial distribution
mapview(pcb_data_utm, zcol = "PCB11", layer.name = "PCB11 Concentration")

# Calculate and plot variogram
vario <- autofitVariogram(PCB11 ~ 1, pcb_data_utm)
plot(vario)

# Create prediction grid
bbox <- bbox(pcb_data_utm)
grid <- expand.grid(
  x = seq(bbox[1,1], bbox[1,2], length.out = 100),
  y = seq(bbox[2,1], bbox[2,2], length.out = 100)
)
coordinates(grid) <- ~x + y
gridded(grid) <- TRUE
proj4string(grid) <- proj4string(pcb_data_utm)

# Perform ordinary kriging
krige_result <- krige(PCB11 ~ 1, pcb_data_utm, grid, model = vario$var_model)

# Convert to raster for visualization
prediction_raster <- raster(krige_result["var1.pred"])
variance_raster <- raster(krige_result["var1.var"])

# Plot results
par(mfrow = c(1, 2))
plot(prediction_raster, main = "PCB11 Concentration Prediction")
points(pcb_data_utm, pch = 16, cex = 0.8)
plot(variance_raster, main = "Prediction Variance")
points(pcb_data_utm, pch = 16, cex = 0.8)



# 1. Create a grid that tightly wraps your data points
data_bbox <- bbox(pcb_data_utm)

# Calculate data extent
x_range <- data_bbox[1,2] - data_bbox[1,1]
y_range <- data_bbox[2,2] - data_bbox[2,1]

# Create grid with 20% buffer around data points
grid_tight <- expand.grid(
  x = seq(data_bbox[1,1] - 0.2*x_range, 
          data_bbox[1,2] + 0.2*x_range, 
          length.out = 100),  # Higher resolution
  y = seq(data_bbox[2,1] - 0.2*y_range, 
          data_bbox[2,2] + 0.2*y_range, 
          length.out = 100)
)

coordinates(grid_tight) <- ~x + y
gridded(grid_tight) <- TRUE
proj4string(grid_tight) <- proj4string(pcb_data_utm)

# 2. Perform kriging on the tighter grid
krige_result_tight <- krige(PCB11 ~ 1, pcb_data_utm, grid_tight, 
                            model = vario_model, nmax = 10)

# 3. Check results
cat("Tight grid kriging results:\n")
print(summary(krige_result_tight$var1.pred))
cat("Non-NA predictions:", sum(!is.na(krige_result_tight$var1.pred)), 
    "/", length(krige_result_tight$var1.pred), "\n")

# 4. Create interactive map
library(mapview)
mapview(raster(krige_result_tight["var1.pred"]), 
        layer.name = "PCB11 Prediction") + 
  mapview(pcb_data_utm, zcol = "PCB11")


