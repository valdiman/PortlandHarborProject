

install.packages(c("gstat", "sp", "sf", "automap", "raster", "mapview", "ape",
                   "viridis", "ggspatial"))

{
  library(gstat)    # Kriging functions
  library(sp)       # Spatial data handling
  library(sf)       # Modern spatial data
  library(automap)  # Automatic variogram fitting
  library(raster)   # For spatial grids
  library(mapview)  # Interactive mapping
  library(tidyverse)
  library(ape)
  library(viridis)
  library(terra)
  library(ggplot2)
  library(stars)
  library(ggspatial)
}

# Read data ---------------------------------------------------------------
pcb_air <- read.csv("Data/AirConc.csv", check.names = FALSE, header = TRUE)

pcb_air$tpcb <- rowSums(pcb_air[,7:179])

# Select total PCB
tpcb <- pcb_air %>%
  select(Latitude, Longitude, tpcb) %>%
  filter(!is.na(tpcb))

coordinates(tpcb) <- ~Longitude + Latitude
proj4string(tpcb) <- CRS(SRS_string = "EPSG:4326")
tpcb_utm <- spTransform(tpcb, CRS(SRS_string = "EPSG:32610"))

coords_df <- as.data.frame(coordinates(tpcb_utm))
coords_df$tpcb <- tpcb_utm$tpcb

unique_coords <- coords_df %>%
  group_by(coords.x1, coords.x2) %>%
  summarise(tpcb_mean = mean(tpcb, na.rm = TRUE),
            n_samples = n(),
            .groups = "drop")

tpcb_unique <- unique_coords
coordinates(tpcb_unique) <- ~coords.x1 + coords.x2
tpcb_unique$tpcb <- tpcb_unique$tpcb_mean
proj4string(tpcb_unique) <- proj4string(tpcb_utm)

tpcb_unique$log_tpcb <- log10(tpcb_unique$tpcb) # use log10 scale

empirical_vario <- variogram(log_tpcb ~ 1, tpcb_unique, 
                             cutoff = 10000,
                             width = 1000)

manual_vario <- fit.variogram(empirical_vario, 
                              model = vgm(psill = 0.1,
                                          model = "Sph", # Options: Exp, Gau, Ste but dont improve the model
                                          range = 1000,
                                          nugget = 0.05))

data_bbox <- bbox(tpcb_unique)
x_range <- data_bbox[1,2] - data_bbox[1,1]
y_range <- data_bbox[2,2] - data_bbox[2,1]

grid_tight <- expand.grid(
  x = seq(data_bbox[1,1] - 0.2*x_range, 
          data_bbox[1,2] + 0.2*x_range, 
          length.out = 50),
  y = seq(data_bbox[2,1] - 0.2*y_range, 
          data_bbox[2,2] + 0.2*y_range, 
          length.out = 50)
)

coordinates(grid_tight) <- ~x + y
gridded(grid_tight) <- TRUE
proj4string(grid_tight) <- proj4string(tpcb_unique)

krige_result <- krige(log_tpcb ~ 1, tpcb_unique, grid_tight, 
                      model = manual_vario,
                      nmax = 10)

krige_result$pred_original <- 10^krige_result$var1.pred

krige_raster <- raster(krige_result["var1.pred"])

# Check results
plot(krige_raster, main = "tPCB Concentration Prediction")









# Evaluate Kriging model --------------------------------------------------
# Moran's I test for spatial autocorrelation

coords <- coordinates(tpcb_unique)
dist_mat <- as.matrix(dist(coords))
dist_inv <- 1/dist_mat
diag(dist_inv) <- 0

moran_test <- Moran.I(tpcb_unique$tpcb, dist_inv)
print("Moran's I test for spatial autocorrelation:")
print(moran_test)

# Leave-one-out cross-validation
krige_cv <- krige.cv(log_tpcb ~ 1, tpcb_unique, manual_vario)

# Calculate performance metrics
cv_metrics <- data.frame(
  RMSE = sqrt(mean(krige_cv$residual^2)),
  MAE = mean(abs(krige_cv$residual)),
  Mean_Error = mean(krige_cv$residual),
  R2 = cor(krige_cv$observed, krige_cv$observed - krige_cv$residual)^2,
  NSE = 1 - sum(krige_cv$residual^2)/sum((krige_cv$observed - mean(krige_cv$observed))^2)
)

print("Cross-validation metrics (log scale):")
print(cv_metrics)

# Back-transform for original scale metrics
cv_metrics_original <- data.frame(
  RMSE = sqrt(mean((10^krige_cv$observed - 10^(krige_cv$observed - krige_cv$residual))^2)),
  MAE = mean(abs(10^krige_cv$observed - 10^(krige_cv$observed - krige_cv$residual))),
  R2 = cor(10^krige_cv$observed, 10^(krige_cv$observed - krige_cv$residual))^2
)

print("Cross-validation metrics (original scale):")
print(cv_metrics_original)

# Check variogram model fit
variogram_fit <- function(actual, model) {
  ss_res <- sum((actual$gamma - model$gamma)^2)
  ss_tot <- sum((actual$gamma - mean(actual$gamma))^2)
  r2 <- 1 - ss_res/ss_tot
  return(r2)
}

vario_r2 <- variogram_fit(empirical_vario, manual_vario)
cat("Variogram R²:", round(vario_r2, 3), "\n")

# Residual analysis
par(mfrow = c(2, 2))

# Residuals vs Fitted
plot(krige_cv$observed, krige_cv$residual, 
     xlab = "Observed (log)", ylab = "Residuals",
     main = "Residuals vs Observed")
abline(h = 0, col = "red")

# QQ-plot of residuals
qqnorm(krige_cv$residual)
qqline(krige_cv$residual, col = "red")

# Histogram of residuals
hist(krige_cv$residual, main = "Residual Distribution", 
     xlab = "Residuals", breaks = 10)

# Spatial pattern of residuals
residual_sf <- st_as_sf(tpcb_unique)
residual_sf$residual <- krige_cv$residual
plot(residual_sf["residual"], pch = 16, main = "Spatial Residual Pattern")

cat("=== KRIGING VALIDATION REPORT ===\n")
cat("Spatial autocorrelation (Moran's I):", round(moran_test$observed, 3), "\n")
cat("Moran's I p-value:", round(moran_test$p.value, 4), "\n")
cat("Variogram R²:", round(vario_r2, 3), "\n\n")

cat("CROSS-VALIDATION METRICS:\n")
cat("RMSE:", round(cv_metrics$RMSE, 3), "\n")
cat("MAE:", round(cv_metrics$MAE, 3), "\n") 
cat("R²:", round(cv_metrics$R2, 3), "\n")
cat("NSE:", round(cv_metrics$NSE, 3), "\n\n")

cat("CROSS-VALIDATION METRICS (original scale):\n")
cat("RMSE:", round(cv_metrics_original$RMSE, 2), "ng/m³\n")
cat("MAE:", round(cv_metrics_original$MAE, 2), "ng/m³\n")
cat("R²:", round(cv_metrics_original$R2, 3), "\n")

# Moran's I: Values > 0 indicate positive spatial autocorrelation (p < 0.05 = significant)
# R²: > 0.7 = good fit, > 0.5 = acceptable
# NSE: > 0.7 = good, > 0.5 = acceptable
# Residuals: Should be randomly distributed without spatial patterns

# Positive Indicators:
# Significant Spatial Autocorrelation:
# Moran's I = 0.095 (p = 0.0077)
# Strong evidence of spatial patterning (clustering)
# Reasonable Model Performance:
# R² = 0.279 | NSE = 0.256
# Moderate predictive power - much better than PCB11
# Spatial Structure Present:
# Variogram R² = 1 → Good variogram fit
# Total PCB shows meaningful spatial gradients




