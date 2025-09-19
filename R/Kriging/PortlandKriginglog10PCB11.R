# Codes to perform a Kriging analysis for PCB 11
# Generated results are mapped in QGIS

install.packages(c("gstat", "sp", "sf", "raster", "ape", "viridis"))

{
  library(gstat)    # Kriging functions
  library(sp)       # Spatial data handling
  library(sf)       # Modern spatial data
  library(raster)   # For spatial grids
  library(tidyverse)
  library(ape)
  library(viridis)
  library(ggplot2)
}

# Read data ---------------------------------------------------------------
pcb_air <- read.csv("Data/AirConc.csv", check.names = FALSE, header = TRUE)

# Select PCB
pcbi <- pcb_air %>%
  select(Latitude, Longitude, PCB11) %>%
  filter(!is.na(PCB11))

# Format data -------------------------------------------------------------
coordinates(pcbi) <- ~Longitude + Latitude
proj4string(pcbi) <- CRS(SRS_string = "EPSG:4326")
pcbi_utm <- spTransform(pcbi, CRS(SRS_string = "EPSG:32610"))

coords_df <- as.data.frame(coordinates(pcbi_utm))
coords_df$PCB11 <- pcbi_utm$PCB11

unique_coords <- coords_df %>%
  group_by(coords.x1, coords.x2) %>%
  summarise(PCB11_mean = mean(PCB11, na.rm = TRUE),
            n_samples = n(),
            .groups = "drop")

pcbi_unique <- unique_coords
coordinates(pcbi_unique) <- ~coords.x1 + coords.x2
pcbi_unique$PCB11 <- pcbi_unique$PCB11_mean
proj4string(pcbi_unique) <- proj4string(pcbi_utm)

pcbi_unique$log_PCB11 <- log10(pcbi_unique$PCB11) # use log10 scale

empirical_vario <- variogram(log_PCB11 ~ 1, pcbi_unique, 
                             cutoff = 10000,
                             width = 1000)

manual_vario <- fit.variogram(empirical_vario, 
                              model = vgm(psill = 0.1,
                                          model = "Sph",
                                          range = 5000,
                                          nugget = 0.05))

data_bbox <- bbox(pcbi_unique)
x_range <- data_bbox[1,2] - data_bbox[1,1]
y_range <- data_bbox[2,2] - data_bbox[2,1]

grid_tight <- expand.grid(
  x = seq(data_bbox[1,1] - 0.05*x_range, 
          data_bbox[1,2] + 0.05*x_range, 
          length.out = 50),
  y = seq(data_bbox[2,1] - 0.05*y_range, 
          data_bbox[2,2] + 0.05*y_range, 
          length.out = 50)
)

coordinates(grid_tight) <- ~x + y
gridded(grid_tight) <- TRUE
proj4string(grid_tight) <- proj4string(pcbi_unique)

krige_result <- krige(log_PCB11 ~ 1, pcbi_unique, grid_tight, 
                      model = manual_vario,
                      nmax = 10)

krige_result$pred_original <- 10^krige_result$var1.pred
krige_raster <- raster(krige_result["pred_original"])

# Convert raster to dataframe for ggplot
krige_df <- as.data.frame(krige_raster, xy = TRUE)
tpcb_df  <- as.data.frame(pcbi_unique)

ggplot() +
  geom_raster(data = krige_df, aes(x = x, y = y, fill = pred_original)) +
  scale_fill_viridis_c(option = "C", na.value = NA, name = "PCB 11 (ng/m³)") +
  geom_point(data = tpcb_df, aes(x = coords.x1, y = coords.x2, size = PCB11_mean),
             color = "blue", alpha = 0.7) +
  theme_minimal() +
  coord_equal() +
  labs(title = "Kriging Prediction of PCB 11 Concentration")

# Evaluate Kriging model --------------------------------------------------
# Moran's I test for spatial autocorrelation
coords <- coordinates(pcbi_unique)
dist_mat <- as.matrix(dist(coords))
dist_inv <- 1/dist_mat
diag(dist_inv) <- 0

moran_test <- Moran.I(pcbi_unique$PCB11, dist_inv)
print("Moran's I test for spatial autocorrelation:")
print(moran_test)

# Leave-one-out cross-validation
krige_cv <- krige.cv(log_PCB11 ~ 1, pcbi_unique, manual_vario)

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
residual_sf <- st_as_sf(pcbi_unique)
residual_sf$residual <- krige_cv$residual
plot(residual_sf["residual"], pch = 16, main = "Spatial Residual Pattern")

cat("=== KRIGING VALIDATION REPORT ===\n")
cat("Spatial autocorrelation (Moran's I):", round(moran_test$observed, 3), "\n")
cat("Moran's I p-value:", round(moran_test$p.value, 4), "\n")
cat("Variogram R²:", round(vario_r2, 3), "\n\n")

cat("CROSS-VALIDATION METRICS (log scale):\n")
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





