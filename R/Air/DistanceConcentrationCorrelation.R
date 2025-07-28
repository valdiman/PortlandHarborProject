# Figure 2. Distance versus measured, distance-modeled and AERMOD-modeled
# airborne PCB concentrations

# Packages and libraries needed --------------------------------------------
# Install packages
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")

# Load libraries
{
  library(ggplot2)
  library(tidyr)
  library(dplyr)
}

# Read data ---------------------------------------------------------------
air.raw <- read.csv("Data/AirConcV2.csv", check.names = FALSE, header = TRUE)
# Remove metadata
air <- air.raw[, 7:179]

# Obtain total PCB concentation
tPCB <- rowSums(air)
# Need to combine 45 and 51 to match water and flux emissions
air$'PCB45+51' <- air$PCB45 + air$PCB51

# Select PCB congeners for analysis
pcbi <- data.frame(
  PCB11 = air$PCB11,
  'PCB18+30' = air$`PCB18+30`,
  'PCB45+51' = air$`PCB45+51`,
  PCB68 = air$PCB68,
  tPCB = tPCB,
  dis = c(241, 241, 241, 48, 64, 64, 64, 209, 10, 10, 10, 10,
          2414, 2414, 483, 483, 483, 2253, 1609, 10, 483, 956,
          1770, 1287, 1287, 2253, 257),
  model.tpcb.2022 = c(17.08, 17.08, 26.71, 14.13, 83.63, 83.63,
                      33.73, 26.18, 29.34, 29.34, 29.34, 7.15,
                      3.9, 5.48, 12.14, 12.14, 13.07, 5.12, 5.16,
                      33.63, 14.89, 5.2, 3.88, 8.62, 9.08, 7.08,
                      36.09),
  model.pcb11 = c(0.56, 0.56, 0.88, 0.47, 2.75, 2.75, 1.11, 0.86,
                  0.97, 0.97, 0.97, 0.24, 0.13, 0.18, 0.4, 0.4,
                  0.43, 0.17, 0.17, 1.11, 0.49, 0.17, 0.13, 0.28,
                  0.3, 0.23, 1.19),
  model.pcb45.51 = c(5.43, 5.43, 8.48, 4.49, 26.56, 26.56, 10.71,
                  8.32, 9.32, 9.32, 9.32, 2.27, 1.24, 1.74,
                  3.86, 3.86, 4.15, 1.63, 1.64, 10.68, 4.73, 1.65,
                  1.23, 2.74, 2.88, 2.25, 11.46),
  model.pcb68 = c(0.57, 0.57, 0.88, 0.47, 2.77, 2.77, 1.12, 0.87,
                  0.97, 0.97, 0.97, 0.24, 0.13, 0.18, 0.4, 0.4,
                  0.43, 0.17, 0.17, 1.11, 0.49, 0.17, 0.13, 0.29,
                  0.3, 0.23, 1.19)
  )

# Format data -------------------------------------------------------------
# Change data format to long format
pcbi_long <- pcbi %>%
  pivot_longer(
    cols = starts_with("pcb"),
    names_to = "congener",
    values_to = "value"
  )

# Model  and plot ---------------------------------------------------------
# Logarithmic regression model
# Model is pcb = a*ln(dis)+b

# Total PCBs
fit.tpcb <- lm(tPCB ~ log(dis), data = pcbi) # need to change tpcb
summary.fit.tpcb <- summary(fit.tpcb)
summary.fit.tpcb$coefficients

r_squared <- summary.fit.tpcb$r.squared
r2_label <- paste0("R² = ", round(r_squared, 2))
p_value_a <- summary.fit.tpcb$coefficients["log(dis)", "Pr(>|t|)"]
p_label <- paste0("p-value = ", signif(p_value_a, 2))

# New distance values to make the prediction looks smoother in plot
dis_smooth <- seq(min(pcbi$dis), max(pcbi$dis), length.out = 500)

# Prediction
tpcb_fit_smooth <- predict(fit.tpcb, newdata = data.frame(dis = dis_smooth))

# Create a new data for smoothed predictions
smoothed_data <- data.frame(dis = dis_smooth, tpcb_fit = tpcb_fit_smooth)

# Add legend labels explicitly
pcbi$legend_measured <- factor("Measured",
                             levels = c("Measured", "AERMOD Prediction", "log-distance model"))
pcbi$legend_aermod   <- factor("AERMOD Prediction",
                             levels = c("Measured", "AERMOD Prediction", "log-distance model"))
smoothed_data$legend_model <- factor("log-distance model",
                                     levels = c("Measured", "AERMOD Prediction", "log-distance model"))

# Plot total PCBs
plotTpcb <- ggplot() +
  geom_point(data = pcbi, aes(x = dis, y = tPCB, color = legend_measured), size = 2) +
  geom_point(data = pcbi, aes(x = dis, y = model.tpcb.2022, color = legend_aermod),
             shape = 20, size = 3) +
  geom_line(data = smoothed_data, aes(x = dis, y = tpcb_fit, color = legend_model),
            linewidth = 1) +
  scale_color_manual(
    values = c("Measured" = "black",
               "AERMOD Prediction" = "red",
               "log-distance model" = "blue")
  ) +
  xlab(expression(bold("Distance to water (m)"))) +
  ylab(expression(bold(Sigma*"PCB (pg/m"^3*")"))) +
  annotate("text", x = Inf, y = Inf, label = r2_label, hjust = 1.1, vjust = 2, size = 4) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold", size = 12),
    aspect.ratio = 1)

# See plot
plotTpcb

# Save plot
ggsave("Output/Plots/TPCBDist.png", plot = plotTpcb, width = 5,
       height = 5, dpi = 500)

# PCB 11
fit.pcb <- lm(PCB11 ~ log(dis), data = pcbi)
summary.fit.pcb <- summary(fit.pcb)
summary.fit.pcb$coefficients

r_squared <- summary.fit.pcb$r.squared
r2_label <- paste0("R² = ", signif(r_squared, 2))
p_value_a <- summary.fit.pcb$coefficients["log(dis)", "Pr(>|t|)"]
p_label <- paste0("p-value = ", signif(p_value_a, 2))

# New distance values to make the prediction looks smoother in plot
dis_smooth <- seq(min(pcbi$dis), max(pcbi$dis), length.out = 500)

# Prediction
pcb_fit_smooth <- predict(fit.pcb, newdata = data.frame(dis = dis_smooth))

# Create a new data for smoothed predictions
smoothed_data <- data.frame(dis = dis_smooth, pcb_fit = pcb_fit_smooth)

# Add legend labels explicitly
pcbi$legend_observed <- factor("Measured",
                             levels = c("Measured", "AERMOD Prediction", "log-distance model"))
pcbi$legend_aermod   <- factor("AERMOD Prediction",
                             levels = c("Measured", "AERMOD Prediction", "log-distance model"))
smoothed_data$legend_model <- factor("log-distance model",
                                     levels = c("Measured", "AERMOD Prediction", "log-distance model"))

# Plot PCB 11
plotpcb11 <- ggplot() +
  geom_point(data = pcbi, aes(x = dis, y = PCB11, color = legend_measured), size = 2) +
  geom_point(data = pcbi, aes(x = dis, y = model.pcb11, color = legend_aermod),
             shape = 20, size = 3) +
  geom_line(data = smoothed_data, aes(x = dis, y = pcb_fit, color = legend_model),
            linewidth = 1) +
  scale_color_manual(
    values = c("Measured" = "black",
               "AERMOD Prediction" = "red",
               "log-distance model" = "blue")
  ) +
  xlab(expression(bold("Distance to water (m)"))) +
  ylab(expression(bold("PCB 11 (pg/m"^3*")"))) +
  annotate("text", x = Inf, y = Inf, label = r2_label, hjust = 1.1, vjust = 2, size = 4) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold", size = 12),
    aspect.ratio = 1)

# See plot
plotpcb11

# Save plot
ggsave("Output/Plots/PCB11Dist.png", plot = plotpcb11, width = 5,
       height = 5, dpi = 500)

# PCBs 45+51
fit.pcb <- lm(PCB45.51 ~ log(dis), data = pcbi)
summary.fit.pcb <- summary(fit.pcb)
summary.fit.pcb$coefficients

r_squared <- summary.fit.pcb$r.squared
r2_label <- paste0("R² = ", signif(r_squared, 2))
p_value_a <- summary.fit.pcb$coefficients["log(dis)", "Pr(>|t|)"]
p_label <- paste0("p-value = ", signif(p_value_a, 2))

# New distance values to make the prediction looks smoother in plot
dis_smooth <- seq(min(pcbi$dis), max(pcbi$dis), length.out = 500)

# Prediction
pcb_fit_smooth <- predict(fit.pcb, newdata = data.frame(dis = dis_smooth))

# Create a new data for smoothed predictions
smoothed_data <- data.frame(dis = dis_smooth, pcb_fit = pcb_fit_smooth)

# Add legend labels explicitly
pcbi$legend_observed <- factor("Measured",
                             levels = c("Measured", "AERMOD Prediction", "log-distance model"))
pcbi$legend_aermod   <- factor("AERMOD Prediction",
                             levels = c("Measured", "AERMOD Prediction", "log-distance model"))
smoothed_data$legend_model <- factor("log-distance model",
                                     levels = c("Measured", "AERMOD Prediction", "log-distance model"))

# Plot PCBs 45+51
plotpcb4551 <- ggplot() +
  geom_point(data = pcbi, aes(x = dis, y = PCB45.51, color = legend_observed), size = 2) +
  geom_point(data = pcbi, aes(x = dis, y = model.pcb45.51, color = legend_aermod),
             shape = 20, size = 3) +
  geom_line(data = smoothed_data, aes(x = dis, y = pcb_fit, color = legend_model),
            linewidth = 1) +
  scale_color_manual(
    values = c("Measured" = "black",
               "AERMOD Prediction" = "red",
               "log-distance model" = "blue")
  ) +
  xlab(expression(bold("Distance to water (m)"))) +
  ylab(expression(bold("PCB 45+51 (pg/m"^3*")"))) +
  annotate("text", x = Inf, y = Inf, label = r2_label, hjust = 1.1, vjust = 2, size = 4) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold", size = 12),
    aspect.ratio = 1)

# See plot
plotpcb4551

# Save plot
ggsave("Output/Plots/PCB4551Dist.png", plot = plotpcb4551, width = 5,
       height = 5, dpi = 500)

# PCB 68
fit.pcb <- lm(PCB68 ~ log(dis), data = pcbi)
summary.fit.pcb <- summary(fit.pcb)
summary.fit.pcb$coefficients

r_squared <- summary.fit.pcb$r.squared
r2_label <- paste0("R² = ", signif(r_squared, 2))
p_value_a <- summary.fit.pcb$coefficients["log(dis)", "Pr(>|t|)"]
p_label <- paste0("p-value = ", signif(p_value_a, 2))

# New distance values to make the prediction looks smoother in plot
dis_smooth <- seq(min(pcbi$dis), max(pcbi$dis), length.out = 500)

# Prediction
pcb_fit_smooth <- predict(fit.pcb, newdata = data.frame(dis = dis_smooth))

# Create a new data for smoothed predictions
smoothed_data <- data.frame(dis = dis_smooth, pcb_fit = pcb_fit_smooth)

# Add legend labels explicitly
pcbi$legend_observed <- factor("Measured",
                             levels = c("Measured", "AERMOD Prediction", "log-distance model"))
pcbi$legend_aermod   <- factor("AERMOD Prediction",
                             levels = c("Measured", "AERMOD Prediction", "log-distance model"))
smoothed_data$legend_model <- factor("log-distance model",
                                     levels = c("Measured", "AERMOD Prediction", "log-distance model"))

# Plot PCB 68
plotpcb68 <- ggplot() +
  geom_point(data = pcbi, aes(x = dis, y = PCB68, color = legend_observed), size = 2) +
  geom_point(data = pcbi, aes(x = dis, y = model.pcb68, color = legend_aermod),
             shape = 20, size = 3) +
  geom_line(data = smoothed_data, aes(x = dis, y = pcb_fit, color = legend_model),
            linewidth = 1) +
  scale_color_manual(
    values = c("Measured" = "black",
               "AERMOD Prediction" = "red",
               "log-distance model" = "blue")
  ) +
  xlab(expression(bold("Distance to water (m)"))) +
  ylab(expression(bold("PCB 68 (pg/m"^3*")"))) +
  annotate("text", x = Inf, y = Inf, label = r2_label, hjust = 1.1, vjust = 2, size = 4) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold", size = 12),
    aspect.ratio = 1)

# See PLot
plotpcb68

# Save plot
ggsave("Output/Plots/PCB68Dist.png", plot = plotpcb68, width = 5,
       height = 5, dpi = 500)

