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
ph <- read.csv("Data/ph.csv",header = T)

# Format data -------------------------------------------------------------
# Change data format to long format
ph_long <- ph %>%
  pivot_longer(
    cols = starts_with("pcb"),
    names_to = "congener",
    values_to = "value"
  )

# Model  and plot ---------------------------------------------------------
# Logarithmic regression model
# Model is pcb = a*ln(dis)+b

# Total PCBs
fit.tpcb <- lm(tpcb ~ log(dis), data = ph) # need to change tpcb
summary.fit.tpcb <- summary(fit.tpcb)
summary.fit.tpcb$coefficients

r_squared <- summary.fit.tpcb$r.squared
r2_label <- paste0("R² = ", round(r_squared, 2))
p_value_a <- summary.fit.tpcb$coefficients["log(dis)", "Pr(>|t|)"]
p_label <- paste0("p-value = ", signif(p_value_a, 2))

# New distance values to make the prediction looks smoother in plot
dis_smooth <- seq(min(ph$dis), max(ph$dis), length.out = 500)

# Prediction
tpcb_fit_smooth <- predict(fit.tpcb, newdata = data.frame(dis = dis_smooth))

# Create a new data for smoothed predictions
smoothed_data <- data.frame(dis = dis_smooth, tpcb_fit = tpcb_fit_smooth)

# Add legend labels explicitly
ph$legend_measured <- factor("Measured",
                             levels = c("Measured", "AERMOD Prediction", "log-distance model"))
ph$legend_aermod   <- factor("AERMOD Prediction",
                             levels = c("Measured", "AERMOD Prediction", "log-distance model"))
smoothed_data$legend_model <- factor("log-distance model",
                                     levels = c("Measured", "AERMOD Prediction", "log-distance model"))

# Plot total PCBs
plotTpcb <- ggplot() +
  geom_point(data = ph, aes(x = dis, y = tpcb, color = legend_measured), size = 2) +
  geom_point(data = ph, aes(x = dis, y = model.tpcb2022, color = legend_aermod),
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
fit.pcb <- lm(pcb11 ~ log(dis), data = ph)
summary.fit.pcb <- summary(fit.pcb)
summary.fit.pcb$coefficients

r_squared <- summary.fit.pcb$r.squared
r2_label <- paste0("R² = ", signif(r_squared, 2))
p_value_a <- summary.fit.pcb$coefficients["log(dis)", "Pr(>|t|)"]
p_label <- paste0("p-value = ", signif(p_value_a, 2))

# New distance values to make the prediction looks smoother in plot
dis_smooth <- seq(min(ph$dis), max(ph$dis), length.out = 500)

# Prediction
pcb_fit_smooth <- predict(fit.pcb, newdata = data.frame(dis = dis_smooth))

# Create a new data for smoothed predictions
smoothed_data <- data.frame(dis = dis_smooth, pcb_fit = pcb_fit_smooth)

# Add legend labels explicitly
ph$legend_observed <- factor("Measured",
                             levels = c("Measured", "AERMOD Prediction", "log-distance model"))
ph$legend_aermod   <- factor("AERMOD Prediction",
                             levels = c("Measured", "AERMOD Prediction", "log-distance model"))
smoothed_data$legend_model <- factor("log-distance model",
                                     levels = c("Measured", "AERMOD Prediction", "log-distance model"))

# Plot PCB 11
plotpcb11 <- ggplot() +
  geom_point(data = ph, aes(x = dis, y = tpcb, color = legend_measured), size = 2) +
  geom_point(data = ph, aes(x = dis, y = model.tpcb2022, color = legend_aermod),
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
fit.pcb <- lm(pcb45.51 ~ log(dis), data = ph)
summary.fit.pcb <- summary(fit.pcb)
summary.fit.pcb$coefficients

r_squared <- summary.fit.pcb$r.squared
r2_label <- paste0("R² = ", signif(r_squared, 2))
p_value_a <- summary.fit.pcb$coefficients["log(dis)", "Pr(>|t|)"]
p_label <- paste0("p-value = ", signif(p_value_a, 2))

# New distance values to make the prediction looks smoother in plot
dis_smooth <- seq(min(ph$dis), max(ph$dis), length.out = 500)

# Prediction
pcb_fit_smooth <- predict(fit.pcb, newdata = data.frame(dis = dis_smooth))

# Create a new data for smoothed predictions
smoothed_data <- data.frame(dis = dis_smooth, pcb_fit = pcb_fit_smooth)

# Add legend labels explicitly
ph$legend_observed <- factor("Measured",
                             levels = c("Measured", "AERMOD Prediction", "log-distance model"))
ph$legend_aermod   <- factor("AERMOD Prediction",
                             levels = c("Measured", "AERMOD Prediction", "log-distance model"))
smoothed_data$legend_model <- factor("log-distance model",
                                     levels = c("Measured", "AERMOD Prediction", "log-distance model"))

# Plot PCBs 45+51
plotpcb4551 <- ggplot() +
  geom_point(data = ph, aes(x = dis, y = pcb45.51, color = legend_observed), size = 2) +
  geom_point(data = ph, aes(x = dis, y = model.pcb45.51, color = legend_aermod),
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
fit.pcb <- lm(pcb68 ~ log(dis), data = ph)
summary.fit.pcb <- summary(fit.pcb)
summary.fit.pcb$coefficients

r_squared <- summary.fit.pcb$r.squared
r2_label <- paste0("R² = ", signif(r_squared, 2))
p_value_a <- summary.fit.pcb$coefficients["log(dis)", "Pr(>|t|)"]
p_label <- paste0("p-value = ", signif(p_value_a, 2))

# New distance values to make the prediction looks smoother in plot
dis_smooth <- seq(min(ph$dis), max(ph$dis), length.out = 500)

# Prediction
pcb_fit_smooth <- predict(fit.pcb, newdata = data.frame(dis = dis_smooth))

# Create a new data for smoothed predictions
smoothed_data <- data.frame(dis = dis_smooth, pcb_fit = pcb_fit_smooth)

# Add legend labels explicitly
ph$legend_observed <- factor("Measured",
                             levels = c("Measured", "AERMOD Prediction", "log-distance model"))
ph$legend_aermod   <- factor("AERMOD Prediction",
                             levels = c("Measured", "AERMOD Prediction", "log-distance model"))
smoothed_data$legend_model <- factor("log-distance model",
                                     levels = c("Measured", "AERMOD Prediction", "log-distance model"))

# Plot PCB 68
plotpcb68 <- ggplot() +
  geom_point(data = ph, aes(x = dis, y = pcb68, color = legend_observed), size = 2) +
  geom_point(data = ph, aes(x = dis, y = model.pcb68, color = legend_aermod),
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

# Extra plots -------------------------------------------------------------
# PCB 4
fit.pcb <- lm(pcb4 ~ log(dis), data = ph)
summary.fit.pcb <- summary(fit.pcb)
summary.fit.pcb$coefficients

r_squared <- summary.fit.pcb$r.squared
r2_label <- paste0("R² = ", signif(r_squared, 2))
p_value_a <- summary.fit.pcb$coefficients["log(dis)", "Pr(>|t|)"]
p_label <- paste0("p-value = ", signif(p_value_a, 2))

# New distance values to make the prediction looks smoother in plot
dis_smooth <- seq(min(ph$dis), max(ph$dis), length.out = 500)

# Prediction
pcb_fit_smooth <- predict(fit.pcb, newdata = data.frame(dis = dis_smooth))

# Create a new data for smoothed predictions
smoothed_data <- data.frame(dis = dis_smooth, pcb_fit = pcb_fit_smooth)

# Add legend labels explicitly
ph$legend_observed <- factor("Observed",
                             levels = c("Observed", "AERMOD Prediction", "log-distance model"))
ph$legend_aermod   <- factor("AERMOD Prediction",
                             levels = c("Observed", "AERMOD Prediction", "log-distance model"))
smoothed_data$legend_model <- factor("log-distance model",
                                     levels = c("Observed", "AERMOD Prediction", "log-distance model"))

# Plot
plotpcb4 <- ggplot() +
  geom_point(data = ph, aes(x = dis, y = pcb4, color = legend_observed), size = 2) +
  geom_point(data = ph, aes(x = dis, y = model.pcb4, color = legend_aermod),
             shape = 20, size = 3) +
  geom_line(data = smoothed_data, aes(x = dis, y = pcb_fit, color = legend_model),
            linewidth = 1) +
  scale_color_manual(
    values = c("Observed" = "black",
               "AERMOD Prediction" = "red",
               "log-distance model" = "blue")
  ) +
  xlab(expression(bold("Distance to water (m)"))) +
  ylab(expression(bold("PCB 4 (pg/m"^3*")"))) +
  annotate("text", x = Inf, y = Inf, label = r2_label, hjust = 1.1, vjust = 2, size = 4) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold", size = 12),
    aspect.ratio = 1)

# See plot
plotpcb4

# PCB44+47+65
fit.pcb <- lm(pcb44.47.65 ~ log(dis), data = ph)
summary.fit.pcb <- summary(fit.pcb)
summary.fit.pcb$coefficients

r_squared <- summary.fit.pcb$r.squared
r2_label <- paste0("R² = ", signif(r_squared, 2))
p_value_a <- summary.fit.pcb$coefficients["log(dis)", "Pr(>|t|)"]
p_label <- paste0("p-value = ", signif(p_value_a, 2))

# New distance values to make the prediction looks smoother in plot
dis_smooth <- seq(min(ph$dis), max(ph$dis), length.out = 500)

# Prediction
pcb_fit_smooth <- predict(fit.pcb, newdata = data.frame(dis = dis_smooth))

# Create a new data for smoothed predictions
smoothed_data <- data.frame(dis = dis_smooth, pcb_fit = pcb_fit_smooth)

# Add legend labels explicitly
ph$legend_observed <- factor("Observed",
                             levels = c("Observed", "AERMOD Prediction", "log-distance model"))
ph$legend_aermod   <- factor("AERMOD Prediction",
                             levels = c("Observed", "AERMOD Prediction", "log-distance model"))
smoothed_data$legend_model <- factor("log-distance model",
                                     levels = c("Observed", "AERMOD Prediction", "log-distance model"))

# Plot
plotpcb444765 <- ggplot() +
  geom_point(data = ph, aes(x = dis, y = pcb44.47.65, color = legend_observed), size = 2) +
  geom_point(data = ph, aes(x = dis, y = model.pcb44.47.65, color = legend_aermod),
             shape = 20, size = 3) +
  geom_line(data = smoothed_data, aes(x = dis, y = pcb_fit, color = legend_model),
            linewidth = 1) +
  scale_color_manual(
    values = c("Observed" = "black",
               "AERMOD Prediction" = "red",
               "log-distance model" = "blue")
  ) +
  xlab(expression(bold("Distance to water (m)"))) +
  ylab(expression(bold("PCB 44+47+65 (pg/m"^3*")"))) +
  annotate("text", x = Inf, y = Inf, label = r2_label, hjust = 1.1, vjust = 2, size = 4) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold", size = 12),
    aspect.ratio = 1)

# See plot
plotpcb444765

