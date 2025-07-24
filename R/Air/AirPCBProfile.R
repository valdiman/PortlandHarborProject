# Air PCB profile Plot. Figure 3 (A) from paper

# Packages and libraries needed --------------------------------------------
# Install packages
install.packages("ggplot2")

# Libraries
library(ggplot2)

# Read data ---------------------------------------------------------------
air.raw <- read.csv("Data/AirConcV2.csv", check.names = FALSE, header = TRUE)
# Remove metadata
air <- air.raw[, 7:179]

# Create Profile ----------------------------------------------------------
# All Air samples
{
  tmp <- rowSums(air, na.rm = TRUE)
  prof <- sweep(air, 1, tmp, FUN = "/")
  # Add back sample site location
  prof.1 <- cbind(sid = air.raw$Event, prof)
  # Save profile data
  write.csv(prof.1, file = "Output/Data/csv/ProfAirV2.csv")
  prof.ave <- data.frame(colMeans(prof, na.rm = TRUE))
  colnames(prof.ave) <- c("mean")
  prof.sd <- data.frame(apply(prof, 2, sd, na.rm = TRUE))
  colnames(prof.sd) <- c("sd")
  congener <- row.names(prof.ave)
  prof.ave <- cbind(congener, prof.ave$mean, prof.sd$sd)
  colnames(prof.ave) <- c("congener", "mean", "sd")
  prof.ave <- data.frame(prof.ave)
  prof.ave$mean <- as.numeric(as.character(prof.ave$mean))
  prof.ave$sd <- as.numeric(as.character(prof.ave$sd))
  prof.ave$congener <- as.character(prof.ave$congener)
  #Then turn it back into a factor with the levels in the correct order
  prof.ave$congener <- factor(prof.ave$congener,
                              levels = unique(prof.ave$congener))
}

# Plot Profile ------------------------------------------------------------
plotavgproferror <- ggplot(prof.ave, aes(x = congener, y = mean)) +
  geom_bar(position = position_dodge(), stat = "identity",
           fill = "black") +
  geom_errorbar(aes(ymin = mean, ymax = (mean+sd)), width = 0.9,
                position = position_dodge(0.9)) +
  xlab("") +
  ylim(0, 0.15) +
  theme_classic() +
  theme(aspect.ratio = 4/16) +
  theme(panel.border = element_rect(color =  'black', fill = NA, size = 1.1)) +
  ylab(expression(bold("Concentration Fraction "*Sigma*"PCB"))) +
  theme(axis.text.y = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank()) +
  annotate("text", x = 4, y = 0.105, label = "PCB 4", size = 5,
           fontface = 1, angle = 90) +
  annotate("text", x = 8, y = 0.115, label = "PCB 8", size = 5,
           fontface = 1, angle = 90) +
  annotate("text", x = 11, y = 0.11, label = "PCB 11", size = 5,
           fontface = 1, angle = 90) +
  annotate("text", x = 17, y = 0.115, label = "PCB 18+30", size = 5,
           fontface = 1, angle = 90) +
  annotate("text", x = 46, y = 0.09, label = "PCB 52", size = 5,
           fontface = 1, angle = 90) +
  annotate("text", x = 168, y = 0.14, label = "(A)", size = 5,
           fontface = 1) 

# See plot
print(plotavgproferror)

# Save plot
ggsave("Output/Plots/AvgAirProfFig3AV2.png", plot = plotavgproferror, width = 10,
       height = 5, dpi = 500)


