# Water PCB profile Plot. Figure 3 (B) from paper

# Packages and libraries needed --------------------------------------------
# Install packages
install.packages("ggplot2")

# Libraries
library(ggplot2)

# Read data ---------------------------------------------------------------
wc.raw <- read.csv("Data/WaterConcentrationV02.csv", header = T)

# Prepare profile ---------------------------------------------------------
# Average of the three highest samples
# This approach was used in the paper to estimate emissions
# and used in AERMOD
# WCPCB_OR-POH003 8/22/18, WCPCB_OR-POH004 8/23/18 &
# WCPCB_OR-POH005 8/21/18
wc.3hi <- wc.raw[c(7, 10, 13), 8:166]

# Create average PCB congener profiles
{
  tmp <- rowSums(wc.3hi, na.rm = TRUE)
  prof <- sweep(wc.3hi, 1, tmp, FUN = "/")
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
  # Save average profile data
  write.csv(prof.ave, file = "Output/Data/csv/AveProf3HWater.csv")
}

# Plot profile ------------------------------------------------------------
waterProfAvg <- ggplot(prof.ave, aes(x = congener, y = mean)) +
  geom_bar(position = position_dodge(), stat = "identity",
           fill = "black") +
  geom_errorbar(aes(ymin = mean, ymax = (mean+sd)), width = 0.9,
                position = position_dodge(0.9)) +
  xlab("") +
  ylim(0, 0.35) +
  theme_classic() +
  theme(aspect.ratio = 4/16) +
  theme(panel.border = element_rect(color =  'black', fill = NA, size = 1.1)) +
  ylab(expression(bold("Concentration Fraction "*Sigma*"PCB"))) +
  theme(axis.text.y = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank()) +
  annotate("text", x = 4, y = 0.16, label = "PCB 4", size = 5,
           fontface = 1, angle = 90) +
  annotate("text", x = 11, y = 0.1, label = "PCB 11", size = 5,
           fontface = 1, angle = 90) +
  annotate("text", x = 18, y = 0.14, label = "PCB 19", size = 5,
           fontface = 1, angle = 90) +
  annotate("text", x = 38, y = 0.2, label = "PCB 44+47+65", size = 5,
           fontface = 1, angle = 90,vjust=-0.1) +
  annotate("text", x = 39, y = 0.27, label = "PCB 45+51", size = 5,
           fontface = 1, angle = 90, vjust=1.2) +
  annotate("text", x = 44, y = 0.09, label = "PCB 52", size = 5,
           fontface = 1, angle = 90) +
  annotate("text", x = 57, y = 0.11, label = "PCB 68", size = 5,
           fontface = 1, angle = 90) +
  annotate("text", x = 83, y = 0.13, label = "PCB 110+115", size = 5,
           fontface = 1, angle = 90) +
  annotate("text", x = 155, y = 0.3, label = "(B)", size = 5,
           fontface = 1) 

print(waterProfAvg)

# Save plot ---------------------------------------------------------------
ggsave("Output/Plots/WaterProfAvgFig3B3Hight.png", plot = waterProfAvg, width = 10,
       height = 5, dpi = 500)



