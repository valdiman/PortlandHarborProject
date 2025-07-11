# Emission PCB profile Plot. Figure 3 (C) from paper

# Packages and libraries needed --------------------------------------------
# Install packages
install.packages("ggplot2")

# Libraries
library(ggplot2)

# Read data ---------------------------------------------------------------
# Data file is created in PCBFluxMonteCarlo.R ("R/Flux")
PCBemission <- read.csv("Output/Data/csv/PCBFlux.csv", header = T,
                        check.names = F)

# Create PCB profile plot -------------------------------------------------
PCBemission$Congener <- as.character(PCBemission$Congener)
# Then turn it back into a factor with the levels in the correct order
PCBemission$Congener <- factor(PCBemission$Congener,
                               levels = unique(PCBemission$Congener))
# Change character to numeric for the fluxes
PCBemission[, 3:6] <- lapply(PCBemission[, 3:6], as.numeric)

# Bar plot
p1 <-ggplot(PCBemission, aes(x = Congener, y = `Mean (ng/m2/d)`)) +
  geom_bar(position = position_dodge(), stat = "identity",
           fill = "black") +
  geom_errorbar(aes(ymin = `Mean (ng/m2/d)`,
                    ymax = (`Mean (ng/m2/d)` + `Std (ng/m2/d)`)),
                width = 0.9, position = position_dodge(0.9)) +
  xlab("") +
  ylim(0, 300) +
  theme_classic() +
  theme(aspect.ratio = 4/16) +
  theme(panel.border = element_rect(color =  'black', fill = NA, size = 1.1)) +
  ylab(expression(bold("PCBi Flux (ng/m"^2*"/d)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  annotate("text", x = 4, y = 120, label = "PCB 4", size = 5,
           fontface = 1, angle = 90) +
  annotate("text", x = 11, y = 90, label = "PCB 11", size = 5,
           fontface = 1, angle = 90)+
  annotate("text", x = 18, y = 125, label = "PCB 19",
           size = 5, fontface = 1, angle = 90) +
  annotate("text", x = 37, y = 170, label = "PCBs 44+47+65",
           size = 5, fontface = 1, angle = 90) +
  annotate("text", x = 41, y = 190, label = "PCBs 45+51",
           size = 5, fontface = 1, angle = 90) +
  annotate("text", x = 44, y = 85, label = "PCB 52",
           size = 5, fontface = 1, angle = 90) +
  annotate("text", x = 57, y = 85, label = "PCB 68",
           size = 5, fontface = 1, angle = 90) +
  annotate("text", x = 155, y = 250, label = "(C)", size = 5,
           fontface = 1) 

# See plot
print(p1)

# Save plot in folder
ggsave("Output/Plots/FluxProfFig3C.png", plot = p1, width = 10,
       height = 5, dpi = 500)

