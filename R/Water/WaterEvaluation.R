# Evaluate Portland water PCB data

# Packages and libraries needed --------------------------------------------
# Install packages
install.packages("ggplot2")
install.packages("ggmap")
install.packages("ggrepel")

# Libraries
{
  library(ggplot2)
  library(ggmap) # make_bbox
  library(ggrepel) #geom_label_repel
}

# Read data ---------------------------------------------------------------
# Read water concentrations
wc.raw <- read.csv("Data/WaterConcentrationV02.csv")
# Select  locations, sampling date, lat, long and PCBs
# PCBs: 4, 11, 20+28, 44+47+65, 45+51, 52, 68 & tPCB
PCB.PO <- data.frame(cbind(wc.raw$LocationID, wc.raw$SampleDate, wc.raw$SampleDate,
                           wc.raw$Latitude, wc.raw$Longitude,
                           wc.raw$PCB4, wc.raw$PCB11, wc.raw$PCB20.28,
                           wc.raw$PCB44.47.65, wc.raw$PCB45.51,
                           wc.raw$PCB52, wc.raw$PCB68,
                           rowSums(wc.raw[, c(8:166)], na.rm = TRUE)))
# Name the columns
colnames(PCB.PO) <- c("Site", "SampleDate", "SampleDate2", "Latitude", "Longitude",
                      "PCB4", "PCB11", "PCB20+28", "PCB44+47+65",
                      "PCB45+51", "PCB52", "PCB68","tPCB")
# Change to numeric
PCB.PO[, 4:13] <- lapply(PCB.PO[, 4:13], as.numeric)
# Change date format
PCB.PO$SampleDate <- as.Date(PCB.PO$SampleDate, format = "%m/%d/%y")
PCB.PO$SampleDate2 <- as.Date(PCB.PO$SampleDate2, format = "%m/%d/%y")
# Create date of only month & year
PCB.PO$SampleDate2 <- format(PCB.PO$SampleDate2, "%Y-%m")

# Maps --------------------------------------------------------------------
# Map with ggmap
# Create a square map around samples
PO.box <- make_bbox(lon = PCB.PO$Longitude, lat = PCB.PO$Latitude,
                    f = 0.8)
PO.map <- get_stamenmap(bbox = PO.box, zoom = 10)
# Average PCB per site
pcb4.av <- aggregate(PCB4 ~ Site + Latitude + Longitude,
                     data = PCB.PO, FUN = mean)
pcb11.av <- aggregate(PCB11 ~ Site, data = PCB.PO, FUN = mean)
pcb20.28.av <- aggregate(`PCB20+28` ~ Site, data = PCB.PO, FUN = mean)
pcb44.47.65.av <- aggregate(`PCB44+47+65` ~ Site, data = PCB.PO,
                            FUN = mean)
pcb45.51.av <- aggregate(`PCB45+51` ~ Site, data = PCB.PO, FUN = mean)
pcb52.av <- aggregate(PCB52 ~ Site, data = PCB.PO, FUN = mean)
pcb68.av <- aggregate(PCB68 ~ Site, data = PCB.PO, FUN = mean)
tPCB.av <- aggregate(tPCB ~ Site, data = PCB.PO, FUN = mean)
# Combine mean in one data frame
PCB.PO.mean <- cbind(pcb4.av, pcb11.av$PCB11, pcb20.28.av$`PCB20+28`,
                     pcb44.47.65.av$`PCB44+47+65`, pcb45.51.av$`PCB45+51`,
                     pcb52.av$PCB52, pcb68.av$PCB68, tPCB.av$tPCB)
# Name the columns
colnames(PCB.PO.mean) <- c("Site", "Latitude", "Longitude", "PCB4.ave",
                           "PCB11.ave", "PCB20+28.ave", "PCB44+47+65.ave",
                           "PCB45+51.ave", "PCB52.ave", "PCB68.ave",
                           "tPCB.ave")
# (1) Map locations
ggmap(PO.map) +
  geom_point(data = PCB.PO.mean, aes(x = Longitude, y = Latitude),
             shape = 21, color = "red",
             fill = "white", size = 1.75, stroke = 0.75) +
  geom_label_repel(aes(x = Longitude, y = Latitude, label = Site),
                   data = PCB.PO.mean, family = 'Times', size = 1, 
                   box.padding = 0.2, point.padding = 0.3,
                   segment.color = 'grey50')
# (2) Map w/average of PCB concentration
# PCB4
ggmap(PO.map) +
  geom_point(data = PCB.PO.mean, aes(x = Longitude, y = Latitude,
                                     size = PCB4.ave), alpha = 1,
             color  = "red") +
  scale_size_area(name = "Ave. PCB4 \n2018-2019 (pg/L)") +
  xlab("Longitude") +
  ylab("Latitude")
# PCB11
ggmap(PO.map) +
  geom_point(data = PCB.PO.mean, aes(x = Longitude, y = Latitude,
                                     size = PCB11.ave), alpha = 1,
             color  = "red") +
  scale_size_area(name = "Ave. PCB11 \n2018-2019 (pg/L)") +
  xlab("Longitude") +
  ylab("Latitude")
# PCB20+28
ggmap(PO.map) +
  geom_point(data = PCB.PO.mean, aes(x = Longitude, y = Latitude,
                                     size = `PCB20+28.ave`), alpha = 1,
             color  = "red") +
  scale_size_area(name = "Ave. PCB20+28 \n2018-2019 (pg/L)") +
  xlab("Longitude") +
  ylab("Latitude")
# PCB44+47+65
ggmap(PO.map) +
  geom_point(data = PCB.PO.mean, aes(x = Longitude, y = Latitude,
                                     size = `PCB44+47+65.ave`), alpha = 1,
             color  = "red") +
  scale_size_area(name = "Ave. PCB44+47+65 \n2018-2019 (pg/L)") +
  xlab("Longitude") +
  ylab("Latitude")
# PCB45+51
ggmap(PO.map) +
  geom_point(data = PCB.PO.mean, aes(x = Longitude, y = Latitude,
                                     size = `PCB45+51.ave`), alpha = 1,
             color  = "red") +
  scale_size_area(name = "Ave. PCB45+51 \n2018-2019 (pg/L)") +
  xlab("Longitude") +
  ylab("Latitude")
# PCB52
ggmap(PO.map) +
  geom_point(data = PCB.PO.mean, aes(x = Longitude, y = Latitude,
                                     size = PCB52.ave), alpha = 1,
             color  = "red") +
  scale_size_area(name = "Ave. PCB52 \n2018-2019 (pg/L)") +
  xlab("Longitude") +
  ylab("Latitude")
# PCB68
ggmap(PO.map) +
  geom_point(data = PCB.PO.mean, aes(x = Longitude, y = Latitude,
                                     size = PCB68.ave), alpha = 1,
             color  = "red") +
  scale_size_area(name = "Ave. PCB68 \n2018-2019 (pg/L)") +
  xlab("Longitude") +
  ylab("Latitude")
# tPCB
ggmap(PO.map) +
  geom_point(data = PCB.PO.mean, aes(x = Longitude, y = Latitude,
                                     size = tPCB.ave), alpha = 1,
             color  = "red") +
  scale_size_area(name = "Ave. Total PCBs (n = 3)\n2018-2019 (pg/L)") +
  xlab("Longitude") +
  ylab("Latitude")

# (2) Map w/average of PCB %
# PCB4
ggmap(PO.map) +
  geom_point(data = PCB.PO.mean, aes(x = Longitude, y = Latitude,
                                     size = PCB4.ave/tPCB.ave), alpha = 1,
             color  = "red") +
  scale_size_area(name = "Ave. PCB4 \n2018-2019 (%)") +
  xlab("Longitude") +
  ylab("Latitude")
# PCB11
ggmap(PO.map) +
  geom_point(data = PCB.PO.mean, aes(x = Longitude, y = Latitude,
                                     size = PCB11.ave/tPCB.ave), alpha = 1,
             color  = "red") +
  scale_size_area(name = "Ave. PCB11 \n2018-2019 (%)") +
  xlab("Longitude") +
  ylab("Latitude")
# PCB20+28
ggmap(PO.map) +
  geom_point(data = PCB.PO.mean, aes(x = Longitude, y = Latitude,
                                     size = `PCB20+28.ave`/tPCB.ave), alpha = 1,
             color  = "red") +
  scale_size_area(name = "Ave. PCB20+28 \n2018-2019 (%)") +
  xlab("Longitude") +
  ylab("Latitude")
# PCB44+47+65
ggmap(PO.map) +
  geom_point(data = PCB.PO.mean, aes(x = Longitude, y = Latitude,
                                     size = `PCB44+47+65.ave`/tPCB.ave), alpha = 1,
             color  = "red") +
  scale_size_area(name = "Ave. PCB44+47+65 \n2018-2019 (%)") +
  xlab("Longitude") +
  ylab("Latitude")
# PCB45+51
ggmap(PO.map) +
  geom_point(data = PCB.PO.mean, aes(x = Longitude, y = Latitude,
                                     size = `PCB45+51.ave`/tPCB.ave), alpha = 1,
             color  = "red") +
  scale_size_area(name = "Ave. PCB45+51 \n2018-2019 (%)") +
  xlab("Longitude") +
  ylab("Latitude")
# PCB52
ggmap(PO.map) +
  geom_point(data = PCB.PO.mean, aes(x = Longitude, y = Latitude,
                                     size = PCB52.ave/tPCB.ave), alpha = 1,
             color  = "red") +
  scale_size_area(name = "Ave. PCB52 \n2018-2019 (%)") +
  xlab("Longitude") +
  ylab("Latitude")
# PCB68
ggmap(PO.map) +
  geom_point(data = PCB.PO.mean, aes(x = Longitude, y = Latitude,
                                     size = PCB68.ave/tPCB.ave), alpha = 1,
             color  = "red") +
  scale_size_area(name = "Ave. PCB68 \n2018-2019 (%)") +
  xlab("Longitude") +
  ylab("Latitude")

# Spatial plot ------------------------------------------------------------
# Concentration
# PCB4
ggplot(PCB.PO, aes(x = factor(Site), y = PCB4)) + 
  geom_point() +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCB 4 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB11
ggplot(PCB.PO, aes(x = factor(Site), y = PCB11)) + 
  geom_point() +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCB 11 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB20+28
ggplot(PCB.PO, aes(x = factor(Site), y = `PCB20+28`)) + 
  geom_point() +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCBs 20+28 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB44+47+65
ggplot(PCB.PO, aes(x = factor(Site), y = `PCB44+47+65`)) + 
  geom_point() +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCBs 44+47+65 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB45+51
ggplot(PCB.PO, aes(x = factor(Site), y = `PCB45+51`)) + 
  geom_point() +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCBs 45+51 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB52
ggplot(PCB.PO, aes(x = factor(Site), y = PCB52)) + 
  geom_point() +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCB 52 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB68
ggplot(PCB.PO, aes(x = factor(Site), y = PCB68)) + 
  geom_point() +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCB 68 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# tPCB
ggplot(PCB.PO, aes(x = factor(Site), y = tPCB)) + 
  geom_point() +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold(Sigma*"PCB (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))

# Include dates
# PCB4
ggplot(PCB.PO, aes(x = Site, y = log10(PCB4), shape = SampleDate2)) + 
  geom_point(aes(color = SampleDate2), shape = 1, size = 2) +
  labs(color = "Date") +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("log10 PCB 4 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB11
ggplot(PCB.PO, aes(x = Site, y = log10(PCB11), group = SampleDate2)) + 
  geom_point(aes(color = SampleDate2), shape = 1, size  = 2) +
  labs(color = "Date") +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("log10PCB 11 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB20+28
ggplot(PCB.PO, aes(x = Site, y = log10(`PCB20+28`), group = SampleDate2)) + 
  geom_point(aes(color = SampleDate2), shape = 1, size  = 2) +
  labs(color = "Date") +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("log10PCBs 20+28 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB44+47+65
ggplot(PCB.PO, aes(x = Site, y = log10(`PCB44+47+65`), group = SampleDate2)) + 
  geom_point(aes(color = SampleDate2), shape = 1, size  = 2) +
  labs(color = "Date") +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("log10PCBs 44+47+65 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB45+51
ggplot(PCB.PO, aes(x = Site, y = log10(`PCB45+51`), group = SampleDate2)) + 
  geom_point(aes(color = SampleDate2), shape = 1, size  = 2) +
  labs(color = "Date") +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("log10PCBs 45+51 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB52
ggplot(PCB.PO, aes(x = Site, y = log10(PCB52), group = SampleDate2)) + 
  geom_point(aes(color = SampleDate2), shape = 1, size  = 2) +
  labs(color = "Date") +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("log10PCB 52 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB68
ggplot(PCB.PO, aes(x = Site, y = log10(PCB68), group = SampleDate2)) + 
  geom_point(aes(color = SampleDate2), shape = 1, size  = 2) +
  labs(color = "Date") +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("log10PCB 68 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# tPCB
ggplot(PCB.PO, aes(x = Site, y = tPCB, group = SampleDate2)) + 
  geom_point(aes(color = SampleDate2), shape = 1, size  = 2) +
  labs(color = "Date") +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold(Sigma*"PCB (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))

# %
# PCB4
ggplot(PCB.PO, aes(x = Site, y = PCB4/tPCB)) + 
  geom_point() +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCB 4 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB11
ggplot(PCB.PO, aes(x = Site, y = PCB11/tPCB)) + 
  geom_point() +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCB 11 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB20+28
ggplot(PCB.PO, aes(x = Site, y = `PCB20+28`/tPCB)) + 
  geom_point() +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCBs 20+28 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB44+47+65
ggplot(PCB.PO, aes(x = Site, y = `PCB44+47+65`/tPCB)) + 
  geom_point() +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCBs 44+47+65 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB45+51
ggplot(PCB.PO, aes(x = Site, y = `PCB45+51`/tPCB)) + 
  geom_point() +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCBs 45+51 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB52
ggplot(PCB.PO, aes(x = Site, y = PCB52/tPCB)) + 
  geom_point() +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCB 52 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB68
ggplot(PCB.PO, aes(x = Site, y = PCB68/tPCB)) + 
  geom_point() +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCB 68 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))

# Include dates
# PCB4
ggplot(PCB.PO, aes(x = Site, y = PCB4/tPCB, shape = SampleDate2)) + 
  geom_point(aes(color = SampleDate2), shape = 1, size = 2) +
  labs(color = "Date") +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCB 4 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB11
ggplot(PCB.PO, aes(x = Site, y = PCB11/tPCB, shape = SampleDate2)) + 
  geom_point(aes(color = SampleDate2), shape = 1, size = 2) +
  labs(color = "Date") +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCB 11 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB20+28
ggplot(PCB.PO, aes(x = Site, y = `PCB20+28`/tPCB, shape = SampleDate2)) + 
  geom_point(aes(color = SampleDate2), shape = 1, size = 2) +
  labs(color = "Date") +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCBs 20+28 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB44+47+65
ggplot(PCB.PO, aes(x = Site, y = `PCB44+47+65`/tPCB, shape = SampleDate2)) + 
  geom_point(aes(color = SampleDate2), shape = 1, size = 2) +
  labs(color = "Date") +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCBs 44+47+65 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB45+51
ggplot(PCB.PO, aes(x = Site, y = `PCB45+51`/tPCB, shape = SampleDate2)) + 
  geom_point(aes(color = SampleDate2), shape = 1, size = 2) +
  labs(color = "Date") +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCBs 45+51 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB52
ggplot(PCB.PO, aes(x = Site, y = PCB52/tPCB, shape = SampleDate2)) + 
  geom_point(aes(color = SampleDate2), shape = 1, size = 2) +
  labs(color = "Date") +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCB 52 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB68
ggplot(PCB.PO, aes(x = Site, y = PCB68/tPCB, shape = SampleDate2)) + 
  geom_point(aes(color = SampleDate2), shape = 1, size = 2) +
  labs(color = "Date") +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold("PCB 68 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))

# Temporal plots ----------------------------------------------------------
# Concentration
# PCB4
ggplot(PCB.PO, aes(x = format(SampleDate,'%Y%m%d'), y = PCB4)) +
  geom_point() +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 10/15) +
  ylab(expression(bold("PCB 4 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB11
ggplot(PCB.PO, aes(x = format(SampleDate,'%Y%m%d'), y = PCB11)) +
  geom_point() +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 10/15) +
  ylab(expression(bold("PCB 11 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB20+28
ggplot(PCB.PO, aes(x = format(SampleDate,'%Y%m%d'), y = `PCB20+28`)) +
  geom_point() +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 10/15) +
  ylab(expression(bold("PCBs 20+28 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB44+47+65
ggplot(PCB.PO, aes(x = format(SampleDate,'%Y%m%d'), y = `PCB44+47+65`)) +
  geom_point() +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 10/15) +
  ylab(expression(bold("PCBs PCB 44+47+65 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB45+51
ggplot(PCB.PO, aes(x = format(SampleDate,'%Y%m%d'), y = `PCB45+51`)) +
  geom_point() +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 10/15) +
  ylab(expression(bold("PCBs 45+51 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB52
ggplot(PCB.PO, aes(x = format(SampleDate,'%Y%m%d'), y = PCB52)) +
  geom_point() +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 10/15) +
  ylab(expression(bold("PCB 52 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB68
ggplot(PCB.PO, aes(x = format(SampleDate,'%Y%m%d'), y = PCB68)) +
  geom_point() +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 10/15) +
  ylab(expression(bold("PCB 68 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# tPCB
ggplot(PCB.PO, aes(x = format(SampleDate,'%Y%m%d'), y = tPCB)) +
  geom_point() +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 10/15) +
  ylab(expression(bold(Sigma*"PCB (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))

# %
# PCB4
ggplot(PCB.PO, aes(x = format(SampleDate,'%Y%m%d'), y = PCB4/tPCB)) +
  geom_point() +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 10/15) +
  ylab(expression(bold("PCB 4 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB11
ggplot(PCB.PO, aes(x = format(SampleDate,'%Y%m%d'), y = PCB11/tPCB)) +
  geom_point() +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 10/15) +
  ylab(expression(bold("PCB 11 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB20+28
ggplot(PCB.PO, aes(x = format(SampleDate,'%Y%m%d'), y = `PCB20+28`/tPCB)) +
  geom_point() +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 10/15) +
  ylab(expression(bold("PCBs 20+28 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB44+47+65
ggplot(PCB.PO, aes(x = format(SampleDate,'%Y%m%d'), y = `PCB44+47+65`/tPCB)) +
  geom_point() +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 10/15) +
  ylab(expression(bold("PCBs PCB 44+47+65 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB45+51
ggplot(PCB.PO, aes(x = format(SampleDate,'%Y%m%d'), y = `PCB45+51`/tPCB)) +
  geom_point() +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 10/15) +
  ylab(expression(bold("PCBs 45+51 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB52
ggplot(PCB.PO, aes(x = format(SampleDate,'%Y%m%d'), y = PCB52/tPCB)) +
  geom_point() +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 10/15) +
  ylab(expression(bold("PCB 52 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))
# PCB68
ggplot(PCB.PO, aes(x = format(SampleDate,'%Y%m%d'), y = PCB68/tPCB)) +
  geom_point() +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 10/15) +
  ylab(expression(bold("PCB 68 (%)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))

# PCB profiles plot -------------------------------------------------------
# Create average PCB congener profiles
# (1) All samples
{
  wc.1 <- subset(wc.raw, select = -c(SampleID:Units))
  tmp <- rowSums(wc.1, na.rm = TRUE)
  prof <- sweep(wc.1, 1, tmp, FUN = "/")
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

# PCB Profile plot
ggplot(prof.ave, aes(x = congener, y = mean)) +
  geom_bar(position = position_dodge(), stat = "identity",
           fill = "black") +
  geom_errorbar(aes(ymin = mean, ymax = (mean+sd)), width = 0.9,
                position = position_dodge(0.9)) +
  xlab("") +
  ylim(0, 0.6) +
  theme_bw() +
  theme(aspect.ratio = 4/16) +
  ylab(expression(bold("Mass fraction "*Sigma*"PCB"))) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  annotate("text", x = 4, y = 0.17, label = "PCB 4", size = 3,
           fontface = 1, angle = 90) +
  annotate("text", x = 11, y = 0.22, label = "PCB 11", size = 3,
           fontface = 1, angle = 90) +
  annotate("text", x = 19, y = 0.24, label = "PCBs 20+28",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 36.2, y = 0.3, label = "PCBs 44+47+65",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 41.3, y = 0.4, label = "PCBs 45+51",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 44.3, y = 0.17, label = "PCB 52",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 57, y = 0.23, label = "PCB 68",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 138, y = 0.53, label = "All samples (n = 21)",
           size = 3.5, fontface = 1)

# (2) Specific site
wc.1 <- subset(wc.raw, select = -c(SampleID:Units))
wc.2 <- cbind(wc.raw$LocationID, wc.1)
colnames(wc.2)[1] <- "LocationID"
# Selected site
{
  wc.POH001 <- wc.2[wc.2$LocationID == 'WCPCB_OR-POH001', ]
  wc.POH002 <- wc.2[wc.2$LocationID == 'WCPCB_OR-POH002', ]
  wc.POH003 <- wc.2[wc.2$LocationID == 'WCPCB_OR-POH003', ]
  wc.POH004 <- wc.2[wc.2$LocationID == 'WCPCB_OR-POH004', ]
  wc.POH005 <- wc.2[wc.2$LocationID == 'WCPCB_OR-POH005', ]
  wc.POH006 <- wc.2[wc.2$LocationID == 'WCPCB_OR-POH006', ]
  wc.POH007 <- wc.2[wc.2$LocationID == 'WCPCB_OR-POH007', ]
}
# Calculate profile for each site
# WCPCB_OR-POH001
{
  tmp <- rowSums(wc.POH001[, 2:160], na.rm = TRUE)
  prof <- sweep(wc.POH001[, 2:160], 1, tmp, FUN = "/")
  prof.POH001.ave <- data.frame(colMeans(prof, na.rm = TRUE))
  colnames(prof.POH001.ave) <- c("mean")
  prof.POH001.sd <- data.frame(apply(prof, 2, sd, na.rm = TRUE))
  colnames(prof.POH001.sd) <- c("sd")
  congener <- row.names(prof.POH001.ave)
  prof.POH001.ave <- cbind(congener, prof.POH001.ave$mean,
                           prof.POH001.sd$sd)
  colnames(prof.POH001.ave) <- c("congener", "mean", "sd")
  prof.POH001.ave <- data.frame(prof.POH001.ave)
  prof.POH001.ave$mean <- as.numeric(as.character(prof.POH001.ave$mean))
  prof.POH001.ave$sd <- as.numeric(as.character(prof.POH001.ave$sd))
  prof.POH001.ave$congener <- as.character(prof.POH001.ave$congener)
  #Then turn it back into a factor with the levels in the correct order
  prof.POH001.ave$congener <- factor(prof.POH001.ave$congener,
                                     levels = unique(prof.POH001.ave$congener))
}

# PCB profile plot
ggplot(prof.POH001.ave, aes(x = congener, y = mean)) +
  geom_bar(position = position_dodge(), stat = "identity",
           fill = "black") +
  geom_errorbar(aes(ymin = mean, ymax = (mean+sd)), width = 0.9,
                position = position_dodge(0.9)) +
  xlab("") +
  ylim(0, 0.60) +
  theme_bw() +
  theme(aspect.ratio = 4/16) +
  ylab(expression(bold("Mass fraction "*Sigma*"PCB"))) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  annotate("text", x = 4, y = 0.17, label = "PCB 4", size = 3,
           fontface = 1, angle = 90) +
  annotate("text", x = 11, y = 0.22, label = "PCB 11", size = 3,
           fontface = 1, angle = 90) +
  annotate("text", x = 19, y = 0.24, label = "PCBs 20+28",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 36.2, y = 0.3, label = "PCBs 44+47+65",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 41.3, y = 0.4, label = "PCBs 45+51",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 44.3, y = 0.17, label = "PCB 52",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 57, y = 0.23, label = "PCB 68",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 150, y = 0.53, label = "POH001",
           size = 3.5, fontface = 1)

# WCPCB_OR-POH002
{
  tmp <- rowSums(wc.POH002[, 2:160], na.rm = TRUE)
  prof <- sweep(wc.POH002[, 2:160], 1, tmp, FUN = "/")
  prof.POH002.ave <- data.frame(colMeans(prof, na.rm = TRUE))
  colnames(prof.POH002.ave) <- c("mean")
  prof.POH002.sd <- data.frame(apply(prof, 2, sd, na.rm = TRUE))
  colnames(prof.POH002.sd) <- c("sd")
  congener <- row.names(prof.POH002.ave)
  prof.POH002.ave <- cbind(congener, prof.POH002.ave$mean,
                           prof.POH002.sd$sd)
  colnames(prof.POH002.ave) <- c("congener", "mean", "sd")
  prof.POH002.ave <- data.frame(prof.POH002.ave)
  prof.POH002.ave$mean <- as.numeric(as.character(prof.POH002.ave$mean))
  prof.POH002.ave$sd <- as.numeric(as.character(prof.POH002.ave$sd))
  prof.POH002.ave$congener <- as.character(prof.POH002.ave$congener)
  #Then turn it back into a factor with the levels in the correct order
  prof.POH002.ave$congener <- factor(prof.POH002.ave$congener,
                                     levels = unique(prof.POH002.ave$congener))
  }

# PCB profile plot
ggplot(prof.POH002.ave, aes(x = congener, y = mean)) +
  geom_bar(position = position_dodge(), stat = "identity",
           fill = "black") +
  geom_errorbar(aes(ymin = mean, ymax = (mean+sd)), width = 0.9,
                position = position_dodge(0.9)) +
  xlab("") +
  ylim(0, 0.60) +
  theme_bw() +
  theme(aspect.ratio = 4/16) +
  ylab(expression(bold("Mass fraction "*Sigma*"PCB"))) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  annotate("text", x = 4, y = 0.17, label = "PCB 4", size = 3,
           fontface = 1, angle = 90) +
  annotate("text", x = 11, y = 0.22, label = "PCB 11", size = 3,
           fontface = 1, angle = 90) +
  annotate("text", x = 19, y = 0.24, label = "PCBs 20+28",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 36.2, y = 0.3, label = "PCBs 44+47+65",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 41.3, y = 0.4, label = "PCBs 45+51",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 44.3, y = 0.19, label = "PCB 52",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 57, y = 0.2, label = "PCB 68",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 150, y = 0.53, label = "POH002",
           size = 3.5, fontface = 1)

# WCPCB_OR-POH003
{
  tmp <- rowSums(wc.POH003[, 2:160], na.rm = TRUE)
  prof <- sweep(wc.POH003[, 2:160], 1, tmp, FUN = "/")
  prof.POH003.ave <- data.frame(colMeans(prof, na.rm = TRUE))
  colnames(prof.POH003.ave) <- c("mean")
  prof.POH003.sd <- data.frame(apply(prof, 2, sd, na.rm = TRUE))
  colnames(prof.POH003.sd) <- c("sd")
  congener <- row.names(prof.POH003.ave)
  prof.POH003.ave <- cbind(congener, prof.POH003.ave$mean,
                           prof.POH003.sd$sd)
  colnames(prof.POH003.ave) <- c("congener", "mean", "sd")
  prof.POH003.ave <- data.frame(prof.POH003.ave)
  prof.POH003.ave$mean <- as.numeric(as.character(prof.POH003.ave$mean))
  prof.POH003.ave$sd <- as.numeric(as.character(prof.POH003.ave$sd))
  prof.POH003.ave$congener <- as.character(prof.POH003.ave$congener)
  #Then turn it back into a factor with the levels in the correct order
  prof.POH003.ave$congener <- factor(prof.POH003.ave$congener,
                                     levels = unique(prof.POH003.ave$congener))
  }

# PCB profile plot
ggplot(prof.POH003.ave, aes(x = congener, y = mean)) +
  geom_bar(position = position_dodge(), stat = "identity",
           fill = "black") +
  geom_errorbar(aes(ymin = mean, ymax = (mean+sd)), width = 0.9,
                position = position_dodge(0.9)) +
  xlab("") +
  ylim(0, 0.6) +
  theme_bw() +
  theme(aspect.ratio = 4/16) +
  ylab(expression(bold("Mass fraction "*Sigma*"PCB"))) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  annotate("text", x = 4, y = 0.17, label = "PCB 4", size = 3,
           fontface = 1, angle = 90) +
  annotate("text", x = 11, y = 0.22, label = "PCB 11", size = 3,
           fontface = 1, angle = 90) +
  annotate("text", x = 19, y = 0.24, label = "PCBs 20+28",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 36.2, y = 0.3, label = "PCBs 44+47+65",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 41.3, y = 0.4, label = "PCBs 45+51",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 44.3, y = 0.19, label = "PCB 52",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 57, y = 0.23, label = "PCB 68",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 150, y = 0.53, label = "POH003",
           size = 3.5, fontface = 1)

# WCPCB_OR-POH004
{
  tmp <- rowSums(wc.POH004[, 2:160], na.rm = TRUE)
  prof <- sweep(wc.POH004[, 2:160], 1, tmp, FUN = "/")
  prof.POH004.ave <- data.frame(colMeans(prof, na.rm = TRUE))
  colnames(prof.POH004.ave) <- c("mean")
  prof.POH004.sd <- data.frame(apply(prof, 2, sd, na.rm = TRUE))
  colnames(prof.POH004.sd) <- c("sd")
  congener <- row.names(prof.POH004.ave)
  prof.POH004.ave <- cbind(congener, prof.POH004.ave$mean,
                           prof.POH004.sd$sd)
  colnames(prof.POH004.ave) <- c("congener", "mean", "sd")
  prof.POH004.ave <- data.frame(prof.POH004.ave)
  prof.POH004.ave$mean <- as.numeric(as.character(prof.POH004.ave$mean))
  prof.POH004.ave$sd <- as.numeric(as.character(prof.POH004.ave$sd))
  prof.POH004.ave$congener <- as.character(prof.POH004.ave$congener)
  #Then turn it back into a factor with the levels in the correct order
  prof.POH004.ave$congener <- factor(prof.POH001.ave$congener,
                                     levels = unique(prof.POH004.ave$congener))
  }

# PCB profile plot
ggplot(prof.POH004.ave, aes(x = congener, y = mean)) +
  geom_bar(position = position_dodge(), stat = "identity",
           fill = "black") +
  geom_errorbar(aes(ymin = mean, ymax = (mean+sd)), width = 0.9,
                position = position_dodge(0.9)) +
  xlab("") +
  ylim(0, 0.60) +
  theme_bw() +
  theme(aspect.ratio = 4/16) +
  ylab(expression(bold("Mass fraction "*Sigma*"PCB"))) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  annotate("text", x = 4, y = 0.17, label = "PCB 4", size = 3,
           fontface = 1, angle = 90) +
  annotate("text", x = 11, y = 0.22, label = "PCB 11", size = 3,
           fontface = 1, angle = 90) +
  annotate("text", x = 19, y = 0.24, label = "PCBs 20+28",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 36.2, y = 0.3, label = "PCBs 44+47+65",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 41.3, y = 0.4, label = "PCBs 45+51",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 44.3, y = 0.17, label = "PCB 52",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 57, y = 0.2, label = "PCB 68",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 150, y = 0.53, label = "POH004",
           size = 3.5, fontface = 1)

# WCPCB_OR-POH005
{
  tmp <- rowSums(wc.POH005[, 2:160], na.rm = TRUE)
  prof <- sweep(wc.POH005[, 2:160], 1, tmp, FUN = "/")
  prof.POH005.ave <- data.frame(colMeans(prof, na.rm = TRUE))
  colnames(prof.POH005.ave) <- c("mean")
  prof.POH005.sd <- data.frame(apply(prof, 2, sd, na.rm = TRUE))
  colnames(prof.POH005.sd) <- c("sd")
  congener <- row.names(prof.POH005.ave)
  prof.POH005.ave <- cbind(congener, prof.POH005.ave$mean,
                           prof.POH005.sd$sd)
  colnames(prof.POH005.ave) <- c("congener", "mean", "sd")
  prof.POH005.ave <- data.frame(prof.POH005.ave)
  prof.POH005.ave$mean <- as.numeric(as.character(prof.POH005.ave$mean))
  prof.POH005.ave$sd <- as.numeric(as.character(prof.POH005.ave$sd))
  prof.POH005.ave$congener <- as.character(prof.POH005.ave$congener)
  #Then turn it back into a factor with the levels in the correct order
  prof.POH005.ave$congener <- factor(prof.POH001.ave$congener,
                                     levels = unique(prof.POH005.ave$congener))
  }

# PCB profile plot
ggplot(prof.POH005.ave, aes(x = congener, y = mean)) +
  geom_bar(position = position_dodge(), stat = "identity",
           fill = "black") +
  geom_errorbar(aes(ymin = mean, ymax = (mean+sd)), width = 0.9,
                position = position_dodge(0.9)) +
  xlab("") +
  ylim(0, 0.60) +
  theme_bw() +
  theme(aspect.ratio = 4/16) +
  ylab(expression(bold("Mass fraction "*Sigma*"PCB"))) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  annotate("text", x = 4, y = 0.22, label = "PCB 4", size = 3,
           fontface = 1, angle = 90) +
  annotate("text", x = 11, y = 0.22, label = "PCB 11", size = 3,
           fontface = 1, angle = 90) +
  annotate("text", x = 19, y = 0.26, label = "PCBs 20+28",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 36.2, y = 0.3, label = "PCBs 44+47+65",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 41.3, y = 0.4, label = "PCBs 45+51",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 44.3, y = 0.16, label = "PCB 52",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 57, y = 0.23, label = "PCB 68",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 150, y = 0.53, label = "POH005",
           size = 3.5, fontface = 1)

# WCPCB_OR-POH006
{
  tmp <- rowSums(wc.POH006[, 2:160], na.rm = TRUE)
  prof <- sweep(wc.POH006[, 2:160], 1, tmp, FUN = "/")
  prof.POH006.ave <- data.frame(colMeans(prof, na.rm = TRUE))
  colnames(prof.POH006.ave) <- c("mean")
  prof.POH006.sd <- data.frame(apply(prof, 2, sd, na.rm = TRUE))
  colnames(prof.POH006.sd) <- c("sd")
  congener <- row.names(prof.POH006.ave)
  prof.POH006.ave <- cbind(congener, prof.POH006.ave$mean,
                           prof.POH006.sd$sd)
  colnames(prof.POH006.ave) <- c("congener", "mean", "sd")
  prof.POH006.ave <- data.frame(prof.POH006.ave)
  prof.POH006.ave$mean <- as.numeric(as.character(prof.POH006.ave$mean))
  prof.POH006.ave$sd <- as.numeric(as.character(prof.POH006.ave$sd))
  prof.POH006.ave$congener <- as.character(prof.POH006.ave$congener)
  #Then turn it back into a factor with the levels in the correct order
  prof.POH006.ave$congener <- factor(prof.POH006.ave$congener,
                                     levels = unique(prof.POH006.ave$congener))
  }

# PCB profile plot
ggplot(prof.POH006.ave, aes(x = congener, y = mean)) +
  geom_bar(position = position_dodge(), stat = "identity",
           fill = "black") +
  geom_errorbar(aes(ymin = mean, ymax = (mean+sd)), width = 0.9,
                position = position_dodge(0.9)) +
  xlab("") +
  ylim(0, 0.60) +
  theme_bw() +
  theme(aspect.ratio = 4/16) +
  ylab(expression(bold("Mass fraction "*Sigma*"PCB"))) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  annotate("text", x = 4, y = 0.13, label = "PCB 4", size = 3,
           fontface = 1, angle = 90) +
  annotate("text", x = 11, y = 0.26, label = "PCB 11", size = 3,
           fontface = 1, angle = 90) +
  annotate("text", x = 19, y = 0.24, label = "PCBs 20+28",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 36.2, y = 0.3, label = "PCBs 44+47+65",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 41.3, y = 0.4, label = "PCBs 45+51",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 44.3, y = 0.16, label = "PCB 52",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 57, y = 0.23, label = "PCB 68",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 150, y = 0.53, label = "POH006",
           size = 3.5, fontface = 1)

# WCPCB_OR-POH007
{
  tmp <- rowSums(wc.POH007[, 2:160], na.rm = TRUE)
  prof <- sweep(wc.POH007[, 2:160], 1, tmp, FUN = "/")
  prof.POH007.ave <- data.frame(colMeans(prof, na.rm = TRUE))
  colnames(prof.POH007.ave) <- c("mean")
  prof.POH007.sd <- data.frame(apply(prof, 2, sd, na.rm = TRUE))
  colnames(prof.POH007.sd) <- c("sd")
  congener <- row.names(prof.POH007.ave)
  prof.POH007.ave <- cbind(congener, prof.POH007.ave$mean,
                           prof.POH007.sd$sd)
  colnames(prof.POH007.ave) <- c("congener", "mean", "sd")
  prof.POH007.ave <- data.frame(prof.POH007.ave)
  prof.POH007.ave$mean <- as.numeric(as.character(prof.POH007.ave$mean))
  prof.POH007.ave$sd <- as.numeric(as.character(prof.POH007.ave$sd))
  prof.POH007.ave$congener <- as.character(prof.POH007.ave$congener)
  #Then turn it back into a factor with the levels in the correct order
  prof.POH007.ave$congener <- factor(prof.POH007.ave$congener,
                                     levels = unique(prof.POH007.ave$congener))
  }

# PCB profile plot
ggplot(prof.POH007.ave, aes(x = congener, y = mean)) +
  geom_bar(position = position_dodge(), stat = "identity",
           fill = "black") +
  geom_errorbar(aes(ymin = mean, ymax = (mean+sd)), width = 0.9,
                position = position_dodge(0.9)) +
  xlab("") +
  ylim(0, 0.60) +
  theme_bw() +
  theme(aspect.ratio = 4/16) +
  ylab(expression(bold("Mass fraction "*Sigma*"PCB"))) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  annotate("text", x = 4, y = 0.13, label = "PCB 4", size = 3,
           fontface = 1, angle = 90) +
  annotate("text", x = 11, y = 0.22, label = "PCB 11", size = 3,
           fontface = 1, angle = 90) +
  annotate("text", x = 19, y = 0.24, label = "PCBs 20+28",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 36.2, y = 0.3, label = "PCBs 44+47+65",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 41.3, y = 0.4, label = "PCBs 45+51",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 44.3, y = 0.17, label = "PCB 52",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 57, y = 0.23, label = "PCB 68",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 150, y = 0.53, label = "POH007",
           size = 3.5, fontface = 1)

# (3) Average of the three highest samples
# WCPCB_OR-POH003 8/22/18, WCPCB_OR-POH004 8/21/18 &
# WCPCB_OR-POH005 8/23/18
{
  wc.hi <- wc.2[c(7, 10, 13), 2:160]
  tmp <- rowSums(wc.hi, na.rm = TRUE)
  prof <- sweep(wc.hi, 1, tmp, FUN = "/")
  prof.wc.hi.ave <- data.frame(colMeans(prof, na.rm = TRUE))
  colnames(prof.wc.hi.ave) <- c("mean")
  prof.wc.hi.sd <- data.frame(apply(prof, 2, sd, na.rm = TRUE))
  colnames(prof.wc.hi.sd) <- c("sd")
  congener <- row.names(prof.wc.hi.ave)
  prof.wc.hi.ave <- cbind(congener, prof.wc.hi.ave$mean,
                          prof.wc.hi.sd$sd)
  colnames(prof.wc.hi.ave) <- c("congener", "mean", "sd")
  prof.wc.hi.ave <- data.frame(prof.wc.hi.ave)
  prof.wc.hi.ave$mean <- as.numeric(as.character(prof.wc.hi.ave$mean))
  prof.wc.hi.ave$sd <- as.numeric(as.character(prof.wc.hi.ave$sd))
  prof.wc.hi.ave$congener <- as.character(prof.wc.hi.ave$congener)
  #Then turn it back into a factor with the levels in the correct order
  prof.wc.hi.ave$congener <- factor(prof.wc.hi.ave$congener,
                                    levels = unique(prof.wc.hi.ave$congener))
  }

# PCB profile plot
ggplot(prof.wc.hi.ave, aes(x = congener, y = mean)) +
  geom_bar(position = position_dodge(), stat = "identity",
           fill = "black") +
  geom_errorbar(aes(ymin = mean, ymax = (mean+sd)), width = 0.9,
                position = position_dodge(0.9)) +
  xlab("") +
  ylim(0, 0.60) +
  theme_bw() +
  theme(aspect.ratio = 4/16) +
  ylab(expression(bold("Mass fraction "*Sigma*"PCB"))) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  annotate("text", x = 4, y = 0.19, label = "PCB 4", size = 3,
           fontface = 1, angle = 90) +
  annotate("text", x = 11, y = 0.13, label = "PCB 11", size = 3,
           fontface = 1, angle = 90) +
  annotate("text", x = 36.2, y = 0.25, label = "PCBs 44+47+65",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 39, y = 0.45, label = "PCBs 45+51",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 44.3, y = 0.13, label = "PCB 52",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 57, y = 0.15, label = "PCB 68",
           size = 3, fontface = 1, angle = 90) +
  annotate("text", x = 140, y = 0.53, label = "3 highest samples",
           size = 3.5, fontface = 1)
