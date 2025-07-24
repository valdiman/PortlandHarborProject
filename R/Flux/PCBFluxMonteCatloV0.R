# Code to estimate individual PCB fluxes from Portland Harbor
# using 2018 and 2019 water samples from EPA
# A Monte Carlo simulation is included

# Packages and libraries needed -------------------------------------------
# Install package
install.packages("ggplot2")

# Load libraries
library(ggplot2)

# Chemical properties -----------------------------------------------------
# Read values
cp <- read.csv('Data/ChemicalProperties.csv')

# Update names
{
  Congener <- cp$Congener
  MW.PCB <- cp$MW.PCB
  nOrtho.Cl <- cp$nOrtho.Cl
  H0.mean <- cp$H0
  H0.error <- cp$H0.error
  Kow.mean <- cp$Kow
  Kow.error <- cp$Kow.error
}

# Water concentrations ----------------------------------------------------
# Read water concentrations
wc.raw <- read.csv("Data/WaterConcentrationV02.csv")
# Different approaches to use the data
# Prepare data [pg/L] = [ng/m3]
{
  wc.1 <- subset(wc.raw, select = -c(SampleID:Units))
  wc.2 <- cbind(wc.raw$LocationID, wc.1)
  colnames(wc.2)[1] <- "LocationID"
}
# (1) All samples
# (i) Mean and standard deviation
{
  wc.ave <- sapply(wc.1, mean, na.rm = TRUE)
  wc.sd <- sapply(wc.1, sd, na.rm = TRUE)
  wc.3 <- data.frame(t(rbind(wc.ave, wc.sd)))
  C.PCB.water.mean <- wc.3$wc.ave
  C.PCB.water.sd <- wc.3$wc.sd
}
# (ii) Geometric mean and geometric standard deviation
# Log 10 individual PCBs
{
  wc.log <- log10(wc.1)
  wc.gm <- exp(sapply(wc.log, mean, na.rm = TRUE))
  wc.gsd <- exp(sapply(wc.log, sd, na.rm = TRUE))
  wc.4 <- data.frame(t(rbind(wc.gm, wc.gsd)))
  C.PCB.water.gm <- wc.4$wc.gm
  C.PCB.water.gsd <- wc.4$wc.gsd
}
# (2) Specific site
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
# Calculate mean and sd for each site
{
  # WCPCB_OR-POH001
  wc.POH001.ave <- sapply(wc.POH001[, 2:160], mean, na.rm = TRUE)
  wc.POH001.sd <- sapply(wc.POH001[, 2:160], sd, na.rm = TRUE)
  wc.POH001.2 <- data.frame(t(rbind(wc.POH001.ave, wc.POH001.sd)))
  C.PCB.water.POH001.mean <- wc.POH001.2$wc.POH001.ave
  C.PCB.water.POH001.sd <- wc.POH001.2$wc.POH001.sd
  # WCPCB_OR-POH002
  wc.POH002.ave <- sapply(wc.POH002[, 2:160], mean, na.rm = TRUE)
  wc.POH002.sd <- sapply(wc.POH002[, 2:160], sd, na.rm = TRUE)
  wc.POH002.2 <- data.frame(t(rbind(wc.POH002.ave, wc.POH002.sd)))
  C.PCB.water.POH002.mean <- wc.POH002.2$wc.POH002.ave
  C.PCB.water.POH002.sd <- wc.POH002.2$wc.POH002.sd
  # WCPCB_OR-POH003
  wc.POH003.ave <- sapply(wc.POH003[, 2:160], mean, na.rm = TRUE)
  wc.POH003.sd <- sapply(wc.POH003[, 2:160], sd, na.rm = TRUE)
  wc.POH003.2 <- data.frame(t(rbind(wc.POH003.ave, wc.POH003.sd)))
  C.PCB.water.POH003.mean <- wc.POH003.2$wc.POH003.ave
  C.PCB.water.POH003.sd <- wc.POH003.2$wc.POH003.sd
  # WCPCB_OR-POH004
  wc.POH004.ave <- sapply(wc.POH004[, 2:160], mean, na.rm = TRUE)
  wc.POH004.sd <- sapply(wc.POH004[, 2:160], sd, na.rm = TRUE)
  wc.POH004.2 <- data.frame(t(rbind(wc.POH004.ave, wc.POH004.sd)))
  C.PCB.water.POH004.mean <- wc.POH004.2$wc.POH004.ave
  C.PCB.water.POH004.sd <- wc.POH004.2$wc.POH004.sd
  # WCPCB_OR-POH005
  wc.POH005.ave <- sapply(wc.POH005[, 2:160], mean, na.rm = TRUE)
  wc.POH005.sd <- sapply(wc.POH005[, 2:160], sd, na.rm = TRUE)
  wc.POH005.2 <- data.frame(t(rbind(wc.POH005.ave, wc.POH005.sd)))
  C.PCB.water.POH005.mean <- wc.POH005.2$wc.POH005.ave
  C.PCB.water.POH005.sd <- wc.POH005.2$wc.POH005.sd
  # WCPCB_OR-POH006
  wc.POH006.ave <- sapply(wc.POH006[, 2:160], mean, na.rm = TRUE)
  wc.POH006.sd <- sapply(wc.POH006[, 2:160], sd, na.rm = TRUE)
  wc.POH006.2 <- data.frame(t(rbind(wc.POH006.ave, wc.POH006.sd)))
  C.PCB.water.POH006.mean <- wc.POH006.2$wc.POH006.ave
  C.PCB.water.POH006.sd <- wc.POH006.2$wc.POH006.sd
  # WCPCB_OR-POH007
  wc.POH007.ave <- sapply(wc.POH007[, 2:160], mean, na.rm = TRUE)
  wc.POH007.sd <- sapply(wc.POH007[, 2:160], sd, na.rm = TRUE)
  wc.POH007.2 <- data.frame(t(rbind(wc.POH007.ave, wc.POH007.sd)))
  C.PCB.water.POH007.mean <- wc.POH007.2$wc.POH007.ave
  C.PCB.water.POH007.sd <- wc.POH007.2$wc.POH007.sd
}

# (3) Average of the three highest samples
# This approach was used in the paper to estimate emissions
# and used in AERMOD
# WCPCB_OR-POH003 8/22/18, WCPCB_OR-POH004 8/23/18 &
# WCPCB_OR-POH005 8/21/18
{
  wc.hi <- wc.2[c(7, 10, 13), 2:160]
  C.PCB.water.hi.ave <- sapply(wc.hi, mean)
  C.PCB.water.hi.sd <- sapply(wc.hi, sd)
  
  # Select water concentration to be used
  C.PCB.water.ave <- C.PCB.water.hi.ave
  C.PCB.water.error <- C.PCB.water.hi.sd
}

# Meteorological data -----------------------------------------------------
# Data obtained from ReadNOAAData.R data and ReadUSGSData.R codes
# 2018-08
{
  tair.mean <- 21.3 # C, data from NOAA
  tair.error <- 5.27 # C, data from NOAA
  twater.mean <- 23.24 # C, data from USGS
  twater.error <- 1.29 # C, data from USGS
  u.mean <- 2.35 # m/s, data from NOAA
  u.error <- 1.58 # m/s, data from NOAA
  # Modify u @6.7 m to @10 m
  u10.mean <- (10.4/(log(6.7) + 8.1))*u.mean
  u10.error <- (10.4/(log(6.7) + 8.1))*u.error 
  P.mean <- 1016 # mbar, data from NOAA
  P.error <- 3.23 # mbar, data from NOAA
}

# Flux calculations -------------------------------------------------------

# Flux function
final.result = function(MW.PCB, H0.mean, H0.error, 
                        C.PCB.water.ave, C.PCB.water.error, nOrtho.Cl,
                        Kow.mean, Kow.error) {
  # fixed parameters
  
  R <- 8.3144 # [Pa m3/K/mol]
  T <- 298.15 # [K]
  
  F.PCB.aw <- NULL
  # number of replicates for Monte Carlo simulation
  for (replication in 1:1000) {
    
    # Random parameters
    # Parameters for calculating Delta Uaw
    a <- rnorm(1, 0.085, 0.007)
    b <- rnorm(1, 1, 0.5)
    c <- rnorm(1, 32.7, 1.6)
    # Parameter for calculating Delta Uoa
    a2 <- rnorm(1, 0.13, 0.02) 
    b2 <- rnorm(1, 2.9, 1.2)
    c2 <- rnorm(1, 47.8, 4.3)
    # Henry's law constant [Pa m3/mol]
    H0 <- rnorm(1, H0.mean, H0.error)
    # Octanol-water partition coefficient [Lwater/Loctanol]
    Kow <- rnorm(1, Kow.mean, Kow.error) 
    # PCB water concentration [pg/L] = [ng/m3]
    # Concentrations can be changed
    C.PCB.water <- abs(rnorm(1, C.PCB.water.ave, C.PCB.water.error)) # [pg/L]
    # DOC [mg/L] (Spencer et al 2012)
    DOC <- abs(rnorm(1, 2, 0.3))
    # Water temperature [C]
    T.water <- rnorm(1, twater.mean, twater.error)
    # Air temperature [C]
    T.air <- rnorm(1, tair.mean, tair.error)
    # atmospheric pressure [mbar]
    P <- rnorm(1, P.mean, P.error)
    # Wind speed @10 m [m/s]
    u <- abs(rnorm(1, u10.mean, u10.error))
    
    # Computed values
    # Henry's law constant (HLC) corrections
    # PCB internal energy for the transfer of water to air transfer
    DeltaUaw <- (a*MW.PCB-b*nOrtho.Cl+c)*1000 # [J/mol]
    # Transform HLC to K
    K <- 10^(H0)*101325/(R*T) # [Lwater/Lair]
    # Correct K using water temperature
    K.air.water <- K*exp(-(DeltaUaw/R)*(1/(T.water + 273.15) - 1/T)) # [Lwater/Lair]
    # Final K (corrected by air and water temperature)
    K.final <- K.air.water*(T.water + 273.15)/(T.air + 273.15) # [Lwater/Lair]
    
    # KDOC calculation and correction
    # PCB internal energy for the transfer of octanol-air
    DeltaUoa <- (-a2*MW.PCB+b2*nOrtho.Cl-c2)*1000 # [J/mol]
    # PCB internal energy for the transfer of octanol-water
    DeltaUow <- DeltaUoa + DeltaUaw # [J/mol]
    # Octanol-water partition coefficient corrected by water temperature
    Kow.water.t <- 10^(Kow)*exp(-(DeltaUow/R)*(1/(T.water + 273.15)-1/T)) # [Loctanol/Lwater]
    # DOC-water partition coefficient
    Kdoc.t <- 0.06*Kow.water.t # [Lwater/kgdoc]
    
    # Freely dissolved water concentration calculations
    C.PCB.water.f <- C.PCB.water/(1 + Kdoc.t*DOC/1000^2) # [ng/m3]
    
    # Air-water mass transfer calculations
    # (1) Air side mass transfer calculations
    # Water diffusivity in air corrected by air
    # temperature and atmospheric pressure
    D.water.air <- (10^(-3)*1013.25*((273.15+T.air)^1.75*((1/28.97) +
                                                            (1/18.0152))^(0.5))/P/(20.1^(1/3) + 9.5^(1/3))^2) # [cm2/s]
    # PCB diffusivity in air
    D.PCB.air <- D.water.air*(MW.PCB/18.0152)^(-0.5) # [cm2/s]
    # Water vapor exchange velocity in air (from eq. 20-15)
    V.water.air <- 0.2*u + 0.3 # u @10 meter [cm/s]
    # Air side mass transfer
    V.PCB.air <- V.water.air*(D.PCB.air/D.water.air)^(2/3) # [cm/s]
    
    # (2) Water side mass transfer calculations
    # Dynamic viscosity of water at water temperature [kg/m/s]
    visc.water <- 10^(-4.5318-220.57/(149.39 - (273.15 + T.water)))
    # Water density corrected at water temperature [kg/m3]
    dens.water <- (999.83952+16.945176*T.water - 7.9870401*10^-3*T.water^2
                   - 46.170461*10^-6*3 + 105.56302*10^-9*T.water^4 -
                     280.54253*10^-12*T.water^5)/(1 + 16.87985*10^-3*T.water)
    # Kinematic viscosity of water
    v.water <- visc.water/dens.water*100^2 # [cm2/s]
    # CO2 diffusivity in water at water temperature
    diff.co2 <- 0.05019*exp(-19.51*1000/(273.15 + T.water)/R) # [cm2/s]
    # PCB diffusivity in water 
    D.PCB.water <- diff.co2*(MW.PCB/44.0094)^(-0.5) # [cm2/s]
    # PCB Schmidt number in water []
    Sc.PCB.water <- v.water/D.PCB.water
    # CO2 Schmidt number in water
    Sc.co2.water <- v.water/diff.co2
    # k600 calculations, u in [m/s], k600 originally [cm/h]
    # from Alin et al., 2011
    k600 <- (4.46 + 7.11*u)/60/60 # [cm/s]
    # Water side mass transfer (from eq. 20-24a)
    if(u > 5){
      V.PCB.water <- k600*(Sc.PCB.water/Sc.co2.water)^(-0.5)  
    } else {
      V.PCB.water <- k600*(Sc.PCB.water/Sc.co2.water)^(-2/3)
    } # [cm/s]
    
    # Air-water mass transfer
    mtc.PCB <- ((1/V.PCB.water+1/(V.PCB.air*K.final)))^(-1) # [cm/s]
    # Flux calculations
    F.PCB.aw <- c(F.PCB.aw, mtc.PCB*(C.PCB.water.f)*(60*60*24)/100) # [ng/m2/d]
    
  }
  
  mmm <-mean(F.PCB.aw) #ng/m2/day
  sss <- sd(F.PCB.aw) #ng/m2/day
  q2.5 <- quantile(F.PCB.aw, 0.025, na.rm = TRUE)
  q97.5 <- quantile(F.PCB.aw, 0.975, na.rm = TRUE)
  
  c(mmm, sss, q2.5, q97.5)
  
}

# Final calculations ------------------------------------------------------

Num.Congener <- length(Congener)

result <- NULL
for (i in 1:Num.Congener) {
  result <- rbind(result, final.result(MW.PCB[i], H0.mean[i], H0.error[i], 
                                       C.PCB.water.ave[i], C.PCB.water.error[i], nOrtho.Cl[i],
                                       Kow.mean[i], Kow.error[i]))
}

# Compiling results
final.result = data.frame(Congener, result)
# Just 3 significant figures
final.result[, 2:5] <- lapply(final.result[, 2:5], formatC, format = "f",
                              decimal.mark = ".", digits = 3)
# Add column names
names(final.result) = c("Congener", "Mean (ng/m2/d)",
                        "Std (ng/m2/d)", "2.5%CL (ng/m2/d)",
                        "97.5%CL (ng/m2/d)")
# Export data
write.csv(final.result, file = "Output/Data/csv/PCBFlux.csv")

# Select individual PCB fluxes --------------------------------------------
# Select individual congeners
# PCB4
print(final.result[4,])
# PCB11
print(final.result[11,])
# PCB44+47+65
print(final.result[38,])
# PCB45+51
print(final.result[39,])
# PCB68
print(final.result[57,])

# Plot fluxes -------------------------------------------------------------
# Organize x-axis
final.result$Congener <- as.character(final.result$Congener)
# Then turn it back into a factor with the levels in the correct order
final.result$Congener <- factor(final.result$Congener,
                                levels = unique(final.result$Congener))
# Change character to numeric for the fluxes
final.result[, 2:5] <- lapply(final.result[, 2:5], as.numeric)

# (1) Bar plot
ggplot(final.result, aes(x = Congener, y = `Mean (ng/m2/d)`)) +
  geom_bar(position = position_dodge(), stat = "identity",
           fill = "black") +
  geom_errorbar(aes(ymin = `Mean (ng/m2/d)`,
                    ymax = (`Mean (ng/m2/d)` + `Std (ng/m2/d)`)),
                width = 0.9, position = position_dodge(0.9)) +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 7/25) +
  ylab(expression(bold("PCBi flux (ng/m"^2*"/d)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  annotate("text", x = 4, y = 110, label = "PCB 4", size = 2.5,
           fontface = 1, angle = 90) +
  annotate("text", x = 11, y = 70, label = "PCB 11", size = 2.5,
           fontface = 1, angle = 90) +
  annotate("text", x = 18, y = 110, label = "PCB 19",
           size = 2.5, fontface = 1, angle = 90) +
  annotate("text", x = 37, y = 140, label = "PCBs 44+47+65",
           size = 2.5, fontface = 1, angle = 90) +
  annotate("text", x = 41, y = 190, label = "PCBs 45+51",
           size = 2.5, fontface = 1, angle = 90) +
  annotate("text", x = 44, y = 60, label = "PCB 52",
           size = 2.5, fontface = 1, angle = 90) +
  annotate("text", x = 57, y = 70, label = "PCB 68",
           size = 2.5, fontface = 1, angle = 90)
