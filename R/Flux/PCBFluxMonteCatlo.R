# Code to estimate individual PCB fluxes from Portland Harbor
# using 2018 and 2019 water samples
# Air data are not used in these calculations
# Monte Carlo simulation is included

# Packages and libraries needed -------------------------------------------
# Install package
install.packages("ggplot2")

# Load libraries
library(ggplot2)

# Chemical properties -----------------------------------------------------

# Create matrix to storage CP data
cp <- data.frame(matrix(NA, nrow = 159, ncol = 7))

# Add column names
colnames(cp) <- c('Congener', 'MW.PCB', 'nOrtho.Cl', 'H0.mean', 'H0.error',
                  'Kow.mean', 'Kow.error')

# Add PCB names
cp[,1] <- as.factor(c("PCB1", "PCB2", "PCB3", "PCB4", "PCB5", "PCB6", "PCB7", "PCB8",
                      "PCB9", "PCB10", "PCB11", "PCB12+13", "PCB14", "PCB15", "PCB16",
                      "PCB17", "PCB18+30", "PCB19", "PCB20+28", "PCB21+33", "PCB22",
                      "PCB23", "PCB24", "PCB25", "PCB26+29", "PCB27", "PCB31", "PCB32",
                      "PCB34", "PCB35", "PCB36", "PCB37", "PCB38", "PCB39", "PCB40+41+71",
                      "PCB42", "PCB43", "PCB44+47+65", "PCB45+51", "PCB46", "PCB48",
                      "PCB49+69", "PCB50+53", "PCB52", "PCB54", "PCB55", "PCB56", "PCB57",
                      "PCB58", "PCB59+62+75", "PCB60", "PCB61+70+74+76", "PCB63", "PCB64",
                      "PCB66", "PCB67", "PCB68", "PCB72", "PCB73", "PCB77", "PCB78",
                      "PCB79", "PCB80", "PCB81", "PCB82", "PCB83+99", "PCB84",
                      "PCB85+116+117", "PCB86+87+97+108+119+125", "PCB88+91", "PCB89",
                      "PCB90+101+113", "PCB92", "PCB93+95+98+100+102", "PCB94", "PCB96",
                      "PCB103", "PCB104", "PCB105", "PCB106", "PCB107+124", "PCB109",
                      "PCB110+115", "PCB111", "PCB112", "PCB114", "PCB118", "PCB120",
                      "PCB121", "PCB122", "PCB123", "PCB126", "PCB127", "PCB128+166",
                      "PCB129+138+160+163", "PCB130", "PCB131", "PCB132", "PCB133",
                      "PCB134+143", "PCB135+151+154", "PCB136", "PCB137", "PCB139+140",
                      "PCB141", "PCB142", "PCB144", "PCB145", "PCB146", "PCB147+149",
                      "PCB148", "PCB150", "PCB152", "PCB153+168", "PCB155", "PCB156+157",
                      "PCB158", "PCB159", "PCB161", "PCB162", "PCB164", "PCB165", "PCB167",
                      "PCB169", "PCB170", "PCB171+173", "PCB172", "PCB174", "PCB175",
                      "PCB176", "PCB177", "PCB178", "PCB179", "PCB180+193", "PCB181",
                      "PCB182", "PCB183+185", "PCB184", "PCB186", "PCB187", "PCB188",
                      "PCB189", "PCB190", "PCB191", "PCB192", "PCB194", "PCB195", "PCB196",
                      "PCB197+200", "PCB198+199", "PCB201", "PCB202", "PCB203", "PCB204",
                      "PCB205", "PCB206", "PCB207", "PCB208", "PCB209"))

# Add molecular weight
{
  cp[1:3,2] <- c(188.644)
  cp[4:14,2] <- c(223.088)
  cp[15:34,2] <- c(257.532)
  cp[35:64,2] <- c(291.976)
  cp[65:93,2] <- c(326.42)
  cp[94:124,2] <- c(360.864)
  cp[125:145,2] <- c(395.308)
  cp[146:155,2] <- c(429.752)
  cp[156:158,2] <- c(465.740544)
  cp[159,2] <- c(498.64)
}

# Add ortho Cl of individual PCB congeners
cp[,3] <- c(1,	0,	0,	2,	1,	1,	1,	1,	1,	2,	0,
            0,	0,	0,	2,	2,	2,	3,	1,	1,	1,	1,
            2,	1,	1,	2,	1,	2,	1,	0,	0,	0,	0,
            0,	2,	2,	2,	2,	3,	3,	2,	2,	3,	2,
            4,	1,	1,	1,	1,	2,	1,	1,	1,	2,	1,
            1,	1,	1,	2,	0,	0,	0,	0,	0,	2,	2,
            3,	2,	2,	3,	3,	2,	2,	3,	3,	4,	3,
            4,	1,	2,	1,	2,	2,	1,	2,	1,	1,	1,
            2,	1,	1,	0,	0,	2,	2,	2,	3,	3,	2,
            3,	3,	4,	2,	3,	2,	3,	3,	4,	2,	3,
            3,	4,	4,	2,	4,	1,	2,	1,	2,	1,	2,
            2,	1,	0,	2,	3,	2,	3,	3,	4,	3,	3,
            4,	2,	3,	3,	3,	4,	4,	3,	4,	1,	2,
            2,	2,	1,	3,	3,	4,	3,	4,	4,	3,	4,
            2,	3,	4,	4,	4)

# Add log10 Ho of individual PCB congeners
cp[,4] <- c(-3.526,	-3.544,	-3.562,	-3.483,	-3.622,	-3.486,	-3.424,	-3.518,	-3.49,
            -3.373,	-3.537,	-3.595,	-3.376,	-3.649,	-3.6,	-3.428,	-3.495,	-3.355,
            -3.544,	-3.62,	-3.719,	-3.497,	-3.5,	-3.5,	-3.526,	-3.393,	-3.562,
            -3.407,	-3.375,	-3.3745,	-3.473,	-3.818,	-3.634,	-3.524,	-3.503,
            -3.592,	-3.475,	-3.638,	-3.45,	-3.47,	-3.519,	-3.452,	-3.366,	-3.496,
            -3.242,	-3.739,	-3.82,	-3.568,	-3.602,	-3.517,	-3.816,	-3.694,	-3.615,
            -3.565,	-3.693,	-3.631,	-3.424,	-3.441,	-3.284,	-3.989,	-3.787,	-3.705,
            -3.426,	-3.844,	-3.835,	-3.603,	-3.6,	-3.716,	-3.736,	-3.461,	-3.526,
            -3.61,	-3.585,	-3.523,	-3.407,	-3.387,	-3.298,	-3.13,	-4.003,	-3.783,
            -3.768,	-3.55,	-3.707,	-3.574,	-3.574,	-3.845,	-3.901,	-3.61,	-3.253,
            -3.901,	-3.759,	-4.087,	-3.807,	-3.984,	-3.886,	-3.817,	-3.616,	-3.693,
            -3.691,	-3.639,	-3.548,	-3.492,	-3.731,	-3.483,	-3.76,	-3.502,	-3.529,
            -3.328,	-3.727,	-3.625,	-3.367,	-3.296,	-3.369,	-3.783,	-3.075,	-4.053,
            -3.782,	-3.808,	-3.545,	-3.881,	-3.754,	-3.56,	-3.959,	-4.186,	-4.059,
            -3.763,	-3.924,	-3.772,	-3.651,	-3.527,	-3.787,	-3.671,	-3.56,	-3.969,
            -3.638,	-3.59,	-3.696,	-3.339,	-3.434,	-3.693,	-3.353,	-3.177,	-3.95,
            -3.876,	-3.718,	-4.174,	-3.926,	-3.884,	-3.619,	-3.644,	-3.884,	-3.651,
            -3.853,	-3.463,	-4.059,	-4.059,	-3.772,	-3.777,	-3.948)

# Add Ho error
cp[,5] <- c(0.662)

# Add log10 Kow of individual PCB congeners
cp[,6] <- c(4.46,	4.69,	4.69,	4.65,	4.97,	5.06,	5.07,	5.07,	5.06,
            4.84,	5.28,	5.29,	5.28,	5.3,	5.16,	5.25,	5.24,	5.02,
            5.67,	5.6,	5.58,	5.57,	5.35,	5.67,	5.66,	5.44,	5.67,
            5.44,	5.66,	5.82,	5.88,	5.83,	5.76,	5.89,	5.98,	5.76,
            5.75,	5.75,	5.53,	5.53,	5.78,	5.85,	5.62,	5.84,	5.21,
            6.11,	6.11,	6.17,	6.17,	5.95,	6.11,	6.2,	6.17,	5.95,
            6.2,	6.2,	6.26,	6.26,	6.04,	6.36,	6.35,	6.42,	6.48,
            6.36,	6.2,	6.39,	6.04,	6.3,	6.29,	6.13,	6.07,	6.38,
            6.35,	6.13,	6.13,	5.71,	6.22,	5.81,	6.65,	6.64,	6.73,
            6.48,	6.48,	6.76,	6.45,	6.65,	6.74,	6.79,	6.64,	6.64,
            6.74,	6.89,	6.95,	6.74,	6.83,	6.8,	6.58,	6.58,	6.86,
            6.55,	6.64,	6.22,	6.83,	6.67,	6.82,	6.51,	6.67,	6.25,
            6.89,	6.67,	6.73,	6.32,	6.22,	6.92,	6.41,	7.18,	7.02,
            7.24,	7.08,	7.24,	7.02,	7.05,	7.27,	7.42,	7.27,	7.11,
            7.33,	7.11,	7.17,	6.76,	7.08,	7.14,	6.73,	7.36,	7.11,
            7.2,	7.2,	6.85,	6.69,	7.17,	6.82,	7.71,	7.46,	7.55,
            7.52,	7.8,	7.56,	7.65,	7.27,	7.2,	7.62,	7.24,	7.65,
            7.3,	8,	8.09,	7.74,	7.71,	8.18)

# Add Kow error
cp[,7] <- c(0.32)

# Update names
{
  Congener <- cp$Congener
  MW.PCB <- cp$MW.PCB
  nOrtho.Cl <- cp$nOrtho.Cl
  H0.mean <- cp$H0.mean
  H0.error <- cp$H0.error
  Kow.mean <- cp$Kow.mean
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
# WCPCB_OR-POH003 8/22/18, WCPCB_OR-POH004 8/21/18 &
# WCPCB_OR-POH005 8/23/18
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
write.csv(final.result, file = "Output/Data/csv/final.result.csv")

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
