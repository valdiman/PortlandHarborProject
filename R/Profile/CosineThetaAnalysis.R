# Script to calculate Cosine theta for the air samples.
# average water profile, sediment profile and Aroclors

# Packages and libraries needed --------------------------------------------
# Install packages
install.packages("lsa")
install.packages("SnowballC")

# Libraries
library(lsa)

# Read data ---------------------------------------------------------------
# Air profiles
prof.air <- read.csv("Output/Data/csv/ProfAir.csv", header = TRUE,
                     check.names = FALSE)
# Aroclor profiles
prof.aroclor <- read.csv("Data/FrameAroclor_173list.csv", header = TRUE,
                check.names = FALSE)
# Water profile
prof.water <- read.csv("Output/Data/csv/AveProfWater.csv", header = TRUE,
                       check.names = FALSE)
# Flux profile
prof.flux <- read.csv("Output/Data/csv/PCBFlux.csv", header = TRUE,
                       check.names = FALSE)

# Format air and aroclors data
# Removing metadata
prof.air.1 <- prof.air[,-c(1, 2)]
prof.aroclor.1 <- prof.aroclor[,-1]

# Format water and flux data
prof.water.1 <- as.data.frame(prof.water[, c(2, 3)])
prof.flux.1 <- as.data.frame(prof.flux[, c(2, 3)])

# Need to transform values to fraction in the flux
prof.flux.2 <- prof.flux.1[, -1]  # Remove metadata
tmp <- sum(prof.flux.2, na.rm = TRUE)
prof <- sweep(prof.flux.2, 1, tmp, FUN = "/")
prof.ave <- data.frame(colMeans(prof, na.rm = TRUE))
colnames(prof.ave) <- c("mean")



prof.water.flux <- cbind(prof.water.1, prof.flux.1$`Mean (ng/m2/d)`)


# Combine both profiles
prof <- rbind(prof.air.1, prof.aroclor.1)
# Change format to numeric
prof[] <- lapply(prof, function(x) as.numeric(as.character(x)))

# Calcualte cosine Theta
CosineTheta <- as.data.frame(cosine(t(prof)))

# Add metadata
siteName <- prof.air[, 2]
siteName <- as.data.frame(siteName)
colnames(siteName) <- 'name'
AroclorName <- prof.aroclor[, 1]
AroclorName <- as.data.frame(AroclorName)
colnames(AroclorName) <- 'name'
id <- rbind(siteName, AroclorName)
colnames(CosineTheta) <- t(id)
CosineTheta <- cbind(id, CosineTheta)
# Remove values on the upper diagonal (duplicates)
CosineTheta[upper.tri(CosineTheta, diag = FALSE)] <- NA

# Save results
write.csv(CosineTheta, "Output/Data/csv/CosineThetaResults.csv")  


