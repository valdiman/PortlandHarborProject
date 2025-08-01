# R code to download and format data from Pangaea
# Reference
# Slade, Alexis; Martinez, Andres; Mathieu-Campbell, Martine;
# Watkins, Shannon; Cohen, Cassie; Hornbuckle, Keri C (2025):
# Airborne polychlorinated biphenyl congener concentrations using
# PUF-PAS from the Portland Harbor Superfund Site, Portland, OR, 2022
# [dataset publication series]. [dataset].  PANGAEA, https://doi.pangaea.de/10.1594/PANGAEA.983837
# final doi: https://doi.org/10.1594/PANGAEA.983837

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
install.packages('pangaear')

# Library
library(pangaear) # Read data from Pangaea

# Read data from Pangaea and format data ------------------------------
# Set cache path to the project folder
pg_cache$cache_path_set(full_path = "Data/")
# Download original datasets from Pangaea
d.0 <- pg_data(doi = '10.1594/PANGAEA.983837')

# Extract dataset
# 4 corresponds to airborne concentration
d <- d.0[[4]]$data

# Modify the names of the congeners
names(d) <- sub(".*\\((.*)\\).*", "\\1", names(d))

# Export modified dataset
write.csv(d, file = "Data/AirConcV2.csv", row.names = FALSE)





