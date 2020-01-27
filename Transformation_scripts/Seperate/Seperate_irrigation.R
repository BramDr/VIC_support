library(ggplot2)
rm(list = ls())

# Input
iso.file = "Input/ISO3166_dev.csv"
with.file = "Input/IrrPerCountry.csv"
fit.out = "Output/country_irrigation_global_fitting.csv"
val.out = "Output/country_irrigation_global_validation.csv"

# Load
iso = read.csv(file = iso.file, stringsAsFactors = F)
with = read.csv(with.file, stringsAsFactors = F)

# Setup
data = merge(with, iso, by = c("Country_number"))

data = data[data$With > 1,]
data$lWith = log(data$With, 10)

data$row = 1:nrow(data)

# Calculate
data.fit = data
data.validation = data

# Save
write.csv(x = data.fit, file = fit.out, row.names = F)
write.csv(x = data.validation, file = val.out, row.names = F)
