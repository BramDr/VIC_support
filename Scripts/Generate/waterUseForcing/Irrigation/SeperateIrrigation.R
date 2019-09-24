library(ggplot2)
rm(list = ls())

# Input
iso.file <- "../../../../Data/Primary/ISO3166/ISO3166_dev.csv"
with.file <- "../../../../Data/Transformed/Irrigation/irrigationWithdrawal_country.csv"
fit.out <- "Saves/irrigation_country_fitting.csv"
val.out <- "Saves/irrigation_country_validation.csv"

# Load
iso <- read.csv(file = iso.file, stringsAsFactors = F)
with <- read.csv(with.file, stringsAsFactors = F)

# Setup
set.seed(23021992)
data <- merge(with, iso, by = c("Country_number"))

data <- data[data$With > 1, ]
data$lWith <- log(data$With, 10)

data$row <- 1:nrow(data)

# Calculate
data.fit <- data
data.validation <- data

# Save
dir.create(dirname(fit.out))
write.csv(x = data.fit, file = fit.out, row.names = F)
dir.create(dirname(val.out))
write.csv(x = data.validation, file = val.out, row.names = F)
