library(fields)
library(ncdf4)
library(raster)
rm(list = ls())

# Input
iso.file <- "../../../Data/Primary/ISO3166/ISO3166_dev.csv"
country.file <- "../../../Data/Transformed/Country/country_6min_global.RDS"
subregion.out <- "../../../Data/Transformed/Country/subregion_6min_global.RDS"

# Load
iso <- read.csv(iso.file, stringsAsFactors = F)
country <- readRDS(country.file)

# Calculate
subregion <- country
for (x in 1:dim(subregion)[1]) {
  for (y in 1:dim(subregion)[2]) {
    if (is.na(country[x, y])) {
      next
    }

    row <- which(iso$Country_number == country[x, y])
    subregion[x, y] <- iso$Subregion_number[row]
  }
}
image.plot(subregion)

# Save
dir.create(dirname(subregion.out))
saveRDS(subregion, subregion.out)
