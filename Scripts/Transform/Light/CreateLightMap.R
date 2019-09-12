library(fields)
library(raster)
rm(list = ls())

# Input
light.file <- "../../../Data/Primary/NASA/BlackMarble_2016_01deg_gray_geo.tif"
country.file <- "../../../Data/Transformed/Country/country_6min_global.RDS"
light.out <- "../../../Data/Transformed/Light/lightCountryFraction_6min_global.RDS"

# Load
light <- raster(light.file)
country <- readRDS(country.file)

# Setup
light <- as.matrix(light)
light <- t(light[nrow(light):1, ])
image.plot(light)

# Calculate
light.country <- data.frame(id = unique(na.omit(c(country))), sum = 0, count = 0)
for (x in 1:dim(country)[1]) {
  for (y in 1:dim(country)[2]) {
    if (is.na(country[x, y]) || is.na(light[x, y])) {
      next
    }

    row <- which(light.country$id == country[x, y])

    light.country$count[row] <- light.country$count[row] + 1
    light.country$sum[row] <- light.country$sum[row] + light[x, y]
  }
}

light.frac <- array(NA, dim = dim(light))
for (x in 1:dim(country)[1]) {
  for (y in 1:dim(country)[2]) {
    if (is.na(country[x, y]) || is.na(light[x, y])) {
      next
    }

    row <- which(light.country$id == country[x, y])

    light.frac[x, y] <- light[x, y] / light.country$sum[row]
    if (light.country$sum[row] == 0) {
      light.frac[x, y] <- 1 / light.country$count[row]
    }
  }
}
image.plot(light.frac)

# Save
dir.create(dirname(light.out))
saveRDS(light.frac, light.out)
