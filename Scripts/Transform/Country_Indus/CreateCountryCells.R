rm(list = ls())
library(fields)
library(ncdf4)

# Input
country.file <- "../../../Data/Transformed/Country/country_5min_Indus.RDS"
cell.out <- "../../../Data/Transformed/Country/country_cellFractions_5min_Indus.RDS"
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

extent.out <- extent(min(out.lons) - resolution / 2, 
                     max(out.lons) + resolution / 2, 
                     min(out.lats) - resolution / 2, 
                     max(out.lats) + resolution / 2)

# Load
country <- readRDS(country.file)

# Setup
country.unique <- unique(na.omit(c(country)))

# Calculate
country.cell <- list()
for(i in country.unique){
  print(i)
  
  country.cell.df <- data.frame(x = numeric(), y = numeric(), frac = numeric())
  
  for (x in 1:dim(country)[1]) {
    for (y in 1:dim(country)[2]) {
      if (is.na(country[x, y])) {
        next
      }
      if (country[x, y] != i) {
        next
      }
  
      country.cell.df[nrow(country.cell.df) + 1, ] <- c(x, y, 1)
    }
  }
  
  country.cell[[as.character(i)]] <- country.cell.df
}

# Save
dir.create(dirname(cell.out))
saveRDS(country.cell, cell.out)
