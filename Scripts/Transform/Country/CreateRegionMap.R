library(fields)
library(ncdf4)
library(raster)
rm(list = ls())

# Input
iso.file <- "../../../Data/Primary/ISO3166/ISO3166_dev.csv"
country.file <- "../../../Data/Transformed/Country/country_6min_global.RDS"
region.out <- "../../../Data/Transformed/Country/region_30min_global.RDS"

# Load
iso <- read.csv(iso.file, stringsAsFactors = F)
country <- readRDS(country.file)

# Setup
getMost <- function(x, na.rm = T) {
  if (na.rm) {
    x <- na.omit(x)
  }

  val <- table(x)
  if (length(val) == 0) {
    return(NA)
  }

  val <- names(val)[order(val, decreasing = T)[1]]
  return(as.numeric(val))
}

# Calculate
region <- country
for (x in 1:dim(region)[1]) {
  for (y in 1:dim(region)[2]) {
    if (is.na(country[x, y])) {
      next
    }

    row <- which(iso$Country_number == country[x, y])
    region[x, y] <- iso$Region_number[row]
  }
}
image.plot(region)

region.r <- raster(region)
extent(region.r) <- c(-180, 180, -90, 90)
region.agg.r <- aggregate(region.r, fact = 5, fun = getMost)
region.agg <- as.matrix(region.agg.r)
image.plot(region.agg)

# Save
dir.create(dirname(region.out))
saveRDS(region.agg, region.out)
