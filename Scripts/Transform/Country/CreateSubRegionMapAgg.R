library(fields)
library(ncdf4)
library(raster)
rm(list = ls())

# Input
iso.file <- "../../../Data/Primary/ISO3166/ISO3166_dev.csv"
country.file <- "../../../Data/Transformed/Country/country_6min_global.RDS"
subregion.out <- "../../../Data/Transformed/Country/subregion_30min_global.RDS"

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

  idx <- order(val, decreasing = T)[1]
  val <- as.numeric(names(val)[idx])
  return(val)
}

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

subregion.r <- raster(subregion)
extent(subregion.r) <- c(-180, 180, -90, 90)
subregion.agg.r <- aggregate(subregion.r, fact = 5, fun = getMost)
subregion.agg <- as.matrix(subregion.agg.r)
image.plot(subregion.agg)

# Save
dir.create(dirname(subregion.out))
saveRDS(subregion.agg, subregion.out)
