library(fields)
library(ncdf4)

rm(list = ls())

# Input
crop.file <- "./Saves/crop_mapping.csv"
Cv.monthly.file <- "Saves/Cv_monthly_30min_global.RDS"
out.dir <- "../../../../Data/VIC/Forcing/global/VICWOFOST_SA/coverage_monthly/coverage_monthly_"

# Load
crops <- read.csv(crop.file, stringsAsFactors = F)
Cv.monthly <- readRDS(Cv.monthly.file)

# Setup
years <- 1979:2016

res <- 0.5
lons <- seq(
  from = -180 + res / 2,
  to = 180 - res / 2,
  by = res
)
lats <- seq(
  from = -90 + res / 2,
  to = 90 - res / 2,
  by = res
)
vegs <- 1:2

lon.dim <- ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = lons,
  longname = "longitude of cell centre"
)
lat.dim <- ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = lats,
  longname = "latitude of cell centre"
)
veg.dim <- ncdim_def(
  name = "veg_class",
  units = "N/A",
  vals = vegs,
  longname = "Vegetation class identification number"
)

# Calculate and save
i <- 1
for (i in 1:nrow(crops)) {
  print(crops$name[i])

  Cv.monthly.tmp <- Cv.monthly[, , c(i, dim(Cv.monthly)[3]), ]
  Cv.monthly.tmp[, , 1, ] <- Cv.monthly.tmp[, , 1, ] > 0
  Cv.monthly.tmp[, , 2, ] <- 1 - Cv.monthly.tmp[, , 1, ]

  z <- 1
  for (z in 1:length(years)) {
    year <- years[z]
    print(year)

    out.dir.tmp <- gsub(x = out.dir, pattern = "_monthly", replacement = paste0("_", crops$name[i], "_", crops$water[i], "_", crops$season[i], "_monthly"))
    out.file <- paste0(out.dir.tmp, year, ".nc")

    times <- seq(
      from = as.Date(paste0(year, "-01-01")),
      to = as.Date(paste0(year, "-12-31")),
      by = "month"
    )

    time.dim <- ncdim_def(
      name = "time",
      units = "days since 1970-01-01",
      vals = as.numeric(times),
      unlim = T,
      calendar = "standard"
    )

    var <- ncvar_def(
      name = "coverage",
      units = "fraction",
      dim = list(lon.dim, lat.dim, veg.dim, time.dim),
      missval = -1,
      longname = "vegetation coverage",
      prec = "double",
      compression = 9
    )

    dir.create(dirname(out.file), recursive = T)
    nc <- nc_create(out.file, list(var))
    ncvar_put(nc, var$name, Cv.monthly.tmp)
    nc_close(nc)
  }
}
