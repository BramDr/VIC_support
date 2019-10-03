library(ncdf4)
library(fields)
rm(list = ls())

# Input
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
force.dir <- "../../../Data/Transformed/WFDEI/"
force.out <- "../../../Data/WOFOST/Forcing/global/"

# Load
nc <- nc_open(mask.file)
mask <- ncvar_get(nc, nc$var$mask)
lats <- nc$dim$lat$vals
lons <- nc$dim$lon$vals
nc_close(nc)

# Setup
years <- 1979:2016

# Calculate
## Initialize files
for (x in 1:length(lons)) {
  for (y in 1:length(lats)) {
    if (is.na(mask[x, y]) || mask[x, y] == 0) {
      next
    }

    file.out <- paste0(force.out, "force_WFDEI_", lats[y], "N_", lons[x], "E", ".txt")
    print(basename(file.out))

    desc.out <- paste0(
      "
** WOFOST FORCING FILE for use with WOFOST Version 5.0, June 1990
** Based on WFDEI forcing data (Weedon et al., 2014)
** Latitude: ", lats[y], " N
** Longitude: ", lons[x], " E
** 
** station number:               (ID)
** year:                         (year)
** day:                          (day-of-year)
** irradiation                   (kJ m-2 d-1)
** minimum temperature           (degrees Celsius)
** maximum temperature           (degrees Celsius)
** early morning vapour pressure (kPa)
** mean wind speed (height: 2 m) (m s-1)
** precipitation                 (mm d-1)
      "
    )

    depr.out <- paste0(
      "
", lats[y], " ", lons[x], " -1 -1 -1           ! lat   lon   elevation   A   B"
    )

    dir.create(dirname(file.out), showWarnings = F, recursive = T)
    if (file.exists(file.out)) {
      file.remove(file.out)
    }
    writeLines(text = paste0(desc.out, depr.out), con = file.out)
  }
}

## Add data
for (year in years) {
  print(year)

  wind.file <- list.files(path = force.dir, pattern = paste0("wind.*", year), full.names = T, recursive = T)
  vp.file <- list.files(path = force.dir, pattern = paste0("vp.*", year), full.names = T, recursive = T)
  pr.file <- list.files(path = force.dir, pattern = paste0("pr.*", year), full.names = T, recursive = T)
  tas.min.file <- list.files(path = force.dir, pattern = paste0("tasMin.*", year), full.names = T, recursive = T)
  tas.max.file <- list.files(path = force.dir, pattern = paste0("tasMax.*", year), full.names = T, recursive = T)
  swdown.file <- list.files(path = force.dir, pattern = paste0("swdown.*", year), full.names = T, recursive = T)

  nc <- nc_open(wind.file)
  dates <- as.Date(nc$dim$time$vals / 24, origin = paste0(year, "-01-01"))
  nc_close(nc)

  station <- rep(-1, length(dates))
  day <- format.Date(dates, "%j")

  nc <- nc_open(swdown.file)
  irradiation <- ncvar_get(nc, nc$var$swdown)
  nc_close(nc)
  nc <- nc_open(tas.min.file)
  minimum.temperature <- ncvar_get(nc, nc$var$tas)
  nc_close(nc)
  nc <- nc_open(tas.max.file)
  maximum.temperature <- ncvar_get(nc, nc$var$tas)
  nc_close(nc)
  nc <- nc_open(vp.file)
  vapour.pressure <- ncvar_get(nc, nc$var$vp)
  nc_close(nc)
  nc <- nc_open(wind.file)
  wind <- ncvar_get(nc, nc$var$wind)
  nc_close(nc)
  nc <- nc_open(pr.file)
  precipitation <- ncvar_get(nc, nc$var$pr)
  nc_close(nc)

  for (x in 1:length(lons)) {
    for (y in 1:length(lats)) {
      if (is.na(mask[x, y]) || mask[x, y] == 0) {
        next
      }

      file.out <- paste0(force.out, "force_WFDEI_", lats[y], "N_", lons[x], "E", ".txt")

      out.data <- data.frame(
        station = station,
        year = year,
        day = day,
        irradiation = format(irradiation[x, y, ] * 1e-3 * 60 * 60 * 24, digits = 2, nsmall = 2),
        minimum.temperature = format(minimum.temperature[x, y, ], digits = 2, nsmall = 2),
        maximum.temperature = format(maximum.temperature[x, y, ], digits = 2, nsmall = 2),
        vapour.pressure = format(vapour.pressure[x, y, ], digits = 2, nsmall = 2),
        wind = format(wind[x, y, ], digits = 2, nsmall = 2),
        precipitation = format(precipitation[x, y, ], digits = 2, nsmall = 2)
      )

      write.table(x = out.data, file = file.out, append = TRUE, sep = " ", row.names = F, col.names = F, quote = F)
    }
  }
}
