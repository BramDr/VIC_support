library(ncdf4)
library(fields)
rm(list = ls())

# Input
test.tmp <- "./Saves/test_5min_Indus.nc"
area.tmp <- "./Saves/area_5min_Indus.nc"
area.out <- "../../../Data/Transformed/Routing/area_5min_Indus.RDS"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Calculate
dim.lon <- ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = out.lons,
  longname = "longitude of grid cell center"
)
dim.lat <- ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = out.lats,
  longname = "latitude of grid cell center"
)
var.tmp <- ncvar_def(
  name = "tmp",
  units = "-",
  dim = list(dim.lon, dim.lat)
)

dir.create(dirname(test.tmp))
nc <- nc_create(
  test.tmp,
  list(
    var.tmp
  )
)
ncvar_put(nc, "tmp", rep(1, length(out.lons) * length(out.lats)))
nc_close(nc)

dir.create(dirname(area.tmp))
system(paste0("cdo gridarea ", test.tmp, " ", area.tmp))

nc = nc_open(area.tmp)
area = ncvar_get(nc, "cell_area")
nc_close(nc)
image.plot(area)

# Save
dir.create(dirname(area.out))
saveRDS(area, area.out)
