library(ncdf4)
library(fields)
rm(list = ls())

# Input
function.script = "../../Support/mapFunctions.R"
crop.file = "./Saves/crop_mapping_single.csv"
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
start.file = "./Saves/plantDay_SAGE_30min_global.RDS"
end.file = "./Saves/harvestDay_SAGE_30min_global.RDS"
season.out = "../../../Data/WOFOST/Parameters/Season/global/SA/season_params_global_SAGE.nc"

# Load
crops = read.csv(crop.file, stringsAsFactors = F)
start = readRDS(start.file)
end = readRDS(end.file)

nc = nc_open(mask.file)
na.map = ncvar_get(nc, nc$var$mask)
nc_close(nc)
na.map = is.na(na.map) | na.map != 1

# Setup
source(function.script)
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)

for(z in 1:nrow(crops)){
  print(z)
  start[,,z] <- fillMap(map = start[,,z], na.map = na.map, nearest.function = getNearestMean)
  end[,,z] <- fillMap(map = end[,,z], na.map = na.map, nearest.function = getNearestMean)
}

# Create
dim.lon <- ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = lons,
  longname = "longitude of grid cell center"
)
dim.lat <- ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = lats,
  longname = "latitude of grid cell center"
)

var.start <- ncvar_def(
  name = "start",
  units = "day of year",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "Day of year the crop is planted",
  compression = 5
)
var.end <- ncvar_def(
  name = "end",
  units = "day of year",
  dim = list(dim.lon, dim.lat),
  missval = -1,
  longname = "Day of year the crop is harvested",
  compression = 5
)

# Calculate & save
for (i in 1:nrow(crops)) {
  print(crops$mirca.name[i])
  
  season.out.tmp = gsub(x = season.out, pattern = "params_", replacement = paste0("params_", crops$mirca.name[i], "_"))
  
  dir.create(dirname(season.out.tmp))
  nc <- nc_create(
    season.out.tmp,
    list(
      var.start,
      var.end
    )
  )
  nc_close(nc)
  
  # Save
  nc <- nc_open(season.out.tmp, write = T)
  ncatt_put(
    nc = nc,
    varid = 0,
    attname = "Description",
    attval = "Season parameters for WOFOST. Created by Bram Droppers"
  )
  
  ncvar_put(nc = nc, varid = nc$var$start, vals = start[,,i])
  ncvar_put(nc = nc, varid = nc$var$end, vals = end[,,i])
  nc_close(nc)
}
