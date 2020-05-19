library(fields)
library(ncdf4)
rm(list = ls())

# Input
function.script = "../../Support/mapFunctions.R"
crop.file = "./Saves/crop_mapping_MIRCA.csv"
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
coverage.file = "./Saves/coverage_monthly_MIRCA_30min_global.RDS"
out.file = "../../../Data/VIC/Forcing/global/coverage_monthly_MIRCA2000/coverage_monthly_MIRCA2000.nc"

# Load
crops = read.csv(crop.file, stringsAsFactors = FALSE)
coverage = readRDS(coverage.file)

nc = nc_open(mask.file)
na.map = ncvar_get(nc, nc$var$mask)
nc_close(nc)
na.map = is.na(na.map) | na.map != 1
image.plot(na.map)

# Setup
source(function.script)
years = 1979:2016

res = 0.5
lons = seq(
  from = -180 + res / 2,
  to = 180 - res / 2,
  by = res
)
lats = seq(
  from = -90 + res / 2,
  to = 90 - res / 2,
  by = res
)
vegs = 1:dim(coverage)[3]

lon.dim = ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = lons,
  longname = "longitude of cell centre"
)
lat.dim = ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = lats,
  longname = "latitude of cell centre"
)
veg.dim = ncdim_def(
  name = "veg_class",
  units = "N/A",
  vals = vegs,
  longname = "Vegetation class identification number"
)
nchar.dim = ncdim_def(
  name = "nchar",
  units = "N/A",
  vals = 1:254,
  longname = "Maximum number of string characters"
)
desc.var = ncvar_def(
  name = "veg_desc",
  units = "N/A",
  dim = list(nchar.dim, veg.dim),
  longname = "vegetation description",
  prec = "char",
  compression = 9
)

# Calculate
description = rep("", veg.dim$len)
vic.id.u = unique(crops$vic.id)
for(i in 1:length(vic.id.u)){
  crops.v = crops[crops$vic.id == vic.id.u[i],]
  description[vic.id.u[i]] = paste0(crops.v$mirca.name[1], "_", crops.v$water[1])
}

for (i in 1:dim(coverage)[3]) {
  print(i)
  
  for(z in 1:dim(coverage)[4]){
    coverage[,,i,z] <- fillMap(map = coverage[,,i,z], na.map = na.map, nearest.function = getNearestZero)
  }
}
#image.plot(coverage[,,39,1])

# Save
for(z in 1:length(years)){
  year = years[z]
  
  out.file.tmp = gsub(x = out.file, pattern = ".nc", replacement = paste0("_", year, ".nc"))
  print(basename(out.file.tmp))
  
  times = seq(
    from = as.Date(paste0(year,"-01-01")),
    to = as.Date(paste0(year, "-12-31")),
    by = "month"
  )
  
  time.dim = ncdim_def(
    name = "time",
    units = "days since 1970-01-01",
    vals = as.numeric(times), 
    unlim = T,
    calendar = "standard"
  )
  
  var = ncvar_def(
    name = "coverage",
    units = "fraction",
    dim = list(lon.dim, lat.dim, veg.dim, time.dim),
    missval = -1,
    longname = "vegetation coverage",
    prec = "double",
    compression = 9
  )
  
  dir.create(dirname(out.file.tmp), recursive = T)
  nc = nc_create(out.file.tmp, list(var, desc.var))
  ncvar_put(nc, var$name, coverage)
  ncvar_put(nc, desc.var$name, description)
  nc_close(nc)
}
