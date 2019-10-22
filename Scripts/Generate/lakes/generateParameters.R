library(ncdf4)
library(fields)
rm(list = ls())

# Input
lakes.file = "Saves/hydrolake_AnnetteJanssen_chars.csv"
area.file = "../../../Data/Primary/VIC/domain_global.nc"
domain.template = "../../../Data/Primary/VIC/domain_global.nc"
param.template = "../../../Data/Primary/VIC/VIC_params_global.nc"
domain.out = "../../../Data/VIC/Parameters/global/domain_hydrolake_test_global.nc"
param.out = "../../../Data/VIC/Parameters/global/VIC_params_hydrolake_AnnetteJanssen_global.nc"

# Load
nc = nc_open(area.file)
area = ncvar_get(nc, "area")
nc_close(nc)

lakes = read.csv(file = lakes.file, stringsAsFactors = FALSE)

# Setup
lats = seq(from = -89.75, to = 89.75, by = 0.5)
lons = seq(from = -179.75, to = 179.75, by = 0.5)

veg.vars = c("veg_descr", "veg_class", "Nveg",
             "Cv", "wind_atten", "wind_h",
             "rmin", "rarc", "rad_atten",
             "RGL", "trunk_ratio", "overstory",
             "root_fract", "root_depth",  "LAI", 
             "displacement", "veg_rough", "albedo")
soil.vars = c("AreaFract", "Ds", "Dsmax", "Ksat", "Pfactor",
                "Wcr_FRACT", "Wpwp_FRACT", "Ws", "annual_prec",
                "avg_T", "bubble", "bulk_density", "c", "cellnum",
                "depth", "dp", "elev", "elevation", "expt", "fs_active",
                "gridcell", "infilt", "init_moist", "mask", "off_gmt",
                "phi_s", "quartz", "resid_moist", "rough", "run_cell", 
                "snow_rough", "soil_density", "root_depth", "root_fract")

lakes$x = NA
lakes$y = NA
lakes$fraction = NA
for(i in 1:nrow(lakes)){
  x = which.min(abs(lakes$lon[i] - lons))
  y = which.min(abs(lakes$lat[i] - lats))
  
  lakes$x[i] = x
  lakes$y[i] = y
  lakes$fraction[i] = min(1, lakes$area[i] / area[x,y])
}

# Calculate
Nlake = array(0, dim = c(length(lons), length(lats)))
for(i in 1:nrow(lakes)){
  x = lakes$x[i]
  y = lakes$y[i]
  Nlake[x,y] = Nlake[x,y] + 1
}
image.plot(Nlake)

## Domain
dir.create(dirname(domain.out))
file.copy(from = domain.template, to = domain.out, overwrite = TRUE)

nc = nc_open(filename = domain.out, write = TRUE)
mask = ncvar_get(nc = nc, varid = nc$var$mask)
mask[is.na(mask)] = 0
ncvar_put(nc = nc, varid = nc$var$mask, vals = Nlake > 0 & mask > 0)
nc_close(nc = nc)

## Parameters
system(command = paste0("ncks -x -v ", paste0(veg.vars, collapse = ","), " ", param.template, " -O ", param.out))

source("generateFunctions.R")
AddVegVars(param.out, max(Nlake, na.rm = T))
AddLakeVars(param.out)
PutVegVars(param.out, Nlake)
PutLakeVars(param.out, lakes, area, Nlake)
PutOtherVars(param.out, soil.vars, Nlake)

nc = nc_open(filename = param.out, write = TRUE)
ncvar_put(nc = nc, varid = "run_cell", Nlake > 0)
nc_close(nc)
