library(ncdf4)
library(fields)
rm(list = ls())

# Input
generate.script = "../../Support/generateFunctions.R"
lake.script = "../../Support/generateLakeFunctions.R"
map.script = "../../Support/mapFunctions.R"
lakes.file = "Saves/hydrolake_AnnetteJanssen_chars.csv"
area.file = "../../../Data/Primary/VIC/domain_global.nc"
domain.template = "../../../Data/Primary/VIC/domain_global.nc"
param.template = "../../../Data/Primary/VIC/VIC_params_global.nc"
domain.out = "../../../Data/VIC/Parameters/global/domain_hydrolake_AnnetteJanssen_global.nc"
param.out = "../../../Data/VIC/Parameters/global/VIC_params_hydrolake_AnnetteJanssen_global.nc"

# Load
source(generate.script)
source(lake.script)
source(map.script)

nc = nc_open(area.file)
area = ncvar_get(nc, "area")
nc_close(nc)

lakes = read.csv(file = lakes.file, stringsAsFactors = FALSE)

# Setup
lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)

lakes$x = NA
lakes$y = NA
lakes$fraction = NA
Nlake = array(0, dim = c(length(lons), length(lats)))
for(i in 1:nrow(lakes)){
  x = which.min(abs(lakes$lon[i] - lons))
  y = which.min(abs(lakes$lat[i] - lats))
  
  lakes$x[i] = x
  lakes$y[i] = y
  lakes$fraction[i] = min(1, lakes$area[i] / area[x,y])
  Nlake[x,y] = Nlake[x,y] + 1
}
image.plot(Nlake)

# Calculate
Cl = array(0, dim = c(length(lons), length(lats), max(Nlake, na.rm = T)))
for(i in 1:nrow(lakes)){
  x = lakes$x[i]
  y = lakes$y[i]
  for(z in 1:dim(Cl)[3]){
    if(Cl[x,y,z] == 0){
      Cl[x,y,z] = lakes$fraction[i]
      break
    }
  }
}
Cl.sum = apply(Cl, c(1,2), sum, na.rm = T)
image.plot(Cl.sum)

Cl.adj = Cl
for(x in 1:dim(Cl.sum)[1]){
  for(y in 1:dim(Cl.sum)[2]){
    if(Cl.sum[x,y] > 1){
      Cl.adj[x,y,] = Cl[x,y,] / Cl.sum[x,y]
    }
  }
}
Cl.adj.sum = apply(Cl.adj, c(1,2), sum, na.rm = T)
image.plot(Cl.adj.sum)

Cv = array(0, dim = c(length(lons), length(lats), max(Nlake, na.rm = T) + 1))
Cv[,,1:(dim(Cv)[3] - 1)] = Cl.adj
Cv[,,dim(Cv)[3]] = 1 - Cl.adj.sum
Cv.sum = apply(Cv, c(1,2), sum, na.rm = T)
image.plot(Cv.sum)

## Domain
dir.create(dirname(domain.out))
file.copy(from = domain.template, to = domain.out, overwrite = TRUE)

nc = nc_open(filename = domain.out, write = TRUE)
ncvar_put(nc = nc, varid = nc$var$mask, vals = Nlake > 0)
ncvar_put(nc = nc, varid = nc$var$frac, vals = Nlake > 0)
nc_close(nc = nc)

## Parameters
removeVegVars(nc.old.file = param.template, nc.new.file = param.out)
addVegVars(nc.file = param.out, nveg_class = max(Nlake, na.rm = T) + 1)
addLakeVars(nc.file = param.out, nlake_class = max(Nlake, na.rm = T))
addVegDefaultData(nc.file = param.out, Cv = Cv, na.map = Nlake == 0)
addLakeDefaultData(nc.file = param.out, xs = lakes$x, ys = lakes$y, na.map = Nlake == 0, 
                   lake_id = lakes$ID, source_id = lakes$sourceID, lake_elevation = lakes$elevation, 
                   basin_depth = lakes$depth, basin_area = lakes$fraction)
fillSoilDefaultData(nc.file = param.out, na.map = Nlake == 0, map.script = map.script)

nc = nc_open(filename = param.out, write = TRUE)
ncvar_put(nc = nc, varid = "run_cell", Nlake > 0)
nc_close(nc)

