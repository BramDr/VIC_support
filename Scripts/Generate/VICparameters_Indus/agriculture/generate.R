rm(list = ls())
library(fields)
library(ncdf4)

# Input
map.support.file <- "../../../Support/mapFunctions.R"
generate.support.file <- "../../../Support/generateFunctions.R"
mask.file <- "../../../../../Data/Transformed/Routing/mask_5min_indus.RDS"
vegetation.file <- "../../../../../Data/VIC/Parameters/Indus_5min/vegetation_params_Modis_Indus.nc"
coverage.file = "./Saves/coverage_adjusted_monthly_5min_Indus.RDS"
canopy.file = "./Saves/canopy_adjusted_monthly_5min_Indus.RDS"
lai.file = "./Saves/lai_adjusted_monthly_5min_Indus.RDS"
vegetation.out <- "../../../../../Data/VIC/Parameters/Indus_5min/vegetation_params_Mirca_Indus.nc"
nbare = 2

# Load
source(map.support.file)
source(generate.support.file)

mask <- readRDS(mask.file)
coverage = readRDS(coverage.file)
canopy = readRDS(canopy.file)
lai = readRDS(lai.file)

# Setup
coverage.mean = apply(X = coverage, MARGIN = c(1,2,3), FUN = mean)
coverage.max = apply(X = coverage, MARGIN = c(1,2,3), FUN = max)
sel = coverage.max > 0 & coverage.mean <= 0
print(sum(sel, na.rm = T))
coverage.mean[sel] = 1e-5

Nveg <- apply(X = coverage.mean, MARGIN = c(1, 2), FUN = function(x) {
  sum(na.omit(x[1:(length(x) - nbare)]) > 0)
})
image.plot(Nveg)

lai.fill = lai
for(v in 1:dim(coverage.mean)[3]){
  print(v)
  na.map = is.na(coverage.mean[,,v]) | coverage.mean[,,v] <= 0
  lai.fill[,,,v] <- fillMap(lai[,,,v], na.map, getNearestMean)
}

canopy.fill = canopy
for(v in 1:dim(coverage.mean)[3]){
  print(v)
  na.map = is.na(coverage.mean[,,v]) | coverage.mean[,,v] <= 0
  canopy.fill[,,,v] <- fillMap(canopy[,,,v], na.map, getNearestMean)
}

#lai.tmp = lai.fill[,,,1:(dim(lai.fill)[4] - 1)]
#canopy.tmp = canopy.fill[,,,1:(dim(lai.fill)[4] - 1)]
#sel = !is.na(lai.tmp) & lai.tmp / canopy.tmp < 1 & canopy.tmp > 0.0001
#print(sum(sel))

copy.orig.data = function(orig.file, new.file, var.name, cv.data, orig.crop.idx = 12){
  nc = nc_open(orig.file)
  var.data = ncvar_get(nc, var.name)
  nc_close(nc)
  
  orig.nvegs = nc$dim$veg_class$len
  orig.veg.idxs = 1:(orig.nvegs - 1)
  orig.bare.idx = orig.nvegs
  
  nc = nc_open(new.file, write = T)
  
  new.nvegs = nc$dim$veg_class$len
  new.veg.idxs = orig.veg.idxs
  new.crop.idxs = orig.nvegs:(new.nvegs - nbare)
  new.bare.idxs = (new.nvegs - nbare + 1):new.nvegs
  
  if(length(dim(var.data)) == 3){
    ncvar_put(nc, var.name, var.data[,,orig.veg.idxs],
              start = c(1,1,min(new.veg.idxs)),
              count = c(-1,-1,length(new.veg.idxs)))
    
    for(new.crop.idx in new.crop.idxs){
      na.map = is.na(cv.data[,,new.crop.idx]) | cv.data[,,new.crop.idx] <= 0
      var.data.crop <- fillMap(var.data[,,orig.crop.idx], na.map, getNearestMean)
      
      ncvar_put(nc, var.name, var.data.crop,
                start = c(1,1,min(new.crop.idx)),
                count = c(-1,-1,length(new.crop.idx)))
    }
    for(new.bare.idx in new.bare.idxs){
      ncvar_put(nc, var.name, var.data[,,orig.bare.idx],
                start = c(1,1,min(new.bare.idx)),
                count = c(-1,-1,length(new.bare.idx)))
    }
    
  } else if (length(dim(var.data)) == 4) {
    ncvar_put(nc, var.name, var.data[,,,orig.veg.idxs],
              start = c(1,1,1,min(new.veg.idxs)),
              count = c(-1,-1,-1,length(new.veg.idxs)))
              
    for(new.crop.idx in new.crop.idxs){
      na.map = is.na(cv.data[,,new.crop.idx]) | cv.data[,,new.crop.idx] <= 0
      var.data.crop <- fillMap(var.data[,,,orig.crop.idx], na.map, getNearestMean)
      
      ncvar_put(nc, var.name, var.data.crop,
                start = c(1,1,1,min(new.crop.idx)),
                count = c(-1,-1,-1,length(new.crop.idx)))
    }
    for(new.bare.idx in new.bare.idxs){
      ncvar_put(nc, var.name, var.data[,,,orig.bare.idx],
                start = c(1,1,1,min(new.bare.idx)),
                count = c(-1,-1,-1,length(new.bare.idx)))
    }
  } else {
    print(paste0(var.name, " dim == ", length(dim(var.data)), "?"))
    next
  }
  
  nc_close(nc)
}

# Create
nc = nc_open(vegetation.file)
dim.lon = nc$dim$lon
dim.lat = nc$dim$lat
dim.month = nc$dim$month
dim.root_zone = nc$dim$root_zone
nc_close(nc)

dim.veg_class = ncdim_def(name = nc$dim$veg_class$name,
                          units = nc$dim$veg_class$units,
                          vals = 1:dim(coverage)[3],
                          longname = nc$dim$veg_class$name)

var.list = list()
for(var in c(veg.vars)) {
  print(var)
  if(var %in% c("Nveg")){
    var.var = cropyCreateVariable(base.file = vegetation.file, base.name = var,
                                  dim = list(dim.lon, dim.lat), 
                                  chunksizes = c(dim.lon$len, dim.lat$len))
  } else if(var %in% c("Cv", "overstory", "rarc", "rmin", "RGL", "rad_atten", "wind_atten", "trunk_ratio", "wind_h")){
    var.var = cropyCreateVariable(base.file = vegetation.file, base.name = var,
                                  dim = list(dim.lon, dim.lat, dim.veg_class), 
                                  chunksizes = c(dim.lon$len, dim.lat$len, 1))
  } else if (var %in% c("veg_rough", "displacement", "albedo", "fcanopy", "LAI")) {
    var.var = cropyCreateVariable(base.file = vegetation.file, base.name = var,
                                  dim = list(dim.lon, dim.lat, dim.month, dim.veg_class), 
                                  chunksizes = c(dim.lon$len, dim.lat$len, 1, 1))
  } else if (var %in% c("root_depth", "root_fract")) {
    var.var = cropyCreateVariable(base.file = vegetation.file, base.name = var,
                                  dim = list(dim.lon, dim.lat, dim.root_zone, dim.veg_class), 
                                  chunksizes = c(dim.lon$len, dim.lat$len, 1, 1))
  }
  
  var.list[[var]] = var.var
}
var.list[["fcanopy"]] = fcanopy.var <- ncvar_def(name = "fcanopy", 
                                                 units = "#", 
                                                 dim = list(dim.lon, dim.lat, dim.month, dim.veg_class), 
                                                 longname = "Canopy cover fraction", 
                                                 prec = "double", missval = -1)

dir.create(dirname(vegetation.out))
nc = nc_create(vegetation.out, vars = var.list)
nc_close(nc)

# Save
var = veg.vars[3]
var = "LAI"
for(var in c(veg.vars, "fcanopy")) {
  print(var)
  if(! (var %in% c("Cv", "fcanopy", "Nveg", "LAI"))){
    copy.orig.data(vegetation.file, vegetation.out, var, coverage.mean, 12)
  } else if (var == "Cv") {
    nc = nc_open(vegetation.out, write = T)
    ncvar_put(nc, var, coverage.mean)
    nc_close(nc)
  } else if (var == "fcanopy") {
    nc = nc_open(vegetation.out, write = T)
    ncvar_put(nc, var, canopy.fill)
    nc_close(nc)
  } else if (var == "LAI") {
    nc = nc_open(vegetation.out, write = T)
    ncvar_put(nc, var, lai.fill)
    nc_close(nc)
  } else if (var == "Nveg") {
    nc = nc_open(vegetation.out, write = T)
    ncvar_put(nc, var, Nveg)
    nc_close(nc)
  }
  
  if (var == "LAI") {
    nc = nc_open(vegetation.out, write = T)
    ncvar_put(nc, var, lai.fill[,,,(dim(lai.fill)[4] - nbare + 1):dim(lai.fill)[4]] * 0,
              start = c(1,1,1,dim(lai.fill)[4] - nbare + 1),
              count = c(-1,-1,-1,nbare))
    nc_close(nc)
  }
  if (var == "fcanopy") {
    nc = nc_open(vegetation.out, write = T)
    ncvar_put(nc, var, canopy.fill[,,,(dim(canopy.fill)[4] - nbare + 1):dim(canopy.fill)[4]] * 0 + 0.0001,
              start = c(1,1,1,dim(canopy.fill)[4] - nbare + 1),
              count = c(-1,-1,-1,nbare))
    nc_close(nc)
  }
}
