library(ncdf4)
library(fields)
rm(list = ls())

# Input
mask.file = "Input/domain_global.nc"
gw.file = "Saves/groundwater_fraction.RDS"
eff.file = "Saves/irrigation_efficiency.RDS"
irr.param.out = "Output/irr_params_MIRCA2000_global.nc"

# Setup
irr.veg = c(12, 13)
irr.pond = c(0, 1)

# Load
groundwater = readRDS(gw.file)
image.plot(groundwater)

eff = readRDS(eff.file)
image.plot(eff)

nc = nc_open(mask.file)
mask = ncvar_get(nc, "mask")
nc_close(nc)

# Setup
get.nearest = function(x,y,data){
  
  for(i in 1:100){
    xmin = x - i
    xmax = x + i
    ymin = y - i
    ymax = y + i
    
    xmin = max(1, xmin)
    ymin = max(1, ymin)
    xmax = min(dim(data)[1], xmax)
    ymax = min(dim(data)[2], ymax)
    
    f = na.omit(c(data[xmin:xmax, ymin:ymax]))
    if(length(f) == 0){
      next
    }
    
    t = table(f)
    o = order(t, decreasing = T)
    r = as.numeric(names(t)[o[1]])
    return(r)
  }
  
  print("could not find")
  return(mean(data))
}

for (x in 1:dim(mask)[1]) {
  for (y in 1:dim(mask)[2]) {
    if (is.na(mask[x, y])) {
      groundwater[x, y] = NA
      eff[x,y] = NA
      next
    }
    
    if (is.na(groundwater[x, y])) {
      groundwater[x, y] = get.nearest(x,y,groundwater)
      #print("missing gw")
    }
    if (is.na(eff[x, y])) {
      eff[x, y] = get.nearest(x,y,eff)
      #print("missing eff")
    }
  }
}
image.plot(groundwater)
image.plot(eff)

# Save
dim.lon = ncdim_def("lon", "degrees_east", seq(
  from = -179.75,
  to = 179.75,
  by = 0.5
))
dim.lat = ncdim_def("lat", "degrees_north", seq(from = -89.75, to = 89.75, by = 0.5))
dim.irr.class = ncdim_def("irr_class", "class", 1:length(irr.veg), longname = "Irrigation class")

var.veg.class = ncvar_def("veg_class",
                          "class",
                          list(dim.irr.class),
                          -1,
                          longname = "Vegetation class of irrigation class",
                          prec = "integer")
var.paddy = ncvar_def(
  "paddy",
  "0 = not paddy irrigation, 1 = paddy irrigation",
  list(dim.irr.class),
  -1,
  longname = "Paddy indicator",
  prec = "integer"
)
var.groundwater_fraction = ncvar_def(
  "groundwater_fraction",
  "-",
  list(dim.lon, dim.lat),
  -1,
  longname = "Fraction of irrigation comming from groundwater",
  prec = "double"
)
var.irrigation_efficiency = ncvar_def(
  "irrigation_efficiency",
  "mm mm-1",
  list(dim.lon, dim.lat),
  -1,
  longname = "Fraction of water withdrawn per water required",
  prec = "double"
)

nc = nc_create(irr.param.out,
               list(var.veg.class, var.paddy, var.groundwater_fraction, var.irrigation_efficiency))
nc_close(nc)

nc = nc_open(irr.param.out, write = T)
ncvar_put(nc, var.veg.class, irr.veg)
ncvar_put(nc, var.paddy, irr.pond)
ncvar_put(nc, var.groundwater_fraction, groundwater)
ncvar_put(nc, var.irrigation_efficiency, eff)
nc_close(nc)
