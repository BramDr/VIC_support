library(fields)
library(ncdf4)
rm(list = ls())

# Input
mask.file = "Input/domain_global.nc"
loc.file = "Input/MIRCA2000_cropping_calendars_30min.txt"
cc.paddy.file = "Saves/MIRCA2000_cell_calendars_paddy.csv"
cc.irr.file = "Saves/MIRCA2000_cell_calendars_irrigation.csv"
cc.rain.file = "Saves/MIRCA2000_cell_calendars_rainfed.csv"
paddy.out = "Saves/parameters_paddy.RDS"
irr.out = "Saves/parameters_irrigated.RDS"
rain.out = "Saves/parameters_rainfed.RDS"

# Load
nc = nc_open(mask.file)
mask = ncvar_get(nc, "mask")
nc_close(nc)
mask = t(mask[,ncol(mask):1])
image.plot(mask)

loc = read.table(loc.file, header = TRUE, stringsAsFactors = F)
cc.paddy = read.csv(file = cc.paddy.file, header = TRUE, stringsAsFactors = F)
cc.irr = read.csv(file = cc.irr.file, header = TRUE, stringsAsFactors = F)
cc.rain = read.csv(file = cc.rain.file, header = TRUE, stringsAsFactors = F)

# Setup
loc = aggregate(formula = cell_ID ~ row + column + lat + lon, data = loc, FUN = mean)
cc.paddy = merge(cc.paddy, loc)
cc.irr = merge(cc.irr, loc)
cc.rain = merge(cc.rain, loc)

create.map = function(d) {
  sel.cols = !colnames(d) %in% c("row", "column")
  map = array(NA, dim = c(dim(mask), sum(sel.cols)))
  
  for(x in 1:dim(mask)[1]){
    for(y in 1:dim(mask)[2]){
      if(is.na(mask[x,y])){
        next
      }
      
      map[x,y,] = 0
    }
  }
  
  for(i in 1:nrow(d)){
    x = d$row[i]
    y = d$column[i]
    
    map[x,y,] = as.numeric(d[i,sel.cols])
  }
  
  t.map = array(NA, dim = c(dim(map)[2], dim(map)[1], dim(map)[3]))
  for(z in 1:dim(map)[3]){
    t.map[,,z] = t(map[dim(map)[1]:1,,z])
  }
  
  return(drop(t.map))
}

create.output = function(cc){
  Fcanopy = create.map(cc[, c("row", "column", paste0("Fcanopy.", 1:12))])
  albedo = create.map(cc[, c("row", "column", paste0("albedo.", 1:12))])
  LAI = create.map(cc[, c("row", "column", paste0("LAI.", 1:12))])
  veg_rough = create.map(cc[, c("row", "column", paste0("veg_rough.", 1:12))])
  displacement = create.map(cc[, c("row", "column", paste0("displacement.", 1:12))])
  Cv = create.map(cc[, c("row", "column", "Cv")])
  root_depth.1 = create.map(cc[, c("row", "column", "root_depth.1")])
  root_depth.2 = create.map(cc[, c("row", "column", "root_depth.2")])
  root_frac.1 = create.map(cc[, c("row", "column", "root_frac.1")])
  root_frac.2 = create.map(cc[, c("row", "column", "root_frac.2")])
  RGL = create.map(cc[, c("row", "column", "RGL")])
  rarc = create.map(cc[, c("row", "column", "rarc")])
  rad_atten = create.map(cc[, c("row", "column", "rad_atten")])
  overstory = create.map(cc[, c("row", "column", "overstory")])
  trunk_ratio = create.map(cc[, c("row", "column", "trunk_ratio")])
  wind_atten = create.map(cc[, c("row", "column", "wind_atten")])
  wind_h = create.map(cc[, c("row", "column", "wind_h")])
  rmin = create.map(cc[, c("row", "column", "rmin")])
  
  out = list(Fcanopy = Fcanopy, 
             albedo = albedo, 
             LAI = LAI, 
             veg_rough = veg_rough, 
             displacement = displacement, 
             Cv = Cv, 
             root_depth.1 = root_depth.1, 
             root_depth.2 = root_depth.2, 
             root_frac.1 = root_frac.1, 
             root_frac.2 = root_frac.2, 
             RGL = RGL, 
             rarc = rarc, 
             rad_atten = rad_atten, 
             overstory = overstory, 
             trunk_ratio = trunk_ratio, 
             wind_atten = wind_atten, 
             wind_h = wind_h, 
             rmin = rmin)
}

plot.output = function(ls){
  image.plot(ls$veg_rough[,,1])
  image.plot(ls$displacement[,,1])
  image.plot(ls$Cv)
  image.plot(ls$root_depth.1)
  image.plot(ls$root_depth.2)
  image.plot(ls$root_frac.1)
  image.plot(ls$root_frac.2)
  image.plot(ls$RGL)
  image.plot(ls$rarc)
  image.plot(ls$rad_atten)
  image.plot(ls$overstory)
  image.plot(ls$trunk_ratio)
  image.plot(ls$wind_atten)
  image.plot(ls$wind_h)
  image.plot(ls$rmin)
  for(m in 1:12){
    image.plot(ls$Fcanopy[,,m], main = m)
  }
  for(m in 1:12){
    image.plot(ls$LAI[,,m], main = m)
  }
  image.plot(ls$albedo[,,1])
}

# Calculate
out.paddy = create.output(cc.paddy)
out.irr = create.output(cc.irr)
out.rain = create.output(cc.rain)

#plot.output(cc.paddy)
#plot.output(cc.irr)
#plot.output(out.rain)

# Save
saveRDS(out.paddy, paddy.out)
saveRDS(out.irr, irr.out)
saveRDS(out.rain, rain.out)
