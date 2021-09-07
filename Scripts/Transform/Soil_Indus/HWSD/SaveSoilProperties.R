rm(list = ls())
library(fields)
library(raster)

raster.file = "/home/bram/Data/Primary/HWSD/HWSD_RASTER/hwsd.bil"
mapping.file = "/home/bram/Data/Primary/HWSD/HWSD_mapping_data.csv"
table.file = "/home/bram/Data/Primary/HWSD/HWSD_data.csv"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

extent.out <- extent(min(out.lons) - resolution / 2, 
                     max(out.lons) + resolution / 2, 
                     min(out.lats) - resolution / 2, 
                     max(out.lats) + resolution / 2)

# Load
raster = raster(raster.file)
raster = crop(raster, extent.out)
raster[raster == 0] = NA
raster = as.matrix(raster)
raster = t(raster[nrow(raster):1,])
image.plot(raster)

table = read.csv(table.file)
mapping = read.csv(mapping.file)

# Setup
transform.soil.data = function(table, raster, name, count = F) {
  data.map = array(NA, dim = dim(raster))
  #id = 3672
  #name = "T_USDA_TEX_CLASS"
  #count = T
  for(id in unique(na.omit(c(raster)))){
    table.soil = table[table$MU_GLOBAL == id,]
    table.soil = table.soil[table.soil$ISSOIL == 1,]
    
    if(nrow(table.soil) == 0){
      data.soil = NA
    } else {
      if(!count){
        data.soil = sum(table.soil[,name] * table.soil$SHARE, na.rm = T) / sum(table.soil$SHARE, na.rm = T)
      } else {
        for(i in 1:nrow(table.soil)){
          data.soil = table.soil[i,name]
          if(!is.na(data.soil)){
            break
          }
        }
      }
    }
    data.map[raster == id] = data.soil
  }
  #image.plot(data.map, zlim = c(1,13))
  #print(unique(c(data.map)))
  
  if(!count){
    data.map.r = raster(data.map)
    data.map.agg = aggregate(data.map.r, fact = 10, fun = mean, na.rm = T)
    data.map.agg = as.matrix(data.map.agg)
    #image.plot(data.map.agg)
  } else {
    count.fun = function(x, na.rm){
      x = na.omit(x)
      if(length(x) == 0){
        return(NA)
      }
      
      val <- table(x)
      idx <- order(val, decreasing = T)[1]
      val <- as.numeric(names(val)[idx])
        
      return(val)
    }
    
    data.map.r = raster(data.map)
    data.map.agg = aggregate(data.map.r, fact = 10, fun = count.fun, na.rm = T)
    data.map.agg = as.matrix(data.map.agg)
  }
  
  #image.plot(data.map.agg, zlim = c(1,13))
  #print(unique(c(data.map.agg)))
  return(data.map.agg)
}

store.transform.soil.data = function(table, raster, name, out.lons, out.lats, out.name, count = F){
  soil.data = array(NA, dim = c(length(out.lons), length(out.lats), 3))
  soil.data[,,1] = transform.soil.data(table, raster, paste0("T_", name), count)
  soil.data[,,2:3] = transform.soil.data(table, raster, paste0("S_", name), count)
  
  dir.create(dirname(out.name))
  saveRDS(soil.data, out.name)
  
  return(soil.data)
}

sand = store.transform.soil.data(table, raster, "SAND", out.lons, out.lats, "./Saves/sand_5min_Indus.RDS")
image.plot(sand[,,1])
image.plot(sand[,,2])
clay = store.transform.soil.data(table, raster, "CLAY", out.lons, out.lats, "./Saves/clay_5min_Indus.RDS")
image.plot(clay[,,1])
image.plot(clay[,,2])
silt = store.transform.soil.data(table, raster, "SILT", out.lons, out.lats, "./Saves/silt_5min_Indus.RDS")
image.plot(silt[,,1])
image.plot(silt[,,2])
bulk = store.transform.soil.data(table, raster, "BULK_DENSITY", out.lons, out.lats, "./Saves/bulk_5min_Indus.RDS") * 1e3
image.plot(bulk[,,1])
image.plot(bulk[,,2])
carbon = store.transform.soil.data(table, raster, "OC", out.lons, out.lats, "./Saves/carbon_5min_Indus.RDS")
image.plot(carbon[,,1])
image.plot(carbon[,,2])
ph = store.transform.soil.data(table, raster, "PH_H2O", out.lons, out.lats, "./Saves/ph_5min_Indus.RDS")
image.plot(ph[,,1])
image.plot(ph[,,2])
usda = store.transform.soil.data(table, raster, "USDA_TEX_CLASS", out.lons, out.lats, "./Saves/usda_5min_Indus.RDS", count = T)
image.plot(usda[,,1], zlim = c(1,13))
image.plot(usda[,,2], zlim = c(1,13))

