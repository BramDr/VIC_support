library(fields)
library(raster)
rm(list = ls())

soil.dir = "../../../../Data/Primary/WISE/wise_30sec_v1/Interchangeable_format"
map.file = "/../../../../Data/Primary/WISE/wise_30sec_v1/Interchangeable_format/wise_30sec_v1.tif"
mapping.file = "/../../../../Data/Primary/WISE/wise_30sec_v1/Interchangeable_format/wise_30sec_v1.tsv"
out.clay = "./Saves/clay_30min_global.RDS"
out.sand = "./Saves/sand_30min_global.RDS"
out.silt = "./Saves/silt_30min_global.RDS"
out.bulk = "./Saves/bulk_30min_global.RDS"

extent.isel = extent(-4.75, 8.25, 42.75, 54.75)
extent.out = extent(-180, 180, -90, 90)

soil.files = list.files(soil.dir, pattern = "_wD[0-9]", full.names = T)
map = raster(map.file)
map = crop(map, extent.isel)
extent.sel = extent(map)
map = as.matrix(map)
mapping = read.table(mapping.file, header = T, stringsAsFactors = F)

get.char = function(map, mapping, soil, var) {
  newmap = array(NA, dim = dim(map))
  
  map.u = unique(c(map))
  for(i in 1:length(map.u)){
    desc = mapping$description[mapping$pixel_vaue == map.u[i]]
    
    if(desc == "nodata"){
      next
    }
    
    row = which(soil$NEWSUID == desc)
    newmap[map == map.u[i]] = soil[row, var]
  }
  return(newmap)
}
agg.map = function(map, factor){
  r = raster(map)
  extent(r) = extent.sel
  r.agg = aggregate(r, fact = factor, fun = mean, na.rm = T)
  r.agg = extend(r.agg, extent.out)
  return(as.matrix(r.agg))
}

for(i in 1:length(soil.files)){
  soil.file = soil.files[i]
  print(soil.file)
  soil = read.table(soil.file, sep = ",", header = T, stringsAsFactors = F)
  
  layer = gsub(x = basename(soil.file), pattern = ".txt", replacement = "")
  layer = gsub(x = layer, pattern = ".*wD", replacement = "")
  layer = as.numeric(layer)
  
  clay = get.char(map, mapping, soil, "CLPC")
  sand = get.char(map, mapping, soil, "SDTO")
  silt = get.char(map, mapping, soil, "STPC")
  bulk = get.char(map, mapping, soil, "BULK")
  
  clay.agg = agg.map(clay,60)
  sand.agg = agg.map(sand,60)
  silt.agg = agg.map(silt,60)
  bulk.agg = agg.map(bulk,60)
  
  extent.print = paste0(extent.isel[1], "_", extent.isel[2], "_", extent.isel[3], "_", extent.isel[4])
  
  out.clay.tmp = gsub(out.clay, pattern = "_30min", replacement = paste0("_", layer, "_", extent.print, "_30min"))
  out.sand.tmp = gsub(out.sand, pattern = "_30min", replacement = paste0("_", layer, "_", extent.print, "_30min"))
  out.silt.tmp = gsub(out.silt, pattern = "_30min", replacement = paste0("_", layer, "_", extent.print, "_30min"))
  out.bulk.tmp = gsub(out.bulk, pattern = "_30min", replacement = paste0("_", layer, "_", extent.print, "_30min"))
  
  dir.create(dirname(out.clay.tmp))
  dir.create(dirname(out.sand.tmp))
  dir.create(dirname(out.silt.tmp))
  dir.create(dirname(out.bulk.tmp))
  
  saveRDS(clay.agg, out.clay.tmp)
  saveRDS(sand.agg, out.sand.tmp)
  saveRDS(silt.agg, out.silt.tmp)
  saveRDS(bulk.agg, out.bulk.tmp)
}
