rm(list = ls())
library(raster)
library(rgdal)
library(gdalUtils)
library(Rcpp)

tile.file = "../tileset_indus.txt"
height.file = "../../../../Data/Primary/Simard2011/Simard_Pinto_3DGlobalVeg_JGR.tif"
mapping.out = "./Saves/height_mapping_500m.RDS"
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)
sourceCpp("./Rcpp_support.cpp")

tiles = read.table(tile.file, stringsAsFactors = F)

height = raster(height.file)
height.resolution = 360 / dim(height)[2]
height = crop(height,
              extent(out.lon.range["min"], 
                     out.lon.range["max"], 
                     out.lat.range["min"], 
                     out.lat.range["max"]))
height.lons = seq(from = extent(height)[1] + height.resolution / 2, 
                  to = extent(height)[2] - height.resolution / 2, 
                  by = height.resolution)
height.lats = seq(from = extent(height)[3] + height.resolution / 2, 
                  to = extent(height)[4] - height.resolution / 2, 
                  by = height.resolution)

height = as.matrix(height)
height[height == 0] = NA
# plot(raster(height))

tile = tiles[1,1]
for (tile in unique(tiles[,1])){
  print(tile)
  
  h = gsub(tile, pattern = "h", replacement = "")
  h = gsub(h, pattern = "v.*", replacement = "")
  h = as.numeric(h)
  v = gsub(tile, pattern = ".*v", replacement = "")
  v = as.numeric(v)
  
  npix_x = npix_y = 2400
  npix_per_deg_y = floor(npix_y/10)
  
  tile.info = modis_sinusoidal_to_latlon(h = h, v = v,
                                           npix_per_deg_y = npix_per_deg_y,
                                           row = 1, col = 1)
  lat.min = tile.info["latcenter"] - tile.info["cellsize_y"]
  lat.max = tile.info["latcenter"] + tile.info["cellsize_y"]
  lon.min = tile.info["loncenter"] - tile.info["cellsize_x"]
  lon.max = tile.info["loncenter"] + tile.info["cellsize_x"]
  #print(paste(lon.min, lon.max))
  tile.info = modis_sinusoidal_to_latlon(h = h, v = v,
                                           npix_per_deg_y = npix_per_deg_y,
                                           row = 1, col = npix_x)
  lat.min = min(lat.min, tile.info["latcenter"] - tile.info["cellsize_y"])
  lat.max = max(lat.max, tile.info["latcenter"] + tile.info["cellsize_y"])
  lon.min = min(lon.min, tile.info["loncenter"] - tile.info["cellsize_x"])
  lon.max = max(lon.max, tile.info["loncenter"] + tile.info["cellsize_x"])
  #print(paste(lon.min, lon.max))
  tile.info = modis_sinusoidal_to_latlon(h = h, v = v,
                                           npix_per_deg_y = npix_per_deg_y,
                                           row = npix_y, col = 1)
  lat.min = min(lat.min, tile.info["latcenter"] - tile.info["cellsize_y"])
  lat.max = max(lat.max, tile.info["latcenter"] + tile.info["cellsize_y"])
  lon.min = min(lon.min, tile.info["loncenter"] - tile.info["cellsize_x"])
  lon.max = max(lon.max, tile.info["loncenter"] + tile.info["cellsize_x"])
  #print(paste(lon.min, lon.max))
  tile.info = modis_sinusoidal_to_latlon(h = h, v = v,
                                           npix_per_deg_y = npix_per_deg_y,
                                           row = npix_y, col = npix_x)
  lat.min = min(lat.min, tile.info["latcenter"] - tile.info["cellsize_y"])
  lat.max = max(lat.max, tile.info["latcenter"] + tile.info["cellsize_y"])
  lon.min = min(lon.min, tile.info["loncenter"] - tile.info["cellsize_x"])
  lon.max = max(lon.max, tile.info["loncenter"] + tile.info["cellsize_x"])
  #print(paste(lon.min, lon.max))
  
  print(paste0("tile size: ", lon.min, "; ", lon.max, "; ", lat.min, "; ", lat.max))
  
  height.lons.tmp = height.lons[height.lons <= lon.max & height.lons >= lon.min]
  height.lats.tmp = height.lats[height.lats <= lat.max & height.lats >= lat.min]
  
  height.lons.start = which(height.lons == min(height.lons.tmp)) - 1
  height.lats.start = which(height.lats == min(height.lats.tmp)) - 1
  
  height.lons.add = which(height.lons == max(height.lons.tmp))
  height.lats.add = which(height.lats == max(height.lats.tmp))
  height.lons.add = length(height.lons) - height.lons.add
  height.lats.add = length(height.lats) - height.lats.add
  
  # height.lats.start + height.lats.add + length(height.lats.tmp)
  # length(height.lats)
  # height.lons.start + height.lons.add + length(height.lons.tmp)
  # length(height.lons)
    
  mapping = modis_to_output_mapping(h = h, v = v,
                                    npix_per_deg_y = npix_per_deg_y,
                                    npix_x = npix_x,
                                    npix_y = npix_y,
                                    output_lats = height.lats.tmp,
                                    output_lons = height.lons.tmp,
                                    output_resolution = height.resolution,
                                    output_lats_start = height.lats.start,
                                    output_lons_start = height.lons.start,
                                    output_lats_add = height.lats.add,
                                    output_lons_add = height.lons.add)
  
  # plot(raster(t(mapping)))
  
  mapping.out.tmp = gsub(mapping.out, pattern = "_500m", replacement = paste0("_", tile, "_500m"))
  dir.create(dirname(mapping.out.tmp))
  saveRDS(mapping, mapping.out.tmp)
}
