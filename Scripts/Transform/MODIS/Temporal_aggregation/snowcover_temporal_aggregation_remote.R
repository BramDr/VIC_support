rm(list = ls())

library(rgdal)
library(gdalUtils)
library(raster)

in.dir = "/mnt/annuna/lustre/backup/WUR/ESG/dropp001/MODIS/MOST/MOD10C1.005/"
out.calib.file = "./Saves/snowcover/snowcover_calibration_3min_Indus.RDS"
out.valid.file = "./Saves/snowcover/snowcover_validation_3min_Indus.RDS"
out.file = "./Saves/snowcover/snowcover_3min_Indus.RDS"
out.resolution = 1 / 12
in.resolution = 1 / 20
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

out.global.lons = seq(from = -180 + out.resolution / 2, to = 180 - out.resolution / 2, by = out.resolution)
out.global.lats = seq(from = -90 + out.resolution / 2, to = 90 - out.resolution / 2, by = out.resolution)
out.lons = out.global.lons[out.global.lons <= out.lon.range["max"] & out.global.lons >= out.lon.range["min"]]
out.lats = out.global.lats[out.global.lats <= out.lat.range["max"] & out.global.lats >= out.lat.range["min"]]

in.global.lons = seq(from = -180 + in.resolution / 2, to = 180 - in.resolution / 2, by = in.resolution)
in.global.lats = seq(from = 90 - in.resolution / 2, to = -90 + in.resolution / 2, by = -in.resolution)
in.lons.sel = which(in.global.lons >= min(out.lons) - out.resolution / 2 & in.global.lons <= max(out.lons) + out.resolution / 2)
in.lats.sel = which(in.global.lats >= min(out.lats) - out.resolution / 2 & in.global.lats <= max(out.lats) + out.resolution / 2)

#in.dirs = list.dirs(in.dir, full.names = T)
#in.dirs = grep(x = in.dirs, pattern = "2000.", value = T)
#in.files = c()
#for(in.dir in in.dirs){
#  in.files = c(in.files, list.files(in.dir, pattern = "*.hdf$", full.names = T))
#}
in.files = list.files(in.dir, pattern = "*.hdf$", full.names = T, recursive = T)
in.dates = as.Date(basename(dirname(in.files)), format = "%Y.%m.%d")
in.years = as.numeric(format.Date(in.dates, "%Y"))
in.months = as.numeric(format.Date(in.dates, "%m"))
in.days = as.numeric(format.Date(in.dates, "%d"))
in.doy = as.numeric(format.Date(in.dates, "%j"))

in.years.u = unique(in.years)
calib.years = in.years.u[c(TRUE, FALSE)]
valid.years = in.years.u[c(FALSE, TRUE)]

out.file.sum = gsub(x = out.file, pattern = "_3min", replacement = paste0("_sum_3min"))
out.file.count = gsub(x = out.file, pattern = "_3min", replacement = paste0("_count_3min"))
out.calib.file.sum = gsub(x = out.calib.file, pattern = "_3min", replacement = paste0("_sum_3min"))
out.calib.file.count = gsub(x = out.calib.file, pattern = "_3min", replacement = paste0("_count_3min"))
out.valid.file.sum = gsub(x = out.valid.file, pattern = "_3min", replacement = paste0("_sum_3min"))
out.valid.file.count = gsub(x = out.valid.file, pattern = "_3min", replacement = paste0("_count_3min"))
if(file.exists(out.file.sum) && file.exists(out.calib.file.sum) && file.exists(out.valid.file.sum)){
  print("FILES ALREADY EXIST")
  quit()
}
dir.create(dirname(out.file.sum), recursive = T)
dir.create(dirname(out.calib.file.sum), recursive = T)
dir.create(dirname(out.valid.file.sum), recursive = T)

subdata <- get_subdatasets(in.files[1])
subdata.data = grep(x = subdata, pattern = "Snow_Cover", value = T)

data = as.matrix(readGDAL(subdata.data, silent = T))
data = data[in.lons.sel, in.lats.sel]
data = data[,ncol(data):1]

data.sum = array(0, dim = c(dim(data), max(in.doy)))
data.count = array(0, dim = c(dim(data), max(in.doy)))
data.calib.sum = array(0, dim = c(dim(data), max(in.doy)))
data.calib.count = array(0, dim = c(dim(data), max(in.doy)))
data.valid.sum = array(0, dim = c(dim(data), max(in.doy)))
data.valid.count = array(0, dim = c(dim(data), max(in.doy)))

j = 65
for(j in unique(in.doy)){
  print(j)
  
  data.sum.tmp = data.sum[,,j]
  data.count.tmp = data.count[,,j]
  data.calib.sum.tmp = data.calib.sum[,,j]
  data.calib.count.tmp = data.calib.count[,,j]
  data.valid.sum.tmp = data.valid.sum[,,j]
  data.valid.count.tmp = data.valid.count[,,j]
  
  doy.sel = which(in.doy == j)
  
  if(length(doy.sel) == 0){
    next
  }
  
  s = 1
  for(s in 1:length(doy.sel)){
    idx = doy.sel[s]
    
    print(basename(in.files[idx]))
    
    # Get file info
    subdata <- get_subdatasets(in.files[idx])
    subdata.data = grep(x = subdata, pattern = "Snow_Cover", value = T)
    subdata.quality = grep(x = subdata, pattern = "Spatial_QA", value = T)
    
    # Get file data
    data = as.matrix(readGDAL(subdata.data, silent = T))
    quality = as.matrix(readGDAL(subdata.quality, silent = T))
    data = data[in.lons.sel, in.lats.sel]
    data = data[,ncol(data):1]
    quality = quality[in.lons.sel, in.lats.sel]
    quality = quality[,ncol(quality):1]
    #plot(raster(data))
    #plot(raster(quality))
    
    # Set data weights
    data[data > 100] = NA
    weight = quality == 0
    #plot(raster(weight))
    
    
    # Adjust data based on quality weights
    data = data * weight
    
    # Save data sum and count based on the number of days within the month it is covering
    sel = !is.na(data)
    data.sum.tmp[sel] = data.sum.tmp[sel] + data[sel]
    data.count.tmp[sel] = data.count.tmp[sel] + weight[sel]
    
    if(in.years[idx] %in% calib.years) {
      data.calib.sum.tmp[sel] = data.calib.sum.tmp[sel] + data[sel]
      data.calib.count.tmp[sel] = data.calib.count.tmp[sel] + weight[sel]
    } else if (in.years[idx] %in% valid.years) {
      data.valid.sum.tmp[sel] = data.valid.sum.tmp[sel] + data[sel]
      data.valid.count.tmp[sel] = data.valid.count.tmp[sel] + weight[sel]
    }
  }
  
  data.sum[,,j] = data.sum.tmp
  data.count[,,j] = data.count.tmp
  data.calib.sum[,,j] = data.calib.sum.tmp
  data.calib.count[,,j] = data.calib.count.tmp
  data.valid.sum[,,j] = data.valid.sum.tmp
  data.valid.count[,,j] = data.valid.count.tmp
}

data.avg = data.sum / data.count
data.avg[data.count <= 3] = NA
data.calib.avg = data.calib.sum / data.calib.count
data.calib.avg[data.calib.count <= 3] = NA
data.valid.avg = data.valid.sum / data.valid.count
data.valid.avg[data.valid.count <= 3] = NA

plot(raster(data.avg[,,315]), main = "total")
plot(raster(data.calib.avg[,,315]), main = "calibration")
plot(raster(data.valid.avg[,,315]), main = "validation")

saveRDS(object = data.sum, file = out.file.sum)
saveRDS(object = data.count, file = out.file.count)
saveRDS(object = data.calib.sum, file = out.calib.file.sum)
saveRDS(object = data.calib.count, file = out.calib.file.count)
saveRDS(object = data.valid.sum, file = out.valid.file.sum)
saveRDS(object = data.valid.count, file = out.valid.file.count)

