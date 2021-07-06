rm(list = ls())

library(rgdal)
library(gdalUtils)
library(raster)
library(Rcpp)

tile.file = "../tileset_indus.txt"
in.dir = "/mnt/annuna/lustre/backup/WUR/ESG/dropp001/Backup/MODIS/MOTA/MCD43A3.006/"
lai.dir = "/mnt/annuna/lustre/backup/WUR/ESG/dropp001/Backup/MODIS/MOTA/MCD15A2H.006/"
out.file = "./Saves/albedo/albedo_500m.RDS"
days.step = 8
days.per.month = c(31,28,31,30,31,30,31,31,30,31,30,31)
sourceCpp("./Rcpp_support.cpp")

tiles = read.table(tile.file, stringsAsFactors = F)

in.files = list.files(in.dir, pattern = paste0(".*hdf$"), recursive = T, full.names = T)
in.dates = as.Date(basename(dirname(in.files)), format = "%Y.%m.%d")
in.years = as.numeric(format.Date(in.dates, "%Y"))
in.months = as.numeric(format.Date(in.dates, "%m"))
in.days = as.numeric(format.Date(in.dates, "%d"))
in.doy = as.numeric(format.Date(in.dates, "%j"))

lai.files = list.files(lai.dir, pattern = paste0(".*hdf$"), recursive = T, full.names = T)
lai.dates = as.Date(basename(dirname(lai.files)), format = "%Y.%m.%d")
lai.years = as.numeric(format.Date(lai.dates, "%Y"))
lai.months = as.numeric(format.Date(lai.dates, "%m"))
lai.days = as.numeric(format.Date(lai.dates, "%d"))
lai.doy = as.numeric(format.Date(lai.dates, "%j"))
 
tile = tiles[6,1]
for(tile in tiles[,1]){
  print(tile)
  
  out.file.sum = gsub(x = out.file, pattern = "_500m", replacement = paste0("_sum_", tile, "_500m"))
  out.file.count = gsub(x = out.file, pattern = "_500m", replacement = paste0("_count_", tile, "_500m"))
  
  if(file.exists(out.file.sum) && file.exists(out.file.count)){
    next
  }
  dir.create(dirname(out.file.sum), recursive = T)
  dir.create(dirname(out.file.count), recursive = T)
  
  tile.sel = grep(in.files, pattern = tile)
  lai.tile.sel = grep(lai.files, pattern = tile)
  if(length(tile.sel) == 0 || length(lai.tile.sel) == 0){
    quit("TILE NOT FOUND")
  }
  
  subdata <- get_subdatasets(in.files[tile.sel[1]])
  subdata.data = grep(x = subdata, pattern = "Albedo_WSA_shortwave", value = T)
  data = as.matrix(readGDAL(subdata.data, silent = T))
  
  data.sum = array(0, dim = c(dim(data), length(unique(in.months))))
  data.count = array(0, dim = c(dim(data), length(unique(in.months))))
  
  m = 5
  for(m in unique(in.months)){
    print(m)
    
    m.next = m + 1
    if(m.next > 12){
      m.next = m.next - 12
    }
    
    data.sum.tmp = data.sum[,,m]
    data.count.tmp = data.count[,,m]
    data.sum.next.tmp = data.sum[,,m.next]
    data.count.next.tmp = data.count[,,m.next]
    
    month.sel = which(in.months == m)
    month.sel = intersect(tile.sel, month.sel)
    
    lai.month.sel = which(lai.months == m)
    lai.month.sel = intersect(lai.tile.sel, lai.month.sel)
    
    if(length(month.sel) == 0){
      next
    }
    
    s = 1
    for(s in 1:length(month.sel)){
      idx = month.sel[s]
      
      lai.date.sel = which(lai.dates == in.dates[idx])
      if(length(lai.date.sel) == 0){
        print("LAI DATE NOT FOUND")
        next
      }
      
      lai.idx = intersect(lai.month.sel, lai.date.sel)
      if(length(lai.idx) == 0){
        print("LAI DATE NOT FOUND")
        next
      }
      
      print(basename(in.files[idx]))
      
      # Calculate days in month
      days.next = in.days[idx] + days.step - 1 - days.per.month[in.months[idx]]
      if(days.next < 0){
        days.next = 0
      }
      days.current = days.step - days.next
      
      # Get file info
      subdata <- get_subdatasets(in.files[idx])
      subdata.data = grep(x = subdata, pattern = "Albedo_WSA_shortwave", value = T)
      subdata.quality = grep(x = subdata, pattern = "Quality_shortwave", value = T)
      
      subdata <- get_subdatasets(lai.files[lai.idx])
      subdata.qc1 = grep(x = subdata, pattern = "Lai_QC", value = T)
      subdata.qc2 = grep(x = subdata, pattern = "Extra_QC", value = T)
      
      # Get file data
      data = as.matrix(readGDAL(subdata.data, silent = T))
      quality = as.matrix(readGDAL(subdata.quality, silent = T))
      qc1 = as.matrix(readGDAL(subdata.qc1, silent = T))
      qc2 = as.matrix(readGDAL(subdata.qc2, silent = T))
      #plot(raster(data))
      #plot(raster(quality))
      #plot(raster(qc1))
      #plot(raster(qc2))
      
      # Set data weights
      weight = get_LAI_QC_weight_quality(qc1,qc2)
      #plot(raster(weight))
      weight[is.na(quality)] = 0 # not processed
      weight[!is.na(quality) & quality != 0 & weight > 0.15] = 0.15 # poor quality
      #plot(raster(weight))
      
      # Adjust data based on quality weights
      data = data * weight
      
      # Save data sum and count based on the number of days within the month it is covering
      sel = !is.na(data)
      data.sum.tmp[sel] = data.sum.tmp[sel] + data[sel] * (days.current / days.step)
      data.count.tmp[sel] = data.count.tmp[sel] + weight[sel] * (days.current / days.step)
      data.sum.next.tmp[sel] = data.sum.next.tmp[sel] + data[sel] * (days.next / days.step)
      data.count.next.tmp[sel] = data.count.next.tmp[sel] + weight[sel] * (days.next / days.step)
    }
    
    data.sum[,,m] = data.sum.tmp
    data.count[,,m] = data.count.tmp
    data.sum[,,m.next] = data.sum.next.tmp
    data.count[,,m.next] = data.count.next.tmp
  }
  
  saveRDS(object = data.sum, file = out.file.sum)
  saveRDS(object = data.count, file = out.file.count)
}
  
