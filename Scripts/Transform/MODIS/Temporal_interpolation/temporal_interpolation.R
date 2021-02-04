rm(list = ls())
library(fields)
library(zoo)

cv.file = "../../../../Data/Transformed/MODIS/Cv_5min_Indus.RDS"
in.files = c("../Spatial_aggregation/Saves/LAI_5min_Indus.RDS",
             "../Spatial_aggregation/Saves/NDVI_5min_Indus.RDS",
             "../Spatial_aggregation/Saves/albedo_5min_Indus.RDS")
in.files = c("../Spatial_aggregation/Saves/LAI_5min_Indus.RDS",
             "../Spatial_aggregation/Saves/NDVI_5min_Indus.RDS")
out.dir = "./Saves"

cv = readRDS(cv.file)

in.file = in.files[2]
for(in.file in in.files) {
  print(basename(in.file))
  
  data = readRDS(in.file)

  x = 106
  y = 140
  z = 13
  data.adj = data
  for(x in 1:dim(cv)[1]){
    for(y in 1:dim(cv)[2]){
      for(z in 1:dim(cv)[3]){
        if(cv[x,y,z] <= 0){
          next
        }
        
        data.monthly = data[x,y,z,]
        nmissing = sum(is.na(data.monthly))
        
        if(nmissing > 0 && nmissing < length(data.monthly)){
          missing.idx = which(is.na(data.monthly))
          
          data.monthly.adj = data.monthly
          change = F
          for(m in missing.idx){
            m.prev = m - 1
            m.next = m + 1
            if(m.prev < 1) { m.prev = m.prev + 12 }
            if(m.next > 12) { m.next = m.next - 12 }
            
            if(is.na(data.monthly[m.prev]) || is.na(data.monthly[m.next])){
              next
            }
            
            data.monthly.adj[m] = (data.monthly[m.prev] + data.monthly[m.next]) / 2
            change = T
          }
          data.adj[x,y,z,] = data.monthly.adj
          
          if(change){
            plot(data[x,y,z,], type = "l", main = paste0(basename(in.file), ": ", x, " ; ", y, " ; ", z))
            points(data[x,y,z,])
            lines(data.adj[x,y,z,], col = "red", lty = 2)
            points(data.adj[x,y,z,], col = "red", pch = 2)
          }
        }
      }
    }
  }
  
  out.file = paste0(out.dir, "/", basename(in.file))
  dir.create(dirname(out.file))
  saveRDS(data.adj, out.file)
}
