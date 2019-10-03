library(ncdf4)
library(fields)
rm(list = ls())

depth.file = "/mnt/hgfs/Shared Workspace/ISIMIP/lakes/lakedepth.nc"
area.file = "/home/bram/Data/VIC/parameters/global/domain_global.nc"
frac.file = "/mnt/hgfs/Shared Workspace/ISIMIP/lakes/pctlake.nc"
check.file = "/mnt/hgfs/Shared Workspace/ISIMIP/lakes/lakemask.nc"

char.out = "/home/bram/Projects/VICsupport/Lakes/Saves/global_lake_chars.csv"

nc = nc_open(area.file)
garea = ncvar_get(nc, "area")
garea = garea[,ncol(garea):1]
nc_close(nc)

nc = nc_open(depth.file)
ldepth = ncvar_get(nc, "LAKEDEPTH")
nc_close(nc)

nc = nc_open(frac.file)
lfrac = ncvar_get(nc, "PCT_LAKE")
nc_close(nc)

image.plot(lfrac > 0)

chars = data.frame(ID = numeric(), depth = numeric(), area = numeric(), elevation = numeric(), lon = numeric(), lat = numeric())
id.counter = 1
for(x in 1:dim(lfrac)[1]){
  for(y in 1:dim(lfrac)[2]){
    if(lfrac[x,y] <= 0){
      next
    }
    
    id = id.counter
    depth = ldepth[x,y]
    area = garea[x,y] * (lfrac[x,y] / 100)
    elevation = 0
    lon = nc$dim$lsmlon$vals[x]
    lat = nc$dim$lsmlat$vals[y]
      
    chars[nrow(chars) + 1,] = c(id, depth, area, elevation, lon, lat)
    id.counter = id.counter + 1
  }
}

write.csv(x = chars, file = char.out, row.names = F)
