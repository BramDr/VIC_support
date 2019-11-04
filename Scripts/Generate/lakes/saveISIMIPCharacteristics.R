library(ncdf4)
library(fields)
rm(list = ls())

# Input
depth.file = "../../../Data/Primary/ISIMIP/LakeData/lakedepth.nc"
area.file = "../../../Data/Primary/VIC/domain_global.nc"
frac.file = "../../../Data/Primary/ISIMIP/LakeData/pctlake.nc"
mask.file = "../../../Data/Primary/VIC/domain_global.nc"
char.out = "Saves/lakes_isimip_chars.csv"

# Load
nc = nc_open(mask.file)
mask = ncvar_get(nc, nc$var$mask)
nc_close(nc)

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

# Calculate
chars = data.frame(ID = numeric(), sourceID = numeric(), depth = numeric(), area = numeric(), elevation = numeric(), lon = numeric(), lat = numeric())
id.counter = 0
for(x in 1:dim(lfrac)[1]){
  for(y in 1:dim(lfrac)[2]){
    if(lfrac[x,y] <= 0){
      next
    }
    
    id = id.counter
    sourceid = id.counter
    depth = ldepth[x,y]
    area = garea[x,y] * (lfrac[x,y] / 100)
    elevation = 0
    lon = nc$dim$lsmlon$vals[x]
    lat = nc$dim$lsmlat$vals[y]
      
    chars[nrow(chars) + 1,] = c(id, sourceid, depth, area, elevation, lon, lat)
    id.counter = id.counter + 1
  }
}

# Save
dir.create(dirname(char.out))
write.csv(x = chars, file = char.out, row.names = F)
