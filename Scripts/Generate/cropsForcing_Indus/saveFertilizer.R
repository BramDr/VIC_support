library(fields)
library(raster)
library(ncdf4)
rm(list = ls())

fert.dir <- "../../../../Data/Primary/GGCMI/Phase3/n-fertilizer/histsoc"
crop.file <- "./Saves/crop_mapping.RDS"
fertilizer.out <- "./Saves/fertilizer_5min_Indus.RDS"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
crops <- readRDS(crop.file)
fert.files <- list.files(fert.dir, pattern = "fertilizer_application", full.names = T)
man.file <- list.files(fert.dir, pattern = "manure_application", full.names = T)

nc <- nc_open(man.file)
nc.years <- nc$dim$time$vals
nc.lats = nc$dim$lat$vals
nc.lons = nc$dim$lon$vals
man.n <- ncvar_get(nc, nc$var$manurerate)
nc_close(nc)

# Setup
sel.lats = nc.lats >= min(out.lats) - resolution / 2 & nc.lats <= max(out.lats) + resolution / 2 
sel.lons = nc.lons >= min(out.lons) - resolution / 2 & nc.lons <= max(out.lons) + resolution / 2
man.n.sel = man.n[sel.lons, sel.lats,]
man.n.sel = man.n.sel[,dim(man.n.sel)[2]:1,]
man.n.sel[is.na(man.n.sel)] = 0
image.plot(man.n.sel[,,10])
nferttimes = 4

#orig.map = fertilizer.n
#new.lons = out.lons
#new.lats = out.lats
disaggregate.maps = function(orig.map, new.lons, new.lats){
  factor = length(new.lons) / dim(orig.map)[1]
  new.map = array(NA, dim = c(length(new.lons), length(new.lats), dim(orig.map)[3:length(dim(orig.map))]))
  
  for(y in 1:dim(orig.map)[3]){
    for(c in 1:dim(orig.map)[4]){
      for(f in 1:dim(orig.map)[5]){
        for(x in 1:dim(new.map)[1]){
          x.orig = floor((x - 1) / factor) + 1
          new.diss = rep(orig.map[x.orig,,y,c,f], each = factor)
          new.map[x,,y,c,f] = new.diss
        }
      }
    }
  }
  
  return(new.map)
}

# Calculate
fertilizer.n <- array(0, dim = c(dim(man.n.sel), nrow(crops), nferttimes))
fertilizer.p <- array(0, dim = c(dim(man.n.sel), nrow(crops), nferttimes))
fertilizer.k <- array(0, dim = c(dim(man.n.sel), nrow(crops), nferttimes))
fertilizer.dvs <- array(0, dim = c(dim(man.n.sel), nrow(crops), nferttimes))

fertilizer.dvs[,,,,1] = 0
fertilizer.dvs[,,,,2] = 0.25
fertilizer.dvs[,,,,3] = 0.5
fertilizer.dvs[,,,,4] = 0.75

i <- 1
for (i in 1:nrow(crops)) {
  print(crops$name[i])

  if (is.na(crops$fertilizer[i])) {
    next
  }

  fert.file <- grep(fert.files, pattern = paste0("_", crops$fertilizer[i], "_"), value = T)

  nc <- nc_open(fert.file)
  fert.n <- ncvar_get(nc, nc$var$fertrate)
  nc_close(nc)
  fert.n.sel = fert.n[sel.lons, sel.lats,]
  fert.n.sel = fert.n.sel[,dim(fert.n.sel)[2]:1,]
  
  fert.n.year = (fert.n.sel + man.n.sel)
  fertilizer.n[, , , i, 1] <- fert.n.year * 0.25
  fertilizer.n[, , , i, 2] <- fert.n.year * 0.25
  fertilizer.n[, , , i, 3] <- fert.n.year * 0.25
  fertilizer.n[, , , i, 4] <- fert.n.year * 0.25
}

fertilizer.n.adj = disaggregate.maps(fertilizer.n, out.lons, out.lats)
fertilizer.p.adj = disaggregate.maps(fertilizer.p, out.lons, out.lats)
fertilizer.k.adj = disaggregate.maps(fertilizer.k, out.lons, out.lats)
fertilizer.dvs.adj = disaggregate.maps(fertilizer.dvs, out.lons, out.lats)
dimnames(fertilizer.n.adj)[[3]] = nc.years
dimnames(fertilizer.p.adj)[[3]] = nc.years
dimnames(fertilizer.k.adj)[[3]] = nc.years
dimnames(fertilizer.dvs.adj)[[3]] = nc.years

image.plot(fertilizer.n.adj[,,100,1,1])

# Save
fertilizer.out.tmp <- gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = "fertilizerDVS_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.dvs.adj, fertilizer.out.tmp)
fertilizer.out.tmp <- gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = "fertilizerN_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.n.adj, fertilizer.out.tmp)
fertilizer.out.tmp <- gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = "fertilizerP_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.p.adj, fertilizer.out.tmp)
fertilizer.out.tmp <- gsub(x = fertilizer.out, pattern = "fertilizer_", replacement = "fertilizerK_")
dir.create(dirname(fertilizer.out.tmp))
saveRDS(fertilizer.k.adj, fertilizer.out.tmp)
