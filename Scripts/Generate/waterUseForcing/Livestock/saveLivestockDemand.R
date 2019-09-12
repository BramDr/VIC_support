library(fields)
library(ncdf4)
rm(list = ls())

# Input
liv.file <- "../../../../Data/Transformed/Livestock/livestockCount_30min_global.csv"
meteo.dir <- "../../../../Data/Transformed/WFDEI/"
mask.file <- "../../../../Data/Primary/VIC/domain_global.nc"
area.file <- "../../../../Data/Transformed/Routing/area_30min_global.RDS"
liv.out <- "Saves/livestockDemand_30min_global.RDS"
years <- 1979:2016

# Load
meteo.files <- list.files(meteo.dir, pattern = ".*Tair_monthly_.*.nc", full.names = T)

liv <- read.csv(liv.file, stringsAsFactors = F)
area <- readRDS(area.file)

nc <- nc_open(mask.file)
mask <- ncvar_get(nc, "mask")
nc_close(nc)

# Setup
lats <- seq(-89.75, 89.75, by = 0.5)
lons <- seq(-179.75, 179.75, by = 0.5)

liv$ct.s = (2.333 * 1e9 / sum(liv$ct) / 365) * liv$ct
liv$bf.s = (2.333 * 1e9 / sum(liv$ct) / 365) * liv$bf
liv$ho.s = (2.333 * 1e9 / sum(liv$ct) / 365) * liv$ho
liv$pg.s = (4.163 * 1e9 / sum(liv$pg) / 365) * liv$pg
liv$sh.s = (4.163 * 1e9 / sum(liv$pg) / 365) * liv$sh
liv$gt.s = (4.163 * 1e9 / sum(liv$pg) / 365) * liv$gt
liv$ch.s = (0.046 * 1e9 / sum(liv$ch) / 100 / 365) * liv$ch
liv$dk.s = (0.046 * 1e9 / sum(liv$ch) / 100 / 365) * liv$dk

liv$ct.mean = (11.400 * 1e9 / sum(liv$ct) / 365) * liv$ct
liv$ho.mean = (11.400 * 1e9 / sum(liv$ct) / 365) * liv$ho
liv$bf.mean = (1.360 * 1e9 / sum(liv$ct) / 365) * liv$ct
liv$gt.mean = (0.770 * 1e9 / sum(liv$ct) / 365) * liv$ct
liv$sh.mean = (1.110 * 1e9 / sum(liv$ct) / 365) * liv$ct
liv$pg.mean = (0.690 * 1e9 / sum(liv$ct) / 365) * liv$ct
liv$ch.mean = (0.930 * 1e9 / sum(liv$ct) / 100 / 365) * liv$ct
liv$dk.mean = (0.930 * 1e9 / sum(liv$ct) / 100 / 365) * liv$dk

liv$ct.min = (73.2 - 44.1) / 10 / 73.2
liv$ho.min = (73.2 - 44.1) / 10 / 73.2
liv$bf.min = (73.2 - 44.1) / 10 / 73.2
liv$gt.min = (9.6 - 7.6) / 10 / 9.6
liv$sh.min = (12.9 - 8.7) / 10 / 12.9
liv$pg.min = (28.3 - 17.2) / 10 / 28.3
liv$ch.min = (25.8 - 13.2) / 10 / 25.8
liv$dk.min = (25.8 - 13.2) / 10 / 25.8

liv$ct.max = (102.3 - 73.2) / 10 / 73.2
liv$ho.max = (102.3 - 73.2) / 10 / 73.2
liv$bf.max = (102.3 - 73.2) / 10 / 73.2
liv$gt.max = (11.9 - 9.6) / 10 / 9.6
liv$sh.max = (20.1 - 12.9) / 10 / 12.9
liv$pg.max = (46.7 - 28.3) / 10 / 28.3
liv$ch.max = (50.5 - 25.8) / 10 / 25.8
liv$dk.max = (50.5 - 25.8) / 10 / 25.8

# Calculate
maps = list()
for(var in c("ct", "bf", "ho", "gt", "sh", "pg", "ch", "dk")){
  high.map = array(NA, dim = c(length(lons), length(lats)))
  low.map = array(NA, dim = c(length(lons), length(lats)))
  drink.map = array(NA, dim = c(length(lons), length(lats)))
  service.map = array(NA, dim = c(length(lons), length(lats)))
  
  for(i in 1:nrow(liv)){
    x = which(lons == liv$lon[i])
    y = which(lats == liv$lat[i])
    
    high.map[x,y] = liv[,paste0(var, ".max")][i]
    low.map[x,y] = liv[,paste0(var, ".min")][i]
    drink.map[x,y] = liv[,paste0(var, ".mean")][i]
    service.map[x,y] = liv[,paste0(var, ".s")][i]
  }
  
  liv.maps = list(high = high.map, low = low.map, drink = drink.map, service = service.map)
  maps[[var]] = liv.maps
}
image.plot(maps[[1]]$high)
image.plot(maps[[1]]$low)
image.plot(maps[[1]]$drink)
image.plot(maps[[1]]$service)

liv.dem = array(0, dim = c(length(lons), length(lats),length(years) * 12))
for(z in 1:length(years)){
  year = years[z]
  print(paste0("Working on year ",year))
  
  meteo.file = grep(meteo.files, pattern = year, value = T)
  
  nc = nc_open(meteo.file)
  tair = ncvar_get(nc, "Tair")
  nc_close(nc)
  
  tair = tair - 273.15
  tair[tair < 15] = 15
  tair[tair > 35] = 35
  
  for(l in 1:length(maps)){
    print(paste0("Variety ", names(maps)[l]))
    
    liv.maps = maps[[l]]
    
    drink.map = array(NA, dim = c(length(lons), length(lats)))
    for(m in 1:12){
      tair.m = tair[,,m]
      
      sel = !is.na(tair.m) & tair.m >= 25
      drink.map[sel] = liv.maps$drink[sel] + liv.maps$drink[sel] * liv.maps$high[sel] * (tair.m[sel] - 25)
      
      sel = !is.na(tair.m) & tair.m < 25
      drink.map[sel] = liv.maps$drink[sel] + liv.maps$drink[sel] * liv.maps$low[sel] * (tair.m[sel] - 25)
      
      liv.dem[,,(z - 1) * 12 + m] = liv.dem[,,(z - 1) * 12 + m] + drink.map + liv.maps$service
    }
  }
}

liv.dem <- array(0, dim = c(length(lons), length(lats), length(years) * 12))
for (z in 1:length(years)) {
  year <- years[z]
  print(paste0("Working on year ", year))

  meteo.file <- grep(meteo.files, pattern = year, value = T)

  nc <- nc_open(meteo.file)
  tair <- ncvar_get(nc, "Tair")
  nc_close(nc)

  tair <- tair - 273.15
  tair[tair < 15] <- 15
  tair[tair > 35] <- 35

  for (m in 1:12) {
    liv$tot = 0
    for(i in 1:nrow(liv)){
      y = which(liv$lat[i] == nc$dim$lat$vals)
      x = which(liv$lon[i] == nc$dim$lon$vals)
      
      tair.m = tair[x,y,m]
      drink.m = liv$ct.s + liv$bf.s + liv$ho.s + liv$gt.s + liv$sh.s + liv$pg.s + liv$ch.s + liv$dk.s
      if(tair.m >= 25) {
        drink.m = drink.m + liv$ct.mean + liv$ct.mean * liv$ct.max * (tair.m - 25)
        drink.m = drink.m + liv$bf.mean + liv$bf.mean * liv$bf.max * (tair.m - 25)
        drink.m = drink.m + liv$ho.mean + liv$ho.mean * liv$ho.max * (tair.m - 25)
        drink.m = drink.m + liv$sh.mean + liv$sh.mean * liv$sh.max * (tair.m - 25)
        drink.m = drink.m + liv$gt.mean + liv$gt.mean * liv$gt.max * (tair.m - 25)
        drink.m = drink.m + liv$pg.mean + liv$pg.mean * liv$pg.max * (tair.m - 25)
        drink.m = drink.m + liv$ch.mean + liv$ch.mean * liv$ch.max * (tair.m - 25)
        drink.m = drink.m + liv$dk.mean + liv$dk.mean * liv$dk.max * (tair.m - 25)
      } else {
        drink.m = drink.m + liv$ct.mean + liv$ct.mean * liv$ct.min * (tair.m - 25)
        drink.m = drink.m + liv$bf.mean + liv$bf.mean * liv$bf.min * (tair.m - 25)
        drink.m = drink.m + liv$ho.mean + liv$ho.mean * liv$ho.min * (tair.m - 25)
        drink.m = drink.m + liv$sh.mean + liv$sh.mean * liv$sh.min * (tair.m - 25)
        drink.m = drink.m + liv$gt.mean + liv$gt.mean * liv$gt.min * (tair.m - 25)
        drink.m = drink.m + liv$pg.mean + liv$pg.mean * liv$pg.min * (tair.m - 25)
        drink.m = drink.m + liv$ch.mean + liv$ch.mean * liv$ch.min * (tair.m - 25)
        drink.m = drink.m + liv$dk.mean + liv$dk.mean * liv$dk.min * (tair.m - 25)
      }
      
      liv.dem[x,y,(z - 1) * 12 + m]
    }
    image.plot(liv.dem[,,(z - 1) * 12 + m])
  }
}

    drink.map <- drink.map <- array(NA, dim = c(length(lons), length(lats)))
    for (m in 1:12) {
      tair.m <- tair[, , m]

      sel <- !is.na(tair.m) & tair.m >= 25
      drink.map[sel] <- liv.maps$drink[sel] + liv.maps$drink[sel] * liv.maps$high[sel] * (tair.m[sel] - 25)

      sel <- !is.na(tair.m) & tair.m < 25
      drink.map[sel] <- liv.maps$drink[sel] + liv.maps$drink[sel] * liv.maps$low[sel] * (tair.m[sel] - 25)

      liv.dem[, , (z - 1) * 12 + m] <- liv.dem[, , (z - 1) * 12 + m] + drink.map + liv.maps$service
    }
  }
}

liv.dem.mm <- liv.dem
for (z in 1:dim(liv.dem.mm)[3]) {
  liv.dem.mm[, , z] <- liv.dem.mm[, , z] / area * 1e3
}
for (x in 1:dim(mask)[1]) {
  for (y in 1:dim(mask)[2]) {
    if (is.na(mask[x, y]) || mask[x, y] == 0) {
      liv.dem.mm[x, y, ] <- NA
      next
    }

    if (is.na(liv.dem.mm[x, y, 1])) {
      liv.dem.mm[x, y, ] <- 0
    }
  }
}

# Save
saveRDS(liv.dem.mm, liv.out)
