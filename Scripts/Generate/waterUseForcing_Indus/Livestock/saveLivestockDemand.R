library(fields)
library(ncdf4)
rm(list = ls())

# Input
liv.tot.file <- "../../../../Data/Transformed/Livestock/livestockCount_30min_global.csv"
liv.file <- "../../../../Data/Transformed/Livestock/livestock_5min_Indus.RDS"
meteo.file <- "../../../../Data/Transformed/ERA5/tas_daily_ERA5_ymonmean_1980_2010.nc"
area.file <- "../../../../Data/Transformed/Routing/area_5min_Indus.RDS"
liv.out <- "Saves/livestockDemand_5min_Indus.RDS"

# Load
liv.tot = read.csv(liv.tot.file, stringsAsFactors = F)
liv <- readRDS(liv.file)
area <- readRDS(area.file)

nc = nc_open(meteo.file)
tas = ncvar_get(nc, "tas")
nc_close(nc)

# Setup
## servicing per animal m3 day-1
ct.s <- (2.333 * 1e9 / sum(liv.tot$ct) / 365)
bf.s <- (2.333 * 1e9 / sum(liv.tot$ct) / 365)
ho.s <- (2.333 * 1e9 / sum(liv.tot$ct) / 365)
pg.s <- (4.163 * 1e9 / sum(liv.tot$pg) / 365)
sh.s <- (4.163 * 1e9 / sum(liv.tot$pg) / 365)
gt.s <- (4.163 * 1e9 / sum(liv.tot$pg) / 365)
ch.s <- (0.046 * 1e9 / (sum(liv.tot$ch) / 100) / 365)
dk.s <- (0.046 * 1e9 / (sum(liv.tot$ch) / 100) / 365)

# Intensity per animal m3 day-1
ct.i <- (11.400 * 1e9 / sum(liv.tot$ct) / 365)
bf.i <- (1.360 * 1e9 / sum(liv.tot$bf) / 365)
ho.i <- (11.400 * 1e9 / sum(liv.tot$ct) / 365)
pg.i <- (0.690 * 1e9 / sum(liv.tot$pg) / 365)
gt.i <- (0.770 * 1e9 / sum(liv.tot$gt) / 365)
sh.i <- (1.110 * 1e9 / sum(liv.tot$sh) / 365)
ch.i <- (0.930 * 1e9 / (sum(liv.tot$ch) / 100) / 365)
dk.i <- (0.930 * 1e9 / (sum(liv.tot$ch) / 100) / 365)

ct.min <- (73.2 - 44.1) / 10 / 73.2
bf.min <- (73.2 - 44.1) / 10 / 73.2
ho.min <- (73.2 - 44.1) / 10 / 73.2
pg.min <- (28.3 - 17.2) / 10 / 28.3
sh.min <- (12.9 - 8.7) / 10 / 12.9
gt.min <- (9.6 - 7.6) / 10 / 9.6
ch.min <- (25.8 - 13.2) / 10 / 25.8
dk.min <- (25.8 - 13.2) / 10 / 25.8

ct.max <- (102.3 - 73.2) / 10 / 73.2
bf.max <- (102.3 - 73.2) / 10 / 73.2
ho.max <- (102.3 - 73.2) / 10 / 73.2
pg.max <- (46.7 - 28.3) / 10 / 28.3
sh.max <- (20.1 - 12.9) / 10 / 12.9
gt.max <- (11.9 - 9.6) / 10 / 9.6
ch.max <- (50.5 - 25.8) / 10 / 25.8
dk.max <- (50.5 - 25.8) / 10 / 25.8

calc.livestock = function(temp, map, serv, int, int.max, int.min){
  temp[temp > 35] = 35
  temp[temp < 15] = 15
  temp = temp - 25
  temp[is.na(temp)] = 0
  
  map.mon = array(0, dim = c(dim(map), 12))
  with = array(0, dim = c(dim(map), 12))
  for(z in 1:12){
    map.mon[,,z] = map
  }
  map.mon[is.na(map.mon)] = 0
  
  sel <- !is.na(temp) & temp >= 0
  with[sel] <- map.mon[sel] * serv + map.mon[sel] * int * (1 + int.max * temp[sel])
  sel <- !is.na(temp) & temp < 0
  with[sel] <- map.mon[sel] * serv + map.mon[sel] * int * (1 + int.min * temp[sel])
  
  for(z in 1:12){
    with[,,z] = with[,,z] / area * 1e3 # m3 day-1 to mm day-1
  }
  return(with)
}

# Calculate
with = list()
for(liv.name in names(liv)){
  with[[liv.name]] = calc.livestock(tas, liv[[liv.name]], ct.s, ct.i, ct.max, ct.min) 
}

image.plot(liv$cattle)
image.plot(with$cattle[,,1])

for(i in 1:length(with)){
  if(i == 1){
    with.tot = with[[i]]
  } else {
    with.tot = with.tot + with[[i]]
  }
}
image.plot(with.tot[,,1])

# Save
dir.create(dirname(liv.out))
saveRDS(with.tot, liv.out)
