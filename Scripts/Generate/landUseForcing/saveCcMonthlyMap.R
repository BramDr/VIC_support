library(fields)

rm(list = ls())

# Input
cell.area.file = "MIRCA2000/MIRCA2000_cell_area.asc"
crop.calendar.file = "MIRCA2000/MIRCA2000_cropping_calendars_30min.txt"
Cc.monthly.out = "Saves/CcMonthly_30min_global.RDS"
crop.area.file = "MIRCA2000/MIRCA2000_maximum_cropping_area_30min.asc"

# Load
subcrop.calendar = read.csv(file = crop.calendar.file, sep = "\t", stringsAsFactors = F)
cell.area = as.matrix(read.table(file = cell.area.file, skip = 6, stringsAsFactors = F))
crop.area = as.matrix(read.table(file = crop.area.file, skip = 6, stringsAsFactors = F))

# Setup
ncrops = 52
nmonths = 12

add.area = function(x, colnames) {
  x = as.numeric(x)
  print(x[colnames == "rowname"])
  
  start = x[colnames == "start"]
  end = x[colnames == "end"]
  area = x[colnames == "area"]
  
  out = rep(0, 12)
  if(start <= end){
    out[start:end] = area
  } else {
    out[start:12] = area
    out[1:end] = area
  }
  
  return(out)
}

# Calculate
## Data
subcrop.calendar$rowname = 1:nrow(subcrop.calendar)
area = apply(X = subcrop.calendar, MARGIN = 1, FUN = add.area, colnames = colnames(subcrop.calendar))

subcrop.calendar = cbind(subcrop.calendar, t(area))
colnames(subcrop.calendar)[(ncol(subcrop.calendar) - nrow(area) + 1):ncol(subcrop.calendar)] = paste0("area.",1:12)

crop.calendar = aggregate(formula = cbind(area.1,area.2,area.3,area.4,area.5,area.6,area.7,area.8,area.9,area.10,area.11,area.12) ~ 
                            cell_ID + row + column + lat + lon + crop, data = subcrop.calendar, FUN = sum)
print("crop.calendar")
cell.calendar = aggregate(formula = cbind(area.1,area.2,area.3,area.4,area.5,area.6,area.7,area.8,area.9,area.10,area.11,area.12) ~ 
                            cell_ID + row + column + lat + lon, data = subcrop.calendar, FUN = sum)
print("cell.calendar")

## Map
area.monthly = array(0, dim = c(360, 720, ncrops + 1, nmonths))
Cc.monthly = array(0, dim = c(360, 720, ncrops + 1, nmonths))
for(i in 1:nrow(cell.calendar)){
  x = cell.calendar$row[i]
  y = cell.calendar$column[i]
  c.bare.area = crop.area[x,y] - cell.calendar[i,paste0("area.",1:12)]
  
  area.monthly[x,y,dim(area.monthly)[3],] = as.numeric(c.bare.area)
}
for(m in 1:dim(area.monthly)[4]){
  image.plot(area.monthly[,,dim(Cc.monthly)[3],m], main = m)
}

for(i in 1:nrow(crop.calendar)){
  x = crop.calendar$row[i]
  y = crop.calendar$column[i]
  c.crop = crop.calendar$crop[i]
  c.crop.area = crop.calendar[i,paste0("area.",1:12)]
  
  area.monthly[x,y,c.crop,] = as.numeric(c.crop.area)
}
for(m in 1:dim(area.monthly)[4]){
  image.plot(area.monthly[,,50,m], main = m)
}
area.crop.max = apply(X = area.monthly, MARGIN = c(1,2,4), FUN = sum)
area.crop.max = apply(X = area.crop.max, MARGIN = c(1,2), FUN = mean)
image.plot(area.crop.max)
image.plot(crop.area)

for(c in 1:dim(Cc.monthly)[3]){
  for(m in 1:dim(Cc.monthly)[4]){
    Cc.monthly[,,c,m] = area.monthly[,,c,m] / cell.area
  }
}
for(m in 1:dim(Cc.monthly)[4]){
  image.plot(Cc.monthly[,,50,m], main = m)
}
Cc.crop.max = apply(X = Cc.monthly, MARGIN = c(1,2,4), FUN = sum)
Cc.crop.max = apply(X = Cc.crop.max, MARGIN = c(1,2), FUN = mean)
image.plot(Cc.crop.max)

## Transpose
Cc.monthly.t = array(0, dim = c(720, 360, dim(Cc.monthly)[3], dim(Cc.monthly)[4]))
for(c in 1:dim(Cc.monthly)[3]){
  for(m in 1:dim(Cc.monthly)[4]){
    Cc.monthly.t[,,c,m] = t(Cc.monthly[nrow(Cc.monthly):1,,c,m])
  }
}
for(c in 1:dim(Cc.monthly.t)[3]){
  image.plot(Cc.monthly.t[,,c,1], main = c)
}

# Save
dir.create(dirname(Cc.monthly.out))
saveRDS(Cc.monthly.t, Cc.monthly.out)
