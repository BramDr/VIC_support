library(ncdf4)
library(fields)
rm(list = ls())

# Input
watch.dir = "Input/"
region.file = "Input/region_global.RDS"
area.file = "Input/domain_global.nc"
mask.file = "Input/domain_global.nc"
out.time.file = "Saves/WATCH_time.csv"
out.space.file = "Saves/WATCH_space.RDS"

# Load
region = readRDS(region.file)
region.number = unique(na.omit(c(region)))

nc = nc_open(area.file)
area = ncvar_get(nc, nc$var$area)
nc_close(nc)

nc = nc_open(mask.file)
mask = ncvar_get(nc, nc$var$mask)
mask.lon = nc$dim$lon$vals
mask.lat = nc$dim$lat$vals
nc_close(nc)

# Setup
month.days = c(31,28,31,30,31,30,31,31,30,31,30,31)
compare.vars = c("domwith", "elecwith", "livecon", "manwith", "indwith")
compare.vars2 = c("adomww", "aeleww", "aliveww", "amanww", "aindww")
compare.models = c("watergap")

# Calculate
output.time = data.frame(model = factor(levels = compare.models), 
                         region = factor(levels = region.number),
                         year = numeric(),
                         variable = factor(levels = compare.vars2),
                         value = numeric())
output.space = list()

for(variable in compare.vars){
  variable2 = compare.vars2[which(compare.vars == variable)]
  
  for(model in compare.models) {
    file = list.files(path = watch.dir, pattern = paste0(model, "_.*", "_.*", variable), full.names = T)
    
    if(length(file) == 0){
      next
    }
    
    # Open and print
    nc = nc_open(file)
    nc_close(nc)
    print(basename(file))
    print(nc$dim$time$units)
    print(nc$var[[1]]$units)
    
    # Get time
    base = gsub(pattern = "years since ", replacement = "", x = nc$dim$time$units)
    base = as.Date(base)
    years = as.numeric(format.Date(base, "%Y")) + nc$dim$time$vals
    dates = as.Date(paste0(years, "-01", "-01"))
    
    # Get and convert data
    nc = nc_open(file)
    data.nc = ncvar_get(nc, nc$var[[1]])
    nc_close(nc)
    
    data = array(NA, dim = c(720, 360, length(years)))
    for(x in 1:dim(data.nc)[1]){
      for(y in 1:dim(data.nc)[2]){
        x.idx = which(nc$dim$lon$vals[x] == mask.lon)
        y.idx = which(nc$dim$lat$vals[y] == mask.lat)
        
        data[x.idx, y.idx, ] = data.nc[x,y,]
      }
    }
    
    for(z in 1:dim(data)[3]){
      data[,,z] = data[,,z] / 1000 * area * 365 * 24 * 60 * 60
    }
    
    # Report global
    reg = "0"
    data.space = apply(X = data, MARGIN = c(1,2), FUN = sum, na.rm = T) / length(unique(years))
    output.space[[paste0(model,"_",variable2)]] = data.space
    image.plot(data.space)
    
    data.months = apply(X = data, MARGIN = c(3), FUN = sum, na.rm = T)
    data.years = aggregate(data.months, by = list(years), FUN = sum)
    for(i in 1:nrow(data.years)){
      output.time[nrow(output.time) + 1,] = c(model, reg, data.years[i,1], variable2, data.years[i,2])
    }
    
    # Report regions
    for(reg in region.number){
      if(reg == 0){
        next
      }
      
      data.reg = data
      for(z in 1:dim(data)[3]){
        data.reg[,,z] = data[,,z] * (region == reg)
      }
      
      data.reg.months = apply(X = data.reg, MARGIN = c(3), FUN = sum, na.rm = T)
      data.reg.years = aggregate(data.reg.months, by = list(years), FUN = sum)
      for(i in 1:nrow(data.years)){
        output.time[nrow(output.time) + 1,] = c(model, reg, data.reg.years[i,1], variable2, data.reg.years[i,2])
      }
    }
  }
}

# Mask
for(x in 1:dim(mask)[1]){
  for(y in 1:dim(mask)[2]){
    if(is.na(mask[x,y])){
      for(i in 1:length(output.space)){
        output.space[[i]][x,y] = NA
      }
    }
  }
}

output.space[["watergap_aindww"]] = output.space[["watergap_aeleww"]] + output.space[["watergap_amanww"]]
output.time.ele = output.time[output.time$variable == "aeleww",]
output.time.man = output.time[output.time$variable == "amanww",]

output.time.ind = output.time.ele
output.time.ind$value = as.numeric(output.time.ind$value) + as.numeric(output.time.man$value)
output.time.ind$variable = "aindww"
output.time = rbind(output.time, output.time.ind)

# Save
write.csv(x = output.time, file = out.time.file, row.names = F)
saveRDS(object = output.space, file = out.space.file)