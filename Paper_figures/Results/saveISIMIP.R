library(ncdf4)
library(fields)
rm(list = ls())

# Input
isimip.dir = "Input/"
region.file = "Input/region_global.RDS"
area.file = "Input/domain_global.nc"
mask.file = "Input/domain_global.nc"
out.time.file = "Saves/ISIMIP_time.csv"
out.space.file = "Saves/ISIMIP_space.RDS"

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
compare.vars = c("pirrww", "airrww", "adomww", "aindww", "aliveww", "amanww")
compare.scen = c("pressoc", "pressoc", "varsoc", "varsoc", "varsoc", "varsoc")
compare.models = c("pcr-globwb", "h08", "vic", "lpjml")

# Calculate
output.time = data.frame(model = factor(levels = compare.models), 
                         region = factor(levels = region.number),
                         year = numeric(),
                         variable = factor(levels = compare.vars),
                         value = numeric())
output.space = list()

for(variable in compare.vars){
  scenario = compare.scen[which(compare.vars == variable)]
  for(model in compare.models) {
    file = list.files(path = isimip.dir, pattern = paste0(model, "_.*", scenario, "_.*", variable), full.names = T)
    
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
    base = gsub(pattern = "months since ", replacement = "", x = nc$dim$time$units)
    base = as.Date(base)
    years = as.numeric(format.Date(base, "%Y")) + floor(nc$dim$time$vals / 12)
    months = nc$dim$time$vals %% 12 + 1
    dates = as.Date(paste0(years, "-", ceiling(months), "-01"))
    
    # Get and convert data
    nc = nc_open(file)
    data = ncvar_get(nc, variable)
    nc_close(nc)
    
    for(z in 1:dim(data)[3]){
      data[,,z] = data[,dim(data)[2]:1,z] / 1000 * area * month.days[months[z]] * 24 * 60 * 60
    }
    
    # Report global
    reg = "0"
    data.space = apply(X = data, MARGIN = c(1,2), FUN = sum, na.rm = T) / length(unique(years))
    output.space[[paste0(model,"_",variable)]] = data.space
    image.plot(data.space)
    
    data.months = apply(X = data, MARGIN = c(3), FUN = sum, na.rm = T)
    data.years = aggregate(data.months, by = list(years), FUN = sum)
    for(i in 1:nrow(data.years)){
      output.time[nrow(output.time) + 1,] = c(model, reg, data.years[i,1], variable, data.years[i,2])
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
        output.time[nrow(output.time) + 1,] = c(model, reg, data.reg.years[i,1], variable, data.reg.years[i,2])
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

# Save
write.csv(x = output.time, file = out.time.file, row.names = F)
saveRDS(object = output.space, file = out.space.file)
