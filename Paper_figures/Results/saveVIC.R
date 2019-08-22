library(ncdf4)
library(fields)
rm(list = ls())

efr.file = "Input/fluxes_global_DAM_EFR_FULL.1979.nc"
nefr.file = "Input/fluxes_global_DAM_NEFR_FULL.1979.nc"
hefr.file = "Input/fluxes_global_DAM_HEFR_FULL.1979.nc"
pirr.file = "Input/fluxes_global_NDAM_NEFR_PIRR.1979.nc"
region.file = "Input/region_global.RDS"
area.file = "Input/domain_global.nc"
out.time.file = "Saves/VIC_time.csv"
out.space.file = "Saves/VIC_space.RDS"

# Load
region = readRDS(region.file)
region.number = unique(na.omit(c(region)))

nc = nc_open(area.file)
area = ncvar_get(nc, nc$var$area)
nc_close(nc)

# Setup
month.days = c(31,28,31,30,31,30,31,31,30,31,30,31)
compare.vars = c("pirrww", "airrww", "adomww", "amanww", "aeleww", "aliveww", "aindww")
compare.sect = c(1,1,2,3,4,5,6)
compare.efr = c("", "_efr", "_hefr")
compare.models = c("vic-wur")
model = "vic-wur"

# Calculate
output.time = data.frame(model = factor(levels = compare.models), 
                         region = factor(levels = region.number),
                         year = numeric(),
                         variable = factor(levels = paste0(rep(compare.vars, each = length(compare.efr)), compare.efr)),
                         value = numeric())
output.space = list()

for(variable in compare.vars){
  sector = compare.sect[which(compare.vars == variable)]
  for(efr in compare.efr) {
    
    if(efr == ""){
      file = nefr.file
    } else if (efr == "_efr") {
      file = efr.file
    } else if (efr == "_hefr"){
      file = hefr.file
    }
    
    if(variable == "aindww"){
      next
    }
    
    if(variable == "pirrww"){
      file = pirr.file
      if(efr != ""){
        next
      }
    }
    
    if(length(file) == 0){
      next
    }
    
    # Open and print
    nc = nc_open(file)
    nc_close(nc)
    print(basename(file))
    print(nc$dim$time$units)
    print(paste0(variable, efr))
    
    # Get time
    base = gsub(pattern = "days since ", replacement = "", x = nc$dim$time$units)
    base = as.Date(base)
    dates = as.Date(nc$dim$time$vals, origin = base) - 2
    years = as.numeric(format.Date(dates, "%Y"))
    
    # Get and convert data
    if(variable != "pirrww"){
      nc = nc_open(file)
      data.gw = ncvar_get(nc, nc$var$OUT_WI_GW_SECT, start = c(1,1,sector,1), count = c(-1,-1,1,-1))
      data.surf = ncvar_get(nc, nc$var$OUT_WI_SURF_SECT, start = c(1,1,sector,1), count = c(-1,-1,1,-1)) +
        ncvar_get(nc, nc$var$OUT_WI_DAM_SECT, start = c(1,1,sector,1), count = c(-1,-1,1,-1)) +
        ncvar_get(nc, nc$var$OUT_WI_REM_SECT, start = c(1,1,sector,1), count = c(-1,-1,1,-1))
      data.tot = data.gw + data.surf
      nc_close(nc)
    } else {
      nc = nc_open(file)
      data.gw = ncvar_get(nc, nc$var$OUT_DE_GW_SECT, start = c(1,1,sector,1), count = c(-1,-1,1,-1))
      data.surf = ncvar_get(nc, nc$var$OUT_DE_SURF_SECT, start = c(1,1,sector,1), count = c(-1,-1,1,-1)) +
        ncvar_get(nc, nc$var$OUT_DE_REM_SECT, start = c(1,1,sector,1), count = c(-1,-1,1,-1))
      data.tot = data.gw + data.surf
      nc_close(nc)
    }
    
    for(z in 1:dim(data.tot)[3]){
      data.gw[,,z] = data.gw[,,z] / 1000 * area
      data.surf[,,z] = data.surf[,,z] / 1000 * area
      data.tot[,,z] = data.tot[,,z] / 1000 * area
    }
    
    # Report global
    reg = "0"
    data.space.tot = apply(X = data.tot, MARGIN = c(1,2), FUN = sum, na.rm = T) / length(unique(years))
    data.space.gw = apply(X = data.gw, MARGIN = c(1,2), FUN = sum, na.rm = T) / length(unique(years))
    data.space.surf = apply(X = data.surf, MARGIN = c(1,2), FUN = sum, na.rm = T) / length(unique(years))
    output.space[[paste0(model,"_",variable, efr, "_tot")]] = data.space.tot
    output.space[[paste0(model,"_",variable, efr, "_gw")]] = data.space.gw
    output.space[[paste0(model,"_",variable, efr, "_surf")]] = data.space.surf
    image.plot(data.space.tot)
    
    data.months = apply(X = data.tot, MARGIN = c(3), FUN = sum, na.rm = T)
    data.years = aggregate(data.months, by = list(years), FUN = sum)
    for(i in 1:nrow(data.years)){
      output.time[nrow(output.time) + 1,] = c(model, reg, data.years[i,1], paste0(variable, efr), data.years[i,2])
    }
    
    # Report regions
    for(reg in region.number){
      if(reg == 0){
        next
      }
      
      data.reg = data.tot
      for(z in 1:dim(data.tot)[3]){
        data.reg[,,z] = data.tot[,,z] * (region == reg)
      }
      
      data.reg.months = apply(X = data.reg, MARGIN = c(3), FUN = sum, na.rm = T)
      data.reg.years = aggregate(data.reg.months, by = list(years), FUN = sum)
      for(i in 1:nrow(data.years)){
        output.time[nrow(output.time) + 1,] = c(model, reg, data.reg.years[i,1], paste0(variable, efr), data.reg.years[i,2])
      }
    }
  }
}

output.space[["vicwur_aindww_tot"]] = output.space[["vicwur_aeleww_tot"]] + output.space[["vicwur_amanww_tot"]]
output.space[["vicwur_aindww_gw"]] = output.space[["vicwur_aeleww_gw"]] + output.space[["vicwur_amanww_gw"]]
output.space[["vicwur_aindww_surf"]] = output.space[["vicwur_aeleww_surf"]] + output.space[["vicwur_amanww_surf"]]
output.time.ele = output.time[output.time$variable == "aeleww",]
output.time.man = output.time[output.time$variable == "amanww",]

output.time.ind = output.time.ele
output.time.ind$value = as.numeric(output.time.ind$value) + as.numeric(output.time.man$value)
output.time.ind$variable = "aindww"
output.time = rbind(output.time, output.time.ind)

# Save
write.csv(x = output.time, file = out.time.file, row.names = F)
saveRDS(object = output.space, file = out.space.file)
