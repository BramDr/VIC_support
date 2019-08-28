library(ncdf4)
library(fields)
rm(list = ls())

# Input
mask.file = "Input/domain_global.nc"
crop.dir = "Output/crop/"
force.dir = "Output/force/" 
management.dir = "Output/management/"
site.dir = "Output/site/"
soil.dir = "Output/soil/"
output.dir = "Output/output/"
dir.out = "Output/config/"

# Load
nc = nc_open(mask.file)
mask = ncvar_get(nc, nc$var$mask)
lats = nc$dim$lat$vals
lons = nc$dim$lon$vals
nc_close(nc)

# Setup
crop.files = list.files(crop.dir, full.names = T)
force.files = list.files(force.dir, full.names = T)
management.files = list.files(management.dir, full.names = T)
site.files = list.files(site.dir, full.names = T)
soil.files = list.files(soil.dir, full.names = T)
years = 1979:2016

# Calculate
for(x in 1:length(lons)){
  for(y in 1:length(lats)){
    if(is.na(mask[x,y]) || mask[x,y] == 0){
      next
    }
    
    file.out = paste0(dir.out, "param_config_", lats[y], "N_", lons[x], "E", ".txt")
    print(basename(file.out))
    
    desc.out = paste0(
"
** WOFOST PARAMETER CONFIGURATION FILE for use with WOFOST Version 5.0, June 1990
** Used for VIC-WOFOST sensitivity analysis
** Latitude: ", lats[y], " N
** Longitude: ", lons[x], " E
"
    )
    
    management.file = management.files[1]
    site.file = grep(x = site.files, pattern = paste0(lats[y], "N_", lons[x], "E"), value = T)
    soil.file = grep(x = soil.files, pattern = paste0(lats[y], "N_", lons[x], "E"), value = T)
    out.file = paste0(output.dir, "output_", lats[y], "N_", lons[x], "E", ".txt")
    
    dir.create(dirname(file.out), showWarnings = F, recursive = T)
    if(file.exists(file.out)){
      file.remove(file.out)
    }
    writeLines(text = desc.out, con = file.out)
    
    for(z in 1:length(crop.files)){
      crop.file = crop.files[z]
      line.out = paste0(getwd(), "/ ", crop.file, " ", soil.file, " ", management.file, " ", site.file, " 01-03 0 ", out.file)
      
      write(x = line.out, file = file.out, append = T)
    }
    
    file.out = paste0(dir.out, "forcing_config_", lats[y], "N_", lons[x], "E", ".txt")
    print(basename(file.out))
    
    desc.out = paste0(
      "
** WOFOST FORCING CONFIGURATION FILE for use with WOFOST Version 5.0, June 1990
** Used for VIC-WOFOST sensitivity analysis
** Latitude: ", lats[y], " N
** Longitude: ", lons[x], " E
"
    )
    
    force.file = grep(x = force.files, pattern = paste0(lats[y], "N_", lons[x], "E"), value = T)
    
    dir.create(dirname(file.out), showWarnings = F, recursive = T)
    if(file.exists(file.out)){
      file.remove(file.out)
    }
    writeLines(text = desc.out, con = file.out)
    
	crop.file = crop.files[z]
	line.out = paste0(getwd(), "/", force.file, " ", years[1], " ", years[length(years)], " ", lats[y], " ", lons[x])
	
	write(x = line.out, file = file.out, append = T)
  }
}
