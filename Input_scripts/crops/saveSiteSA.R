library(ncdf4)
library(fields)
rm(list = ls())

# Input
mask.file = "Input/domain_global.nc"
soil.file = "Input/VIC_params_global.nc"
dir.out = "Output/site/"

# Load
nc = nc_open(mask.file)
mask = ncvar_get(nc, nc$var$mask)
lats = nc$dim$lat$vals
lons = nc$dim$lon$vals
nc_close(nc)

nc = nc_open(soil.file)
depth = ncvar_get(nc, nc$var$depth)
init_moist = ncvar_get(nc, nc$var$init_moist)
bulk_dens = ncvar_get(nc, nc$var$bulk_density)
soil_dens = ncvar_get(nc, nc$var$soil_density)
nc_close(nc)

# Setup
porosity = bulk_dens / soil_dens

RDMSOL = (depth[,,1] + depth[,,2]) * 100
IFUNRN = RDMSOL * 0
SSMAX = RDMSOL * 0
WAV = init_moist[,,1] + init_moist[,,2]
ZTI = (RDMSOL >= 0) * 999
NOTINF = RDMSOL * 0
SSI = RDMSOL * 0
SMLIM = (porosity[,,1] * depth[,,1] + porosity[,,2] * depth[,,2]) / (depth[,,1] + depth[,,2]) # mm_water mm_soil-1
CO2 = (RDMSOL >= 0) * 360

image.plot(RDMSOL)
image.plot(IFUNRN)
image.plot(SSMAX)
image.plot(WAV)
image.plot(ZTI)
image.plot(NOTINF)
image.plot(SSI)
image.plot(SMLIM)
image.plot(CO2)

# Calculate
for(x in 1:length(lons)){
  for(y in 1:length(lats)){
    if(is.na(mask[x,y]) || mask[x,y] == 0){
      next
    }
    
    file.out = paste0(dir.out, "site_", lats[y], "N_", lons[x], "E", ".site")
    print(basename(file.out))
    
    desc.out = paste0(
"
** SITE DATA FILE for use with WOFOST Version 5.0, June 1990
** Based on VIC input file (Nijssen et al., 2001)
** Latitude: ", lats[y], " N
** Longitude: ", lons[x], " E
"
    )
    
    swi.out = paste0(
"
** site water inputs
IFUNRN = ", format(IFUNRN[x,y], digits = 3, nsmall = 3) , "			!  Is infiltration rainfall dependent? [flag; 1 = yes, 0 = no]
SSMAX  = ", format(SSMAX[x,y], digits = 3, nsmall = 3) , "			!  Maximum surface water storage [mm]
WAV    = ", format(WAV[x,y], digits = 3, nsmall = 3) , "		!  Initial soil water storage [mm]
ZTI    = ", format(ZTI[x,y], digits = 3, nsmall = 3) , "	!  Water table depth [cm]
RDMSOL = ", format(RDMSOL[x,y], digits = 3, nsmall = 3) , "		!  Maximum rooting depth [cm]
NOTINF = ", format(NOTINF[x,y], digits = 3, nsmall = 3) , "			!  Fraction of water not infiltrating [mm / mm]
SSI    = ", format(SSI[x,y], digits = 3, nsmall = 3) , "			!  Initial surface water storage [mm]
SMLIM  = ", format(SMLIM[x,y], digits = 3, nsmall = 3) , "			!  Maximum initial soil water storage [mm water / mm soil]
CO2    = ", format(CO2[x,y], nsmall = 0) , "			!  CO2 concentration [ppm]
"
    )
    
    dir.create(dirname(file.out), showWarnings = F, recursive = T)
    if(file.exists(file.out)){
      file.remove(file.out)
    }
    writeLines(text = paste0(desc.out, swi.out), con = file.out)
  }
}
