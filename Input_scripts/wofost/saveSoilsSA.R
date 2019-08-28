library(ncdf4)
library(fields)
rm(list = ls())

# Input
mask.file = "Input/domain_global.nc"
soil.file = "Input/VIC_params_global.nc"
flux.file = "Input/fluxes_VICWUR_NAT_tmp.1979.nc"
dir.out = "Output/soil/"

# Load
nc = nc_open(mask.file)
mask = ncvar_get(nc, nc$var$mask)
lats = nc$dim$lat$vals
lons = nc$dim$lon$vals
nc_close(nc)

nc = nc_open(soil.file)
depth = ncvar_get(nc, nc$var$depth)
bulk_dens = ncvar_get(nc, nc$var$bulk_density)
soil_dens = ncvar_get(nc, nc$var$soil_density)
Wwp = ncvar_get(nc, nc$var$Wpwp_FRACT)
Wcr = ncvar_get(nc, nc$var$Wcr_FRACT)
Ksat = ncvar_get(nc, nc$var$Ksat)
expt = ncvar_get(nc, nc$var$expt)
nc_close(nc)

nc = nc_open(flux.file)
eff_sat = ncvar_get(nc, nc$var$OUT_SOIL_EFF_SAT)
nc_close(nc)

# Setup
porosity = bulk_dens / soil_dens
Wfc = Wcr / 0.8

eff_sat = apply(X = eff_sat, MARGIN = c(1,2,3), mean)
Q12 = Ksat * eff_sat ^ expt

image.plot(Q12[,,1] * 0.1, zlim = c(0,0.1))
image.plot(Q12[,,2] * 0.1, zlim = c(0,0.1))

SM0 = (porosity[,,1] * depth[,,1] + porosity[,,2] * depth[,,2]) / (depth[,,1] + depth[,,2]) # mm_water mm_soil-1
SMW = (Wwp[,,1] * depth[,,1] + Wwp[,,2] * depth[,,2]) / (depth[,,1] + depth[,,2]) * SM0 # mm_water mm_soil-1
SMFCF = (Wfc[,,1] * depth[,,1] + Wfc[,,2] * depth[,,2]) / (depth[,,1] + depth[,,2]) * SM0 # mm_water mm_soil-1
CRAIRC = (SMW >= 0) * 0.065
K0 = (Ksat[,,1] * depth[,,1] * 0.1 + Ksat[,,2] * depth[,,2] * 0.1) / (depth[,,1] + depth[,,2]) # cm day-1
SOPE = KSUB = (Q12[,,1] * depth[,,1] * 0.1 + Q12[,,2] * depth[,,2] * 0.1) / (depth[,,1] + depth[,,2]) # cm day-1

image.plot(SM0)
image.plot(SMW)
image.plot(SMFCF)
image.plot(CRAIRC)
image.plot(K0)
image.plot(SOPE, zlim = c(0,0.5))
image.plot(KSUB)

# Calculate
for(x in 1:length(lons)){
  for(y in 1:length(lats)){
    if(is.na(mask[x,y]) || mask[x,y] == 0){
      next
    }
    
    file.out = paste0(dir.out, "soil_", lats[y], "N_", lons[x], "E", ".txt")
    print(basename(file.out))
    
    desc.out = paste0(
"
** SOIL DATA FILE for use with WOFOST Version 5.0, June 1990
** Based on VIC input file (Nijssen et al., 2001)
** Latitude: ", lats[y], " N
** Longitude: ", lons[x], " E
"
    )
    
    swr.out = paste0(
"
** soil water retention
SMW      =   ", format(SMW[x,y], digits = 3, nsmall = 3) , "         !  soil moisture content at wilting point [cm3 water / cm3 soil]
SMFCF    =   ", format(SMFCF[x,y], digits = 3, nsmall = 3) , "         !  soil moisture content at field capacity [cm3 water / cm3 soil]
SM0      =   ", format(SM0[x,y], digits = 3, nsmall = 3) , "         !  soil moisture content at saturation [cm3 water / cm3 soil]
CRAIRC   =   ", format(CRAIRC[x,y], digits = 3, nsmall = 3) , "         ! critical soil air content for aeration [cm3 water / cm3 soil]
"
    )
    
    hc.out = paste0(
"
** hydraulic conductivity
K0       =   ", format(K0[x,y], digits = 3, nsmall = 3) , "         ! hydraulic conductivity of saturated soil [cm / day]
SOPE     =   ", format(SOPE[x,y], digits = 3, nsmall = 3) , "         ! maximum percolation rate root zone[cm / day]
KSUB     =   ", format(KSUB[x,y], digits = 3, nsmall = 3) , "         ! maximum percolation rate subsoil [cm / day]
"
    )
    
    dir.create(dirname(file.out), showWarnings = F, recursive = T)
    if(file.exists(file.out)){
      file.remove(file.out)
    }
    writeLines(text = paste0(desc.out, swr.out, hc.out), con = file.out)
  }
}
