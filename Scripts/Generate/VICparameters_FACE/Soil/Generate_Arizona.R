library(ncdf4)
rm(list = ls())

# Input
generate.support.file <- "../../../Support/generateFunctions.R"
soil.file = "./Saves/soil_Arizona.RDS"
vic.orig <- "../../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out <- "../../../../Data/VIC/Parameters/FACE/Arizona/soil_params_Arizona.nc"

point <- c(33.0628, -111.9826) # lat-lon

# Load
source(generate.support.file)

soil <- readRDS(soil.file)

# Calculate
Nlayer = length(soil$expt)
soil$soil.dens = soil$bulk.dens / (1 - soil$max.moist)
soil$depth = c(0.3, 1, 100 / soil$max.moist[3] * 1e-3)

# Save
dir.create(dirname(vic.out))
system(command = paste0("ncks -h",
                        " -d lon,", 1, ",", 1, 
                        " -d lat,", 1, ",", 1, 
                        " -v ", paste0(soil.vars[!soil.vars %in% c("init_moist")], collapse = ","), " ", vic.orig, " -O ", vic.out))
for (att in unused.atts) {
  system(command = paste0("ncatted -h -a ", att, ",global,d,, -O ", vic.out))
}

nc <- nc_open(vic.out, write = T)

Nlayer.var <- ncvar_def(name = "Nlayer", units = "#", dim = list(nc$dim$lon, nc$dim$lat), 
      longname = "Number of active layers", prec = "integer", missval = -1)
Wfc.var <- ncvar_def(name = "Wfc_FRACT", units = "fraction", dim = list(nc$dim$lon, nc$dim$lat, nc$dim$nlayer), 
      longname = "Fractional soil moisture content at field capacity (fraction of maximum soil moisture)", prec = "double", missval = -1)
nc <- ncvar_add(nc, Nlayer.var)
nc <- ncvar_add(nc, Wfc.var)

ncvar_put(nc, "lat", point[1])
ncvar_put(nc, "lon", point[2])
ncvar_put(nc, "Nlayer", Nlayer)
ncvar_put(nc, "quartz", soil$quartz)
ncvar_put(nc, "expt", soil$expt)
ncvar_put(nc, "Ksat", soil$ksat)
ncvar_put(nc, "bulk_density", soil$bulk.dens)
ncvar_put(nc, "Wcr_FRACT", soil$Wcr)
ncvar_put(nc, "Wpwp_FRACT", soil$Wpwp)
ncvar_put(nc, "Wfc_FRACT", soil$Wfc)
ncvar_put(nc, "depth", soil$depth)
ncvar_put(nc, "bubble", soil$bubble)
ncvar_put(nc, "soil_density", soil$soil.dens)
ncvar_put(nc, "resid_moist", soil$resid.moist)
nc_close(nc)
