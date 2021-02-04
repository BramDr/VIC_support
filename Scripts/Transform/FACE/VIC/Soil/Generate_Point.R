library(ncdf4)
rm(list = ls())

# Input
generate.support.file <- "../../../../Scripts/Support/generateFunctions.R"
quartz.file <- "./Out_point/quartz_point.RDS"
expt.file <- "./Out_point/expt_point.RDS"
ksat.file <- "./Out_point/ksat_point.RDS"
bulk.dens.file <- "./Out_point/bulk_dens_point.RDS"
Wcr.file <- "./Out_point/Wcr_point.RDS"
Wpwp.file <- "./Out_point/Wpwp_point.RDS"
bubble.file <- "./Out_point/bubble_point.RDS"
soil.dens.file <- "./Out_point/soil_dens_point.RDS"
resid.moist.file <- "./Out_point/resid_moist_point.RDS"
vic.orig <- "../../../../Data/Primary/VIC/VIC_params_global.nc"
vic.out <- "./soil_params_point.nc"

point <- c(33.0628, -111.9826) # lat-lon

# Load
source(generate.support.file)

quartz <- readRDS(quartz.file)
expt <- readRDS(expt.file)
ksat <- readRDS(ksat.file)
bulk.dens <- readRDS(bulk.dens.file)
Wcr <- readRDS(Wcr.file)
Wpwp <- readRDS(Wpwp.file)
bubble <- readRDS(bubble.file)
soil.dens <- readRDS(soil.dens.file)
resid.moist <- readRDS(resid.moist.file)

nc <- nc_open(vic.orig)
depth <- ncvar_get(nc, "depth")
lats = nc$dim$lat$vals
lons = nc$dim$lon$vals
nc_close(nc)
y = which.min(abs(lats - point[1]))
x = which.min(abs(lons - point[2]))
depth = depth[x,y,]

# Calculate
Nlayer = length(expt)

Nlayer.fill <- Nlayer
quartz.fill <- quartz
expt.fill <- expt
ksat.fill <- ksat
bulk.dens.fill <- bulk.dens
Wcr.fill <- Wcr
Wpwp.fill <- Wpwp
bubble.fill <- bubble
soil.dens.fill <- soil.dens
resid.moist.fill <- resid.moist

porosity <- 1 - bulk.dens.fill / soil.dens.fill
Wpwp.fill[!is.na(Wpwp.fill) & Wpwp.fill < resid.moist.fill / porosity] <- resid.moist.fill[!is.na(Wpwp.fill) & Wpwp.fill < resid.moist.fill / porosity] / porosity - 1e-6
Wcr.fill[!is.na(Wcr.fill) & Wcr.fill < Wpwp.fill] <- Wpwp.fill[!is.na(Wcr.fill) & Wcr.fill < Wpwp.fill] + 1e-6

# Set third layer depth to contain 100 mm
depth[3] <- 100 / ((1 - bulk.dens.fill[3] / soil.dens.fill[3]) * 1000)

max.moist <- (1 - bulk.dens.fill / soil.dens.fill) * 1000 * depth
init.moist.fill <- (Wcr.fill + (1 - Wcr.fill) * 0.5) * max.moist

# Save
dir.create(dirname(vic.out))
system(command = paste0("ncks -h",
                        " -d lon,", x, ",", x, 
                        " -d lat,", y, ",", y, 
                        " -v ", paste0(soil.vars[!soil.vars %in% c("init_moist")], collapse = ","), " ", vic.orig, " -O ", vic.out))
for (att in unused.atts) {
  system(command = paste0("ncatted -h -a ", att, ",global,d,, -O ", vic.out))
}

nc <- nc_open(vic.out, write = T)
Nlayer.var <- ncvar_def(name = "Nlayer", units = "#", dim = list(nc$dim$lon, nc$dim$lat), longname = "Number of active layers", prec = "integer", missval = -1)
nc <- ncvar_add(nc, Nlayer.var)
ncvar_put(nc, "lat", point[1])
ncvar_put(nc, "lon", point[2])
ncvar_put(nc, "Nlayer", Nlayer.fill)
ncvar_put(nc, "quartz", quartz.fill)
ncvar_put(nc, "expt", expt.fill)
ncvar_put(nc, "Ksat", ksat.fill)
ncvar_put(nc, "bulk_density", bulk.dens.fill)
ncvar_put(nc, "Wcr_FRACT", Wcr.fill)
ncvar_put(nc, "Wpwp_FRACT", Wpwp.fill)
ncvar_put(nc, "bubble", bubble.fill)
ncvar_put(nc, "soil_density", soil.dens.fill)
ncvar_put(nc, "resid_moist", resid.moist.fill)
nc_close(nc)
