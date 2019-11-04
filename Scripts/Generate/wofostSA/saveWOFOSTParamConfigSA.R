library(ncdf4)
library(fields)
rm(list = ls())

# Input
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
crop.dir <- "../../../Data/WOFOST/Parameters/Crop/global/"
management.dir <- "../../../Data/WOFOST/Parameters/Management/global/"
site.dir <- "../../../Data/WOFOST/Parameters/Site/global/"
soil.dir <- "../../../Data/WOFOST/Parameters/Soil/global/"
output.dir <- "../../../Data/WOFOST/Output/global/"
configuration.param.out <- "../../../Data/WOFOST/Configuration/Parameters/global/"

# Load
nc <- nc_open(mask.file)
mask <- ncvar_get(nc, nc$var$mask)
lats <- nc$dim$lat$vals
lons <- nc$dim$lon$vals
nc_close(nc)

# Setup
crop.files <- list.files(crop.dir, full.names = T)
management.files <- list.files(management.dir, full.names = T)
site.files <- list.files(site.dir, full.names = T)
soil.files <- list.files(soil.dir, full.names = T)
soil.files <- grep(x = soil.files, pattern = "EC3", value = T)

# Calculate & Save
for (x in 1:length(lons)) {
  for (y in 1:length(lats)) {
    if (is.na(mask[x, y]) || mask[x, y] == 0) {
      next
    }

    file.out <- paste0(configuration.param.out, "param_config_SA_", lats[y], "N_", lons[x], "E", ".txt")
    print(basename(file.out))

    desc.out <- paste0(
      "
** WOFOST PARAMETER CONFIGURATION FILE for use with WOFOST Version 5.0, June 1990
** Used for VIC-WOFOST sensitivity analysis
** Latitude: ", lats[y], " N
** Longitude: ", lons[x], " E
      "
    )

    management.file <- management.files[1]
    site.file <- site.files[1]
    soil.file <- soil.files[1]
	out.files <- paste0(getwd(), "/", output.dir, "output_", lats[y], "N_", lons[x], "E", ".txt")

    dir.create(dirname(file.out), showWarnings = F, recursive = T)
    if (file.exists(file.out)) {
      file.remove(file.out)
    }
    writeLines(text = desc.out, con = file.out)

    for (z in 1:length(crop.files)) {
      crop.file <- crop.files[z]
      
      crop.name <- gsub(x = basename(crop.file), pattern = "crop_params_", replacement = "")
      crop.name <- gsub(x = basename(crop.name), pattern = ".txt", replacement = "")
      out.file <- gsub(x = out.files, pattern = "output_", replacement = paste0("output_", crop.name, "_"))
      
      line.out <- paste0(getwd(), "/ ", crop.file, " ", soil.file, " ", management.file, " ", site.file, " 01-03 0 ", out.file)
      write(x = line.out, file = file.out, append = T)
    }
  }
}
