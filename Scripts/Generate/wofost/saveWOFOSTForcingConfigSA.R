library(ncdf4)
library(fields)
rm(list = ls())

# Input
mask.file <- "../../../Data/Primary/VIC/domain_global.nc"
force.dir <- "../../../Data/WOFOST/Forcing/global/"
configuration.force.out <- "../../../Data/WOFOST/Configuration/Forcing/global/"

# Load
nc <- nc_open(mask.file)
mask <- ncvar_get(nc, nc$var$mask)
lats <- nc$dim$lat$vals
lons <- nc$dim$lon$vals
nc_close(nc)

# Setup
force.files <- list.files(force.dir, full.names = T)
years <- 1979:2016

# Calculate
for (x in 1:length(lons)) {
  for (y in 1:length(lats)) {
    if (is.na(mask[x, y]) || mask[x, y] == 0) {
      next
    }
    
    # Forcing
    file.out <- paste0(configuration.force.out, "forcing_config_", lats[y], "N_", lons[x], "E", ".txt")
    print(basename(file.out))

    desc.out <- paste0(
      "
** WOFOST FORCING CONFIGURATION FILE for use with WOFOST Version 5.0, June 1990
** Used for VIC-WOFOST sensitivity analysis
** Latitude: ", lats[y], " N
** Longitude: ", lons[x], " E
"
    )

    force.file <- grep(x = force.files, pattern = paste0(lats[y], "N_", lons[x], "E"), value = T)

    dir.create(dirname(file.out), showWarnings = F, recursive = T)
    if (file.exists(file.out)) {
      file.remove(file.out)
    }
    writeLines(text = desc.out, con = file.out)

    line.out <- paste0(getwd(), "/", force.file, " ", years[1], " ", years[length(years)], " ", lats[y], " ", lons[x])

    write(x = line.out, file = file.out, append = T)
  }
}
