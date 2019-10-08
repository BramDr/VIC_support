library(ncdf4)
library(fields)
rm(list = ls())

# Input
crop.dir <- "../../../Data/WOFOST/Parameters/Crop/global/"
management.dir <- "../../../Data/WOFOST/Parameters/Management/global/"
site.dir <- "../../../Data/WOFOST/Parameters/Site/global/"
configuration.param.out <- "../../../Data/VIC/Parameters/global/SA/WOFOST_params_config_global.txt"

# Setup
crop.files <- list.files(crop.dir, full.names = T)
management.files <- list.files(management.dir, full.names = T)
site.files <- list.files(site.dir, full.names = T)

# Calculate & Save
desc.out <- paste0(
  "
** WOFOST PARAMETER CONFIGURATION FILE for use with VIC-WOFOST Version 0.1, October 2019
** Used for VIC-WOFOST sensitivity analysis
  "
)

management.file <- management.files[1]
site.file <- site.files[1]

for (z in 1:length(crop.files)) {
  crop.file <- crop.files[z]
  
  line.out <- paste0(getwd(), "/ ", crop.file, " ", management.file, " 01-03 0")
  
  crop.name <- gsub(x = basename(crop.file), pattern = "crop_params_", replacement = "")
  crop.name <- gsub(x = crop.name, pattern = ".txt", replacement = "")
  
  file.out <- configuration.param.out
  file.out <- gsub(x = file.out, pattern = "_global", replacement = paste0("_", crop.name, "_global"))
  print(basename(file.out))
  
  dir.create(dirname(file.out), showWarnings = F, recursive = T)
  if (file.exists(file.out)) {
    file.remove(file.out)
  }
  writeLines(text = desc.out, con = file.out)
  write(x = line.out, file = file.out, append = T)
}
