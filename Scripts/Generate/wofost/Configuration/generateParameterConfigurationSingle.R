rm(list = ls())

# Input
crop.dir = "../../../Data/WOFOST/Parameters/Crop/global/SA/"
management.dir = "../../../Data/WOFOST/Parameters/Management/global/"
soil.dir = "../../../Data/WOFOST/Parameters/Soil/global/"
site.dir = "../../../Data/WOFOST/Parameters/Site/global/"
season.dir = "../../../Data/WOFOST/Parameters/Season/global/SA/"
out.dir = "../../../Data/WOFOST/Output/global/SA/"
conf.out = "../../../Data/WOFOST/Configuration/global/SA/param_config_global_single.txt"

# Load
crop.files = list.files(crop.dir, full.names = T, pattern = "single")
management.files = list.files(management.dir, full.names = T, pattern = "limited")
soil.files = list.files(soil.dir, full.names = T, pattern = "EC2")
site.files = list.files(site.dir, full.names = T)
season.files = list.files(season.dir, full.names = T)

# Setup
crop.names = gsub(x = basename(crop.files), pattern = "crop_params_", replacement = "")
crop.names = gsub(x = crop.names, pattern = ".txt", replacement = "")
management.names = gsub(x = basename(management.files), pattern = "management_params_", replacement = "")
management.names = gsub(x = management.names, pattern = ".txt", replacement = "")
soil.names = gsub(x = basename(soil.files), pattern = "soil_params_", replacement = "")
soil.names = gsub(x = soil.names, pattern = ".txt", replacement = "")
site.names = gsub(x = basename(site.files), pattern = "site_params_", replacement = "")
site.names = gsub(x = site.names, pattern = ".txt", replacement = "")

# Calculate & save
header = "
** WOFOST PARAMETER CONFIGURATION FILE for use with WOFOST-C, October 2019
** For use in VIC-WOFOST
"
for (i in 1:length(crop.files)){
  text = c()
  conf.out.tmp = gsub(x = conf.out, pattern = "param_config_", replacement = paste0("param_config_", crop.names[i], "_"))
  
  m = grep(x = season.files, pattern = crop.names[i])
  for (j in 1:length(management.files)) {
    for (k in 1:length(soil.files)) {
      for (l in 1:length(site.files)) {
        comb.out = paste0(getwd(), "/", out.dir, "/", "fluxes_global_", crop.names[i], "_", soil.names[k], "_", management.names[j], "_", site.names[l], ".nc")
        line = paste0(getwd(), "/ ", 
                      crop.files[i], " ", 
                      soil.files[k], " ", 
                      management.files[j], " ", 
                      site.files[l], " ", 
                      season.files[m], " ",
                      season.files[m], " ",
                      " 0 ", comb.out, " NCDF")
        text = c(text, line)
      }
    }
  }
  
  text = c(header, text)
  dir.create(dirname(conf.out.tmp))
  writeLines(text = text, con = conf.out.tmp)
}
