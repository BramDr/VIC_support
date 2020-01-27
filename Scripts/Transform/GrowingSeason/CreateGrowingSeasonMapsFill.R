library(ncdf4)
library(fields)
rm(list = ls())

# Input
sage.dir = "../../../Data/Primary/SAGE"
grow.season.out = "../../../Data/Transformed/GrowingSeason/growingSeasonFill_30min_global.RDS"

# Load
sage.files = list.files(path = sage.dir, pattern = ".fill.nc", full.names = T)

# Calculate & save
for(sage.file in sage.files){
  grow.season = list()
  
  crop.name = gsub(x = basename(sage.file), pattern = ".crop.calendar.*", replacement = "")
  crop.name = tolower(crop.name)
  grow.season.out.tmp = gsub(x = grow.season.out, pattern = "_30min", replacement = paste0("_", crop.name, "_30min"))
  print(basename(grow.season.out.tmp))
  
  nc = nc_open(sage.file)
  plant = ncvar_get(nc, nc$var$plant)
  plant.start = ncvar_get(nc, nc$var$plant.start)
  plant.end = ncvar_get(nc, nc$var$plant.end)
  harvest = ncvar_get(nc, nc$var$harvest)
  harvest.start = ncvar_get(nc, nc$var$harvest.start)
  harvest.end = ncvar_get(nc, nc$var$harvest.end)
  nc_close(nc)
  
  plant = plant[,ncol(plant):1]
  plant.start = plant.start[,ncol(plant.start):1]
  plant.end = plant.end[,ncol(plant.end):1]
  harvest = harvest[,ncol(harvest):1]
  harvest.start = harvest.start[,ncol(harvest.start):1]
  harvest.end = harvest.end[,ncol(harvest.end):1]
  
  grow.season$plant = plant
  grow.season$plant.start = plant.start
  grow.season$plant.end = plant.end
  grow.season$harvest = harvest
  grow.season$harvest.start = harvest.start
  grow.season$harvest.end = harvest.end
  #image.plot(grow.season$plant)
  #image.plot(grow.season$harvest)
  
  dir.create(dirname(grow.season.out.tmp))
  saveRDS(object = grow.season, file = grow.season.out.tmp)
}
