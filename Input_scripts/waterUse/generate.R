library(fields)
library(ncdf4)
rm(list = ls())

# Input
in.dir = "Input/"
out.dir = "Output/"
years = 1979:2016

# Load
in.files = list.files(in.dir, patter = ".RDS", full.names = T)

# Setup
res = 0.5
lons = seq(
  from = -180 + res / 2,
  to = 180 - res / 2,
  by = res
)
lats = seq(
  from = -90 + res / 2,
  to = 90 - res / 2,
  by = res
)

lon.dim = ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = lons,
  longname = "longitude of cell centre"
)
lat.dim = ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = lats,
  longname = "latitude of cell centre"
)

# Calculate and save
for(in.file in in.files){
  print(paste0("Working on file ", basename(in.file)))
  
  sector = ""
  type = ""
  if(length(grep(x = in.file, "manufacturing")) > 0){
    sector = "manufacturing"
    sec = "man"
  }
  if(length(grep(x = in.file, "domestic")) > 0){
    sector = "domestic"
    sec = "dom"
  }
  if(length(grep(x = in.file, "energy")) > 0){
    sector = "energy"
    sec = "ene"
  }
  if(length(grep(x = in.file, "livestock")) > 0){
    sector = "livestock"
    sec = "liv"
  }
  if(length(grep(x = in.file, "demand")) > 0){
    type = "demand"
    ty = "demand"
  }
  if(length(grep(x = in.file, "groundwater")) > 0){
    type = "groundwater fraction"
    ty = "ground"
  }
  if(length(grep(x = in.file, "consumption")) > 0){
    type = "consumption fraction"
    ty = "consump"
  }
  
  data = readRDS(in.file)

  for(z in 1:length(years)){
    year = years[z]
    
    out.name = paste0(sec, "_", ty, "_6hourly_", year, ".nc")
    out.sdir = paste0("/",sec, "_", ty, "_6hourly/")
    out.file = paste0(out.dir, out.sdir, out.name)
    
    times = seq(
      from = as.Date(paste0(year,"-01-01")),
      to = as.Date(paste0(year, "-12-31")),
      by = "month",
      origin = "1900-01-01"
    )
    
    time.dim = ncdim_def(
      name = "time",
      units = "days since 1970-01-01",
      vals = as.numeric(times), 
      unlim = T,
      calendar = "standard"
    )
    
    var = ncvar_def(
      name = "demand",
      units = "mm",
      dim = list(lon.dim, lat.dim, time.dim),
      missval = -1,
      longname = paste0(sector, " water ", type),
      prec = "double",
      compression = 9
    )
    
    dir.create(dirname(out.file), showWarnings = F)
    nc = nc_create(out.file, list(var))
    
    if(length(dim(data)) == 3){
    ncvar_put(nc, var$name, data[,,((z - 1) * 12 + 1):(z * 12)] / 4)
    } else {
      for(m in 1:12) {
        ncvar_put(nc, var$name, data / 4, start = c(1,1,m), count = c(-1,-1,1)) 
      }
    }
    nc_close(nc)
  }
}