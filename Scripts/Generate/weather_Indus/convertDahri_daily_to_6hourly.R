rm(list = ls())
#library(devtools)
#remotes::install_github("wietsefranssen/metGeneratoR")
#devtools::install_git("https://github.com/wietsefranssen/metGeneratoR")
library(metGeneratoR)
library(ncdf4)
library(fields)
library(Rcpp)
library(ncdf4.helpers)

weather.dir = "../../../Data/Primary/HIAWARE/"
weather.out = "../../../Data/VIC/Forcing/Indus_5min/"
timestep = 6
years = 1981:2010
years = 1981:1990
variable.merge = data.frame(HIAWARE = c("prec", "lwra", "swra", "tavg", "tmin", "tmax"),
                            VIC = c("pr", "lwdown", "swdown", "tas", "tmin", "tmax"),
                            units = c("mm", "W m-2", "W m-2", "degrees_celsius", "degrees_celsius", "degrees_celsius"),
                            stringsAsFactors = F)

# Load
in.files = list.files(weather.dir, full.names = T)

# Setup
nc = nc_open(in.files[1])
out.lons = nc$dim$lon$vals
out.lats = nc$dim$lat$vals
nc_close(nc)

#mgsetLonlatbox(c(out.lons[1], out.lons[length(out.lons)], out.lats[length(out.lats)], out.lats[1]))
mgsetInDt(inDt = 24)
mgsetOutDt(outDt = timestep)
mgsetOutVars(c("tas", "pr", "swdown", "lwdown"))

year = years[1]
for (year in years) {
  mgsetPeriod(startdate = paste0(year, "-01-01"), enddate = paste0(year, "-12-31"))
  
  pr.file = grep(x = in.files, pattern = paste0("prec_", year), value = T)
  tasmin.file = grep(x = in.files, pattern = paste0("tmin_", year), value = T)
  tasmax.file = grep(x = in.files, pattern = paste0("tmax_", year), value = T)
  swdown.file = grep(x = in.files, pattern = paste0("swra_", year), value = T)
  lwdown.file = grep(x = in.files, pattern = paste0("lwra_", year), value = T)
  
  mgsetInVars(varlist = list(
    pr = list(ncname = "pr", filename = pr.file)
    #tasmin = list(ncname = "tasmin", filename = tasmin.file),
    #tasmax = list(ncname = "tasmax", filename = tasmax.file),
    #swdown = list(ncname = "swra", filename = swdown.file),
    #lwdown = list(ncname = "lwra", filename = lwdown.file)
  ))
  
  mgsetOutName(nameString = paste0(weather.out, "/<VAR>_6hourly_HIAWARE/<VAR>_6hourly_HIAWARE_<SYEAR>.nc"), message = T)
  metGenRun()
}
