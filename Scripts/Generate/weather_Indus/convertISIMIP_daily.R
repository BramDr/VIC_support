rm(list = ls())

library(ncdf4)
library(fields)
library(humidity)

weather.dir.in = "./Saves/Disaggregated_daily/"
weather.dir.out = "./Saves/Setup_daily/"
weather.dir.tmp = "./Saves/Converted_daily/"
patterns = c("GFDL-ESM4_historical", "IPSL-CM6A-LR_historical", "MPI-ESM1-2-HR_historical", "MRI-ESM2-0_historical", "UKESM1-0-LL_historical",
             "GFDL-ESM4_ssp126", "IPSL-CM6A-LR_ssp126", "MPI-ESM1-2-HR_ssp126", "MRI-ESM2-0_ssp126", "UKESM1-0-LL_ssp126",
             "GFDL-ESM4_ssp370", "IPSL-CM6A-LR_ssp370", "MPI-ESM1-2-HR_ssp370", "MRI-ESM2-0_ssp370", "UKESM1-0-LL_ssp370",
             "GFDL-ESM4_ssp585", "IPSL-CM6A-LR_ssp585", "MPI-ESM1-2-HR_ssp585", "MRI-ESM2-0_ssp585", "UKESM1-0-LL_ssp585")

pattern = patterns[1]
for(pattern in patterns){
  
  # Load
  in.files = list.files(weather.dir.in, pattern = pattern, full.names = T, recursive = T)
  out.files = list.files(weather.dir.out, full.names = T, recursive = T)
  tmp.files = list.files(weather.dir.tmp, pattern = pattern, full.names = T, recursive = T)
  
  #for(tmp.file in tmp.files){
  #  file.remove(tmp.file)
  #}
  
  # Setup
  years = 1850:2100
  
  # Convert vapour pressure
  svp.cc = function(t, isK = T){
    if(!isK){
      t = t + 273.15
    }
    6.11 * exp((2.5e6 / 461.52) * (1/273.15 - 1/t))
  }
  
  year = years[1]
  for(year in years){
    in.file.hurs = grep(x = in.files, pattern = paste0("/hurs_.*", year), value = T)
    in.file.tas = grep(x = in.files, pattern = paste0("/tas_.*", year), value = T)
    out.file.vp = grep(x = out.files, pattern = paste0("/vp_.*", year), value = T)
    tmp.file.vp = gsub(x = out.file.vp, pattern = weather.dir.out, replacement = weather.dir.tmp)
    tmp.file.vp = gsub(x = tmp.file.vp, pattern = paste0("vp", "_daily"), replacement = paste0("vp", "_daily_", pattern))
    if(file.exists(tmp.file.vp)){
      next
    }
    if(length(in.file.hurs) == 0 || length(in.file.tas) == 0){
      next
    }
  
    nc = nc_open(in.file.hurs)
    hurs = ncvar_get(nc, nc$var[[1]])
    nc_close(nc)
    nc = nc_open(in.file.tas)
    tas = ncvar_get(nc, nc$var[[1]])
    nc_close(nc)
    
    svp = svp.cc(tas, isK = F)
    vp = WVP2(hurs, svp) * 1e-3
    #image.plot(vp[,,5])
    
    print(basename(tmp.file.vp))
    dir.create(dirname(tmp.file.vp), recursive = T)
    file.copy(out.file.vp, tmp.file.vp)
    
    nc = nc_open(tmp.file.vp, write = T)
    ncvar_put(nc, nc$var[[1]], vp)
    nc_close(nc)
  }
  rm(hurs, tas, svp, vp)
}
