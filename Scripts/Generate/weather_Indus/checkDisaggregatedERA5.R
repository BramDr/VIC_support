rm(list = ls())

library(ncdf4)
library(fields)
library(ncdf4.helpers)
library(abind)

weather.dir.in = "./Saves/Disaggregated_6hourly/pr_6hourly_ERA5"

# Load
in.files = list.files(weather.dir.in, pattern = "ERA5", full.names = T, recursive = T)
in.years = gsub(x = basename(in.files), pattern = ".RDS", replacement = "")
in.years = gsub(x = in.years, pattern = ".*\\.", replacement = "")
in.years = unique(na.omit(as.numeric(in.years)))

out.data = array(NA, dim = c(204, 180, length(in.years)))

in.year = in.years[1]
for(in.year in in.years){
  print(in.year)
  
  in.file = grep(x = in.files, pattern = paste0(in.year, ".", in.year), value = T)
  in.data = readRDS(in.file)
  in.data.mean = apply(X = in.data, MARGIN = c(1,2), FUN = mean)
  #image.plot(in.data.mean)
  
  out.data[,,which(in.years == in.year)] = in.data.mean
}

out.data.time = apply(X = out.data, MARGIN = 3, FUN = sum)
plot(in.years, out.data.time, type = "l")
