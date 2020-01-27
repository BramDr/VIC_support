library(fields)
library(raster)
rm(list = ls())

# Input
coef.file = "Input/country_domestic_global.csv"
gdp.file = "Input/country_GDP_interpolated_global.csv"
country.file = "Input/country_6min_global.RDS"
dom.tmp = "Saves/domestic_demand_global_tmp.RDS"
years = 1979:2016

# Load
coef = read.csv(coef.file, stringsAsFactors = F)
gdp = read.csv(gdp.file, stringsAsFactors = F)

country = readRDS(country.file)

# Setup
lats = seq(-89.75, 89.75, by = 0.5)
lons = seq(-179.75, 179.75, by = 0.5)

data = merge(gdp, coef, all.x = T)

data$Int = 1
data$Int[data$Development_number == 3 & data$Year > 1980] = 0.995
data$Int[data$Development_number == 2 & data$Year > 1980] = 0.99
data$Int[data$Development_number == 1 & data$Year > 1980] = 0.98

data$lGdpPc = log(data$GdpPc, 10)

# Calculate
data$lWithPc = log(10^(data$lPar + (data$uPar - data$lPar) / 
                       (1 + exp(-data$fPar * (data$lGdpPc - data$oPar)))) * 
                   data$Int ^ (data$Year - 1980), 10)
data$WithPc = 10^data$lWithPc

dom.withpc = array(NA, dim = c(length(lons), length(lats), length(years)))
for(z in 1:length(years)) {
  print(paste0("Working on year ", years[z]))
  
  dom.withpc.y = array(NA, dim = dim(country))
  for(x in 1:dim(country)[1]){
    for(y in 1:dim(country)[2]){
      if(is.na(country[x,y]) || country[x,y] == 10){
        next
      }
      
      row.start = min(which(data$Country_number == country[x,y]))
      dom.withpc.y[x,y] = data$WithPc[row.start + z - 1]
    }
  }
  #image.plot(dom.withpc.y)
  
  dom.withpc.y.r = raster(dom.withpc.y)
  extent(dom.withpc.y.r) = c(-90, 90, -180, 180)
  #plot(dom.withpc.y.r)
  
  dom.withpc.agg = aggregate(x = dom.withpc.y.r, fact = 5, fun = mean, na.rm = T)
  dom.withpc.agg = as.matrix(dom.withpc.agg)
  #image.plot(dom.withpc.agg)
  
  dom.withpc[,,z] = dom.withpc.agg
}
saveRDS(dom.withpc, dom.tmp)
