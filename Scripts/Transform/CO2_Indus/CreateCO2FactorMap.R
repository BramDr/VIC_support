rm(list = ls())

factor.file = "../../../Data/Primary/Ainsworth2007/CO2_vegetation_characteristics.csv"
factor.out = "../../../Data/Transformed/CO2/bco2_5min_Indus.RDS"

resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]

# Load
factor = read.csv(factor.file, stringsAsFactors = F)

# Setup
# 1 = Evergreen needleleaf forests 2 = Evergreen broadleaf forests 3 = Deciduous needleleaf forests 
# 4 = Deciduous broadleaf forests 5 = Mixed forests 6 = Closed shrublands 7 = Open shrublands
# 8 = Woody savannas 9 = Savannas 10 = Grasslands 11 = Permanent wetlands 12 = Croplands
# 13 = Urband and build-up lands 14 = Barren
mapping = data.frame(vic = 1:14,
            ainsworth = c(rep(10, 5), rep(9, 2), rep(4, 2), 6, 9, 5, 9, NA))

# Calculate
factor$b = ((1 / factor$factor) - 1) / (factor$elevated.CO2 / factor$ambient.CO2 - 1)

adjusted = array(0, dim = c(length(out.lons), length(out.lats), nrow(mapping)))
for(i in 1:nrow(mapping)){
  if(is.na(mapping$ainsworth[i])){
    next
  }
  adjusted[,,i] = factor$b[mapping$ainsworth[i]]
}
plot(adjusted[1,1,], type = "l")

# Save
dir.create(dirname(factor.out))
saveRDS(adjusted, factor.out)
