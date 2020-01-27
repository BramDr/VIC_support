library(fields)
library(ncdf4)
rm(list = ls())

# Input
mask.file = "Input/domain_global.nc"
country.file = "Output/country_6min_global.RDS"
cell.out = "Output/country_cell_fractions.RDS"

# Load
nc = nc_open(mask.file)
mask = ncvar_get(nc, "mask")
nc_close(nc)

country = readRDS(country.file)

# Setup
lons.mask = nc$dim$lon$vals
lats.mask = nc$dim$lat$vals

lons.country = seq(from = -180 + 360 / dim(country)[1] / 2, to = 180 - 360 / dim(country)[1] / 2, length.out = dim(country)[1])
lats.country = seq(from = -90 + 180 / dim(country)[2] / 2, to = 90 - 180 / dim(country)[2] / 2, length.out = dim(country)[2])

res = (lons.country[2] - lons.country[1]) / (lons.mask[2] - lons.mask[1])
res = res * res

country.unique = unique(na.omit(c(country)))
country.cell = vector(mode = "list", length = length(country.unique))
names(country.cell) = country.unique
for(i in country.unique){
  country.cell[[as.character(i)]] = data.frame(x = numeric(), y = numeric(), frac = numeric())
}

# Calculate
for(x in 1:dim(country)[1]){
  for(y in 1:dim(country)[2]){
    if(is.na(country[x,y])){
      next
    }
    
    mask.x = which.min(abs(lons.mask - lons.country[x]))
    mask.y = which.min(abs(lats.mask - lats.country[y]))
    
    if(is.na(mask[mask.x, mask.y])){
      next
    }
    
    country.cell.df = country.cell[[as.character(country[x,y])]]
    row = which(country.cell.df$x == mask.x & country.cell.df$y == mask.y)
    
    if(length(row) == 0){
      country.cell.df[nrow(country.cell.df) + 1,] = c(mask.x, mask.y, res)
    } else {
      country.cell.df$frac[row] = country.cell.df$frac[row] + res
    }
    
    country.cell[[as.character(country[x,y])]] = country.cell.df
  }
}

frac.map = array(NA, dim = dim(mask))
id.map = array(NA, dim = dim(mask))
for(c.id in names(country.cell)){
  c.cells = country.cell[[as.character(c.id)]]
  
  if(nrow(c.cells) == 0){
    next
  }
  
  for(i in 1:nrow(c.cells)){
    if(is.na(frac.map[c.cells$x[i], c.cells$y[i]])){
      frac.map[c.cells$x[i], c.cells$y[i]] = 0
      id.map[c.cells$x[i], c.cells$y[i]] = -9999
    }
    if(c.cells$frac[i] > frac.map[c.cells$x[i], c.cells$y[i]]){
      id.map[c.cells$x[i], c.cells$y[i]] = as.numeric(c.id)
    }
    frac.map[c.cells$x[i], c.cells$y[i]] = frac.map[c.cells$x[i], c.cells$y[i]] + c.cells$frac[i]
  }
}
image.plot(frac.map)
image.plot(id.map)

country.cell.ext = country.cell
for(x in 1:dim(frac.map)[1]){
  for(y in 1:dim(frac.map)[2]){
    if(is.na(frac.map[x,y]) || frac.map[x,y] == 1 || frac.map[x,y] == 0){
      next
    }
    
    c.id = id.map[x,y]
    
    c.cells = country.cell.ext[[as.character(c.id)]]
    c.row = which(c.cells$x == x & c.cells$y == y)
    
    c.cells$frac[c.row] = c.cells$frac[c.row] + (1 - frac.map[x,y])
    if(c.cells$frac[c.row] < 0){
      print(c.cells$frac[c.row])
      print(frac.map[x,y])
    }
    country.cell.ext[[as.character(c.id)]] = c.cells
  }
}

# Save
saveRDS(country.cell.ext, cell.out)
