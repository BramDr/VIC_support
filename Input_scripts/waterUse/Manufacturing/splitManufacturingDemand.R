library(fields)
rm(list = ls())

# Input
cells.file = "Input/country_cell_fractions.RDS"
iso.file = "Input/ISO3166_dev.csv"
ind.file = "Input/industrial_demand_global.RDS"
ene.file = "Input/energy_demand_global.RDS"
man.out = "Saves/manufacturing_demand_global.RDS"
ene.out = "Saves/energy_demand_global.RDS"
years = 1979:2016

# Load
iso = read.csv(iso.file, stringsAsFactors = F)

cells = readRDS(cells.file)
ene = readRDS(ene.file)
ind = readRDS(ind.file)

# Setup
man = ind - ene
sum(ind < 0, na.rm = T)
sum(ene < 0, na.rm = T)
sum(man < 0, na.rm = T)

# Calculate
ids.c = as.numeric(names(cells))
country.ind = data.frame(Country_number = rep(ids.c, each = length(years) * 12), 
                         Year = rep(years, length.out = length(ids.c) * 12, each =  12),
                         month = rep(1:12, length.out = length(years) * 12),
                         Manufacturing_withdrawal = 0,
                         Energy_withdrawal = 0)
country.ind = merge(country.ind, iso, by = "Country_number")

for(i in 1:length(cells)){
  cells.c = cells[[i]]
  id.c = as.numeric(names(cells)[i])
  
  if(nrow(cells.c) == 0){
    next
  }
  
  for(j in 1:nrow(cells.c)){
    x = cells.c$x[j]
    y = cells.c$y[j]
    frac = cells.c$frac[j]
    
    rows.c = which(country.ind$Country_number == id.c)
    mans.c = man[x,y,] * frac
    ene.c = mans.c
    
    mans.c[mans.c < 0] = 0
    ene.c[ene.c > 0] = 0
    ene.c = abs(ene.c)
    
    country.ind$Manufacturing_withdrawal[rows.c] = country.ind$Manufacturing_withdrawal[rows.c] + mans.c
    country.ind$Energy_withdrawal[rows.c] = country.ind$Energy_withdrawal[rows.c] + ene.c
  }
}

country.map = array(NA, dim = dim(man)[1:2])
frac.map = array(0, dim = dim(man)[1:2])
for(i in 1:length(cells)){
  cells.c = cells[[i]]
  id.c = as.numeric(names(cells)[i])
  
  if(nrow(cells.c) == 0){
    next
  }
  
  for(j in 1:nrow(cells.c)){
    x = cells.c$x[j]
    y = cells.c$y[j]
    frac = cells.c$frac[j]
    
    if(frac > frac.map[x,y]){
      country.map[x,y] = id.c
      frac.map[x,y] = frac
    }
  }
}
image.plot(country.map)
image.plot(frac.map)

# Lower industrial cells if manufacturing < 0
country.ind$Man_reduce_fraction = 1
country.ind$Ene_reduce_fraction = 1
for(x in 1:dim(man)[1]){
  for(y in 1:dim(man)[2]){
    if(is.na(man[x,y,1])){
      next
    }
    
    id.c = country.map[x,y]
    frac.c = frac.map[x,y]
    
    for(z in 1:dim(man)[3]){
      if(man[x,y,z] >= 0){
        next
      }
      
      year = years[z]
      row = which(country.ind$Country_number == id.c & country.ind$Year == year)
      
      dem = abs(man[x,y,z])
      avail = country.ind$Manufacturing_withdrawal[row]
      ind.frac = dem / avail
      
      country.ind$Man_reduce_fraction[row] = country.ind$Man_reduce_fraction[row] - ind.frac
    }
  }
}

sel = country.ind$Man_reduce_fraction < 0.1
country.ind$Ene_reduce_fraction[sel] = (0.1 - country.ind$Man_reduce_fraction[sel]) * 
  country.ind$Manufacturing_withdrawal[sel]
country.ind$Ene_reduce_fraction[sel] = 1 - 
  (country.ind$Ene_reduce_fraction[sel] / country.ind$Energy_withdrawal[sel])
country.ind$Man_reduce_fraction[sel] = 0.1

# Lower
man.adj = man
man.adj2 = man
man.adj[man.adj < 0] = 0
man.adj2[man.adj2 < 0] = 0

ene.adj = ene
ene.adj[man < 0] = abs(man[man < 0])
ene.adj2 = ene.adj

for(i in 1:length(cells)){
  cells.c = cells[[i]]
  id.c = as.numeric(names(cells)[i])
  
  if(nrow(cells.c) == 0){
    next
  }
  
  for(j in 1:nrow(cells.c)){
    x = cells.c$x[j]
    y = cells.c$y[j]
    frac = cells.c$frac[j]
    
    rows.c = which(country.ind$Country_number == id.c)
    
    fracs.man.c = country.ind$Man_reduce_fraction[rows.c]
    fracs.ene.c = country.ind$Ene_reduce_fraction[rows.c]
    mans.c = man.adj2[x,y,]
    enes.c = ene.adj2[x,y,]
    
    mans.c = mans.c - frac * mans.c * (1 - fracs.man.c)
    enes.c = enes.c - frac * enes.c * (1 - fracs.ene.c)
    man.adj[x,y,] = mans.c
    ene.adj[x,y,] = enes.c
  }
}

# Save
saveRDS(man.adj, man.out)
saveRDS(ene.adj, ene.out)
