library(ncdf4)
library(fields)
rm(list = ls())

# Input
iso.file = "Input/ISO3166_dev.csv"
cell.file = "Input/country_cell_fractions.RDS"
flux.file = "Input/fluxes_global_NDAM_NEFR_PIRR.1979.nc"
area.file = "Input/domain_global.nc"
fit.out = "Saves/country_irrigation_global.csv"

# Load
iso = read.csv(iso.file, stringsAsFactors = F)
cell = readRDS(cell.file)

nc = nc_open(area.file)
area = ncvar_get(nc, "area")
nc_close(nc)

nc = nc_open(flux.file)
demand = ncvar_get(nc, nc$var$OUT_DE_GW_SECT, start = c(1,1,1,1), count = c(-1,-1,1,-1)) +
  ncvar_get(nc, nc$var$OUT_DE_SURF_SECT, start = c(1,1,1,1), count = c(-1,-1,1,-1))
time = as.Date(nc$dim$time$vals, origin = "0000-12-30")
time.years = as.numeric(format.Date(time, "%Y"))
nc_close(nc)

# Setup
for(z in 1:dim(demand)[3]){
  demand[,,z] = demand[,,z] / 1000 * area # mm to m3
}
image.plot(demand[,,2])

# Calculate
sim = data.frame(Country_number = numeric(), Year = numeric(), fWith = numeric())
for(i in names(cell)){
  c.df = cell[[i]]
  
  if(nrow(c.df) == 0){
    next
  }
  
  # if(max(c.df$frac) < 1){
  #   next
  # }
  
  if(exists("sim.cell")){
    rm(sim.cell)
  }
  for(j in 1:nrow(c.df)){
    if(exists("sim.cell")){
      sim.cell = sim.cell + demand[c.df$x[j], c.df$y[j],] * c.df$frac[j]
    } else {
      sim.cell = demand[c.df$x[j], c.df$y[j],] * c.df$frac[j]
    }
  }
  
  sim[(nrow(sim) + 1):(nrow(sim) + length(sim.cell)),] = c(rep(i, length(sim.cell)), unique(time.years), sim.cell)
}
sim$Country_number = as.numeric(sim$Country_number)
sim$Year = as.numeric(sim$Year)
sim$fWith = as.numeric(sim$fWith)

sim = merge(sim, iso, by = "Country_number")
sim = sim[,c("Country_number", "Subregion_number", "Region_number", "Year", "fWith")]

# Save
write.csv(x = sim, file = fit.out, row.names = F)

