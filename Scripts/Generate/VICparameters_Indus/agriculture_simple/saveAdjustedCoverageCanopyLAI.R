rm(list = ls())
library(fields)
library(ncdf4)

# Input
vegetation.file <- "../../../../Data/VIC/Parameters/Indus_5min/vegetation_params_Modis_Indus.nc"
fraction.file = "./Saves/canopy_fraction_crops_monthly_5min_Indus.RDS"
coverage.file = "./Saves/coverage_monthly_5min_Indus.RDS"
vegetation.crop.idx = 12
canopy.out = "./Saves/canopy_adjusted_monthly_5min_Indus.RDS"
lai.out = "./Saves/lai_adjusted_monthly_5min_Indus.RDS"
coverage.out = "./Saves/coverage_adjusted_monthly_5min_Indus.RDS"
nbare = 2

# Load
nc = nc_open(vegetation.file)
vegetation.canopy = ncvar_get(nc, "fcanopy")
vegetation.lai = ncvar_get(nc, "LAI")
nc_close(nc)

fraction = readRDS(fraction.file)
coverage = readRDS(coverage.file)

# Calculate
canopy.sel = vegetation.canopy[,,,vegetation.crop.idx]
lai.sel = vegetation.lai[,,,vegetation.crop.idx]

image.plot(lai.sel[,,1])
image.plot(lai.sel[,,6])
image.plot(canopy.sel[,,1])
image.plot(canopy.sel[,,6])

canopy.sel = canopy.sel / fraction
lai.sel = lai.sel / fraction

canopy.sel[!is.na(canopy.sel) & fraction == 0] = NA
lai.sel[!is.na(lai.sel) & fraction == 0] = NA
canopy.sel[!is.na(canopy.sel) & canopy.sel > 1] = 1
lai.sel[!is.na(lai.sel) & lai.sel > 7] = 7

image.plot(lai.sel[,,1])
image.plot(lai.sel[,,6])
image.plot(canopy.sel[,,1])
image.plot(canopy.sel[,,6])

canopy.adj = array(NA, dim = dim(coverage)[c(1:2, 4, 3)])
canopy.adj[,,,1:(dim(vegetation.canopy)[4] - 1)] = vegetation.canopy[,,,1:(dim(vegetation.canopy)[4] - 1)]
canopy.adj[,,,(dim(coverage)[3] - nbare + 1):dim(coverage)[3]] = vegetation.canopy[,,,dim(vegetation.canopy)[4]]
canopy.adj[,,,dim(vegetation.canopy)[4]:(dim(coverage)[3] - nbare)] = canopy.sel

lai.adj = array(NA, dim = dim(coverage)[c(1:2, 4, 3)])
lai.adj[,,,1:(dim(vegetation.lai)[4] - 1)] = vegetation.lai[,,,1:(dim(vegetation.lai)[4] - 1)]
lai.adj[,,,(dim(coverage)[3] - nbare + 1):dim(coverage)[3]] = vegetation.lai[,,,dim(vegetation.lai)[4]]
lai.adj[,,,dim(vegetation.lai)[4]:(dim(coverage)[3] - nbare)] = lai.sel

coverage.adj = coverage
coverage.adj[,,dim(coverage)[3],] = coverage.adj[,,dim(coverage)[3],] + coverage[,,vegetation.crop.idx,]
coverage.adj[,,vegetation.crop.idx,] = coverage.adj[,,vegetation.crop.idx,] * 0

# Save
dir.create(dirname(canopy.out))
saveRDS(canopy.adj, canopy.out)
dir.create(dirname(lai.out))
saveRDS(lai.adj, lai.out)
dir.create(dirname(coverage.out))
saveRDS(coverage.adj, coverage.out)

