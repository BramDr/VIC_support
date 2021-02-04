rm(list = ls())

clay.file <- "./Saves_observed/clay_observed.RDS"
sand.file <- "./Saves_observed/sand_observed.RDS"
silt.file <- "./Saves_observed/silt_observed.RDS"
bulk.file <- "./Saves_observed/bulk_observed.RDS"
ocar.file <- "./Saves_observed/ocar_observed.RDS"
ksat.file <- "./Saves_observed/ksat_observed.RDS"
poro.file <- "./Saves_observed/poro_observed.RDS"

quartz.out <- "./Out_observed/quartz_observed.RDS"
expt.out <- "./Out_observed/expt_observed.RDS"
ksat.out <- "./Out_observed/ksat_observed.RDS"
bulk.dens.out <- "./Out_observed/bulk_dens_observed.RDS"
Wcr.out <- "./Out_observed/Wcr_observed.RDS"
Wpwp.out <- "./Out_observed/Wpwp_observed.RDS"
bubble.out <- "./Out_observed/bubble_observed.RDS"
soil.dens.out <- "./Out_observed/soil_dens_observed.RDS"
resid.moist.out <- "./Out_observed/resid_moist_observed.RDS"

clay <- readRDS(clay.file)
sand <- readRDS(sand.file)
silt <- readRDS(silt.file)
bulk <- readRDS(bulk.file)
ocar <- readRDS(ocar.file)
ksat <- readRDS(ksat.file)
poro <- readRDS(poro.file)

calc.cosby.multivariate <- function(clay, silt, sand) {
  maps <- list()

  maps[["b"]] <- 3.10 + clay * 0.157 + sand * -0.003
  maps[["ys"]] <- 10^(1.54 + sand * -0.0095 + silt * 0.0063)
  maps[["ks"]] <- 10^(-0.60 + sand * 0.0126 + clay * -0.0064)
  maps[["vs"]] <- 50.5 + sand * -0.142 + clay * -0.037

  return(maps)
}
calc.cosby.univariate <- function(clay, silt, sand) {
  maps <- list()

  maps[["b"]] <- 2.91 + clay * 0.159
  maps[["ys"]] <- 10^(1.88 + sand * -0.0131)
  maps[["ks"]] <- 10^(-0.884 + sand * 0.0153)
  maps[["vs"]] <- 48.9 + sand * -0.126

  return(maps)
}
calc.saxton <- function(clay, silt, sand) {
  maps <- list()

  maps[["a"]] <- exp(-4.396 + clay * -0.0715 + sand^2 * -4.880e-4 + sand^2 * clay * -4.285e-5) * 100.0
  maps[["b"]] <- -3.140 + (clay^2) * -0.00222 + (sand^2) * clay * -3.484e-5
  maps[["vs"]] <- 0.332 + sand * -7.251e-4 + log(clay, base = 10) * 0.1276
  maps[["vs"]][maps[["vs"]] <= 0] <- NA
  maps[["ys"]] <- 100.0 * (-0.108 + maps[["vs"]] * 0.341)
  maps[["ks"]] <- 2.778e-6 * exp(12.012 + sand * -0.0755 + ((-3.8950 + sand * 0.03671 + clay * -0.1103 + (clay^2) * 8.7546e-4) / maps[["vs"]]))

  return(maps)
}

cosby.m <- calc.cosby.multivariate(clay * 100, silt * 100, sand * 100)
cosby.u <- calc.cosby.univariate(clay * 100, silt * 100, sand * 100)
saxton <- calc.saxton(clay * 100, silt * 100, sand * 100)

agg.layers <- function(maps) {
  agg.map <- rep(NA, 3)
  agg.map[1] <- (sum(maps[1] * 5) + sum(maps[2] * 10) + sum(maps[3] * 15)) / 30
  agg.map[2] <- (sum(maps[c(4,7:8)] * 20) + sum(maps[c(5:6,9:10)] * 10)) / 100
  agg.map[3] <- sum(maps[11] * 20) / 20
  agg.map[agg.map == 0] <- NA
  return(agg.map)
}

quartz <- sand
quartz <- agg.layers(quartz)

expt <- 2 * -saxton$b + 3
expt <- agg.layers(expt)

ksat <- saxton$ks * 1000 * 60 * 60 * 24
ksat <- agg.layers(ksat)

resid.moist <- (1000000 / saxton$a)^(1 / saxton$b)
Wpwp <- (1500 / saxton$a)^(1 / saxton$b) / saxton$vs
Wfc <- (33 / saxton$a)^(1 / saxton$b) / saxton$vs
Wcr <- Wpwp + 0.7 * (Wfc - Wpwp)

Wpwp[Wpwp > 1 | Wpwp < 0] <- NA
Wpwp <- agg.layers(Wpwp)

resid.moist[resid.moist > 1 | resid.moist < 0] <- NA
resid.moist <- agg.layers(resid.moist)
resid.moist[!is.na(resid.moist) & !is.na(Wpwp) & resid.moist > Wpwp] <- Wpwp[!is.na(resid.moist) & !is.na(Wpwp) & resid.moist > Wpwp] - 1e-6

Wcr[Wcr > 1 | Wcr < 0] <- NA
Wcr <- agg.layers(Wcr)
Wcr[!is.na(Wcr) & !is.na(Wpwp) & Wcr < Wpwp] <- Wpwp[!is.na(Wcr) & !is.na(Wpwp) & Wcr < Wpwp] + 1e-6

bulk.dens <- agg.layers(bulk * 1e3)

bubble <- saxton$ys / 0.0980665
bubble[bubble < 0] <- 0
bubble <- agg.layers(bubble)

soil.dens <- (bulk * 1e3) / (-saxton$vs + 1)
soil.dens <- agg.layers(soil.dens)

dir.create(dirname(quartz.out))
dir.create(dirname(expt.out))
dir.create(dirname(ksat.out))
dir.create(dirname(Wcr.out))
dir.create(dirname(Wpwp.out))
dir.create(dirname(bulk.dens.out))
dir.create(dirname(bubble.out))
dir.create(dirname(soil.dens.out))
dir.create(dirname(resid.moist.out))

saveRDS(quartz, quartz.out)
saveRDS(expt, expt.out)
saveRDS(ksat, ksat.out)
saveRDS(Wcr, Wcr.out)
saveRDS(Wpwp, Wpwp.out)
saveRDS(bulk.dens, bulk.dens.out)
saveRDS(bubble, bubble.out)
saveRDS(soil.dens, soil.dens.out)
saveRDS(resid.moist, resid.moist.out)
