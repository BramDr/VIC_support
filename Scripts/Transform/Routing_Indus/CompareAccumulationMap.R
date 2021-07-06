library(ncdf4)
library(fields)
rm(list = ls())

# Input
accumulation.hydro.file <- "../../../Data/Transformed/Routing/accumulation_Hydro1k_5min_Indus.RDS"
accumulation.lpjml.file <- "../../../Data/Transformed/Routing/accumulation_LPJmL_5min_Indus.RDS"
accumulation.dahri.file <- "../../../Data/Transformed/Routing/accumulation_Dahri_5min_Indus.RDS"
accumulation.file <- "../../../Data/Transformed/Routing/accumulation_5min_Indus.RDS"

# Load
accumulation.dahri = readRDS(accumulation.dahri.file)
accumulation.hydro = readRDS(accumulation.hydro.file)
accumulation.lpjml = readRDS(accumulation.lpjml.file)
accumulation = readRDS(accumulation.file)

image.plot(accumulation.hydro, zlim = c(0, 1e11))
image.plot(accumulation.lpjml, zlim = c(0, 1e11))
image.plot(accumulation.dahri, zlim = c(0, 1e11))
image.plot(accumulation, zlim = c(0, 1e11))


image.plot(accumulation.lpjml, zlim = c(0, 1e11))
image.plot(accumulation, zlim = c(0, 1e11))
