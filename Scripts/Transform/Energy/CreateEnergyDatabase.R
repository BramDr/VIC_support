library(openxlsx)
rm(list = ls())

# Input
plant.file <- "../../../Data/Primary/vanVliet2016/Thermoelectric_Plants/ThermoelectricPowerPlants_global.xlsx"
plants.out <- "../../../Data/Transformed/Energy/energyWithdrawal_location.csv"

# Load
plants <- read.xlsx(plant.file)

# Setup
gal.to.m3 <- 0.00378541

# Calculate
plants$IntensityWith <- NA
plants$IntensityCon <- NA
for (i in 1:nrow(plants)) {
  if (plants$Cooling_type[i] == "OT" ||
    plants$Cooling_type[i] == "OTF" ||
    plants$Cooling_type[i] == "MIXED" ||
    plants$Cooling_type[i] == "CMB") {
    # Once through
    if (plants$Fuel_type[i] == "UR") {
      plants$IntensityWith[i] <- (25000 + 60000) / 2 * gal.to.m3
      plants$IntensityCon[i] <- 400 * gal.to.m3
    }
    else if (plants$Fuel_type[i] == "GAS" ||
      plants$Fuel_type[i] == "WSTGAS" ||
      plants$Fuel_type[i] == "BFG" ||
      plants$Fuel_type[i] == "LNG") {
      plants$IntensityWith[i] <- (7500 + 20000) / 2 * gal.to.m3
      plants$IntensityCon[i] <- 100 * gal.to.m3
    }
    else if (plants$Fuel_type[i] == "COAL" ||
      plants$Fuel_type[i] == "COKE") {
      plants$IntensityWith[i] <- 380 * gal.to.m3
      plants$IntensityCon[i] <- 200 * gal.to.m3
    }
    else if (plants$Fuel_type[i] == "GEO") {
      plants$IntensityWith[i] <- 380 * gal.to.m3
      plants$IntensityCon[i] <- 200 * gal.to.m3
    }
    else {
      plants$IntensityWith[i] <- (20000 + 50000) / 2 * gal.to.m3
      plants$IntensityCon[i] <- 300 * gal.to.m3
    }
  }
  if (plants$Cooling_type[i] == "M/NDT" ||
    plants$Cooling_type[i] == "CT" ||
    plants$Cooling_type[i] == "NDT" ||
    plants$Cooling_type[i] == "MDT") {
    # Cooling tower
    if (plants$Fuel_type[i] == "UR") {
      plants$IntensityWith[i] <- (800 + 1100) / 2 * gal.to.m3
      plants$IntensityCon[i] <- 720 * gal.to.m3
    }
    else if (plants$Fuel_type[i] == "GAS" ||
      plants$Fuel_type[i] == "WSTGAS" ||
      plants$Fuel_type[i] == "BFG" ||
      plants$Fuel_type[i] == "LNG") {
      plants$IntensityWith[i] <- 230 * gal.to.m3
      plants$IntensityCon[i] <- 180 * gal.to.m3
    }
    else if (plants$Fuel_type[i] == "COAL" ||
      plants$Fuel_type[i] == "COKE") {
      plants$IntensityWith[i] <- 380 * gal.to.m3
      plants$IntensityCon[i] <- 200 * gal.to.m3
    }
    else {
      plants$IntensityWith[i] <- (500 + 600) / 2 * gal.to.m3
      plants$IntensityCon[i] <- 480 * gal.to.m3
    }
  }
}
plants$Withdrawal <- plants$IntensityWith * plants$`Installed_Cap[MW]` * 24 # m3 / day
plants$Consumption <- plants$IntensityCon * plants$`Installed_Cap[MW]` * 24 # m3 / day

# adjust for actual generation
plants$Withdrawal[plants$Fuel_type == "UR"] <-
  plants$Withdrawal[plants$Fuel_type == "UR"] * 0.72
plants$Withdrawal[plants$Fuel_type == "WOOD" | plants$Fuel_type == "BIOMASS"] <-
  plants$Withdrawal[plants$Fuel_type == "WOOD" | plants$Fuel_type == "BIOMASS"] * 0.56
plants$Withdrawal[plants$Fuel_type != "WOOD" & plants$Fuel_type != "BIOMASS" & plants$Fuel_type != "UR"] <-
  plants$Withdrawal[plants$Fuel_type != "WOOD" & plants$Fuel_type != "BIOMASS" & plants$Fuel_type != "UR"] * 0.46

plants$Consumption[plants$Fuel_type == "UR"] <-
  plants$Consumption[plants$Fuel_type == "UR"] * 0.72
plants$Consumption[plants$Fuel_type == "WOOD" | plants$Fuel_type == "BIOMASS"] <-
  plants$Consumption[plants$Fuel_type == "WOOD" | plants$Fuel_type == "BIOMASS"] * 0.56
plants$Consumption[plants$Fuel_type != "WOOD" & plants$Fuel_type != "BIOMASS" & plants$Fuel_type != "UR"] <-
  plants$Consumption[plants$Fuel_type != "WOOD" & plants$Fuel_type != "BIOMASS" & plants$Fuel_type != "UR"] * 0.46

plants$ConsumptionFraction <- plants$Consumption / plants$Withdrawal

sum(plants$Withdrawal) * 365 * 1e-9
sum(plants$Consumption) / sum(plants$Withdrawal)

# Save
write.csv(plants, plants.out, row.names = F)
