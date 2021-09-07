rm(list = ls())

factor.file = "../../../Data/Primary/Ainsworth2007/CO2_vegetation_characteristics.mapped.csv"
factor.out = "../../../Data/Transformed/CO2/bco2_30min_global.RDS"

# Load
factor = read.csv(factor.file, stringsAsFactors = F)

# Setup
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)

# Calculate
factor$b = ((1 / factor$factor) - 1) / (factor$elevated.CO2 / factor$ambient.CO2 - 1)
factor$test366 = 1 / (1 + factor$b * (366 / 366 - 1))
factor$test567 = 1 / (1 + factor$b * (567 / 366 - 1))
factor$test732 = 1 / (1 + factor$b * (732 / 366 - 1))

adjusted = array(NA, dim = c(length(lons), length(lats), nrow(factor)))
for(i in 1:nrow(factor)){
  adjusted[,,i] = factor$b[i]
}

# Save
dir.create(dirname(factor.out))
saveRDS(adjusted, factor.out)
