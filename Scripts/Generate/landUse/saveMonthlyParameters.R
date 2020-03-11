library(fields)
library(plyr)
rm(list = ls())

# Input
fao.file <- "../../../Data/Primary/FAO/Allen1998/FAO_crop_characteristics.csv"
cc.file <- "../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
kc.file <- "Saves/croppingCalendars_development.csv"
albedo.out <- "Saves/croppingCalendars_albedo.csv"
height.out <- "Saves/croppingCalendars_height.csv"
LAI.out <- "Saves/croppingCalendars_LAI.csv"
veg_rough.out <- "Saves/croppingCalendars_roughness.csv"
displacement.out <- "Saves/croppingCalendars_displacement.csv"

# Load
cc <- read.csv(file = cc.file, header = TRUE, stringsAsFactors = F)
fao <- read.csv(fao.file, stringsAsFactors = F)
kc <- read.csv(kc.file, stringsAsFactors = F)

# Setup
add.height <- function(x, columns) {
  x <- as.numeric(x)
  # print(x[columns == "rowname"])

  row <- which((x[columns == "crop"] - 1) %% 26 + 1 == fao$number)
  maxValue <- as.numeric(fao[row, c("height")])
  out <- x[columns %in% paste0("Kc.", 1:12)] * maxValue

  return(out)
}
add.albedo <- function(x, columns) {
  x <- as.numeric(x)
  # print(x[columns == "rowname"])

  row <- which((x[columns == "crop"] - 1) %% 26 + 1 == fao$number)
  maxValue <- as.numeric(fao[row, c("albedo")])
  out <- (x[columns %in% paste0("Kc.", 1:12)] > 0) * maxValue

  return(out)
}
add.displacement <- function(x, columns) {
  x <- as.numeric(x)
  # print(x[columns == "rowname"])

  out <- x[columns %in% paste0("height.", 1:12)] * 0.67

  return(out)
}
add.veg_rough <- function(x, columns) {
  x <- as.numeric(x)
  # print(x[columns == "rowname"])

  out <- x[columns %in% paste0("height.", 1:12)] * 0.123

  return(out)
}
add.LAI <- function(x, columns) {
  x <- as.numeric(x)
  # print(x[columns == "rowname"])

  row <- which((x[columns == "crop"] - 1) %% 26 + 1 == fao$number)
  maxValue <- as.numeric(fao[row, c("LAI")])
  out <- x[columns %in% paste0("Kc.", 1:12)] * maxValue

  return(out)
}

# Calculate
cc <- cbind(cc, kc)

height <- apply(X = cc, MARGIN = 1, FUN = add.height, columns = colnames(cc))
height <- as.data.frame(t(height))
colnames(height) <- paste0("height.", 1:12)

albedo <- apply(X = cc, MARGIN = 1, FUN = add.albedo, columns = colnames(cc))
albedo <- as.data.frame(t(albedo))
colnames(albedo) <- paste0("albedo.", 1:12)

LAI <- apply(X = cc, MARGIN = 1, FUN = add.LAI, columns = colnames(cc))
LAI <- as.data.frame(t(LAI))
colnames(LAI) <- paste0("LAI.", 1:12)

displacement <- apply(X = height, MARGIN = 1, FUN = add.displacement, columns = colnames(height))
displacement <- as.data.frame(t(displacement))
colnames(displacement) <- paste0("displacement.", 1:12)

veg_rough <- apply(X = height, MARGIN = 1, FUN = add.veg_rough, columns = colnames(height))
veg_rough <- as.data.frame(t(veg_rough))
colnames(veg_rough) <- paste0("veg_rough.", 1:12)

# Save
dir.create(dirname(height.out))
write.csv(height, height.out, row.names = F)
dir.create(dirname(albedo.out))
write.csv(albedo, albedo.out, row.names = F)
dir.create(dirname(LAI.out))
write.csv(LAI, LAI.out, row.names = F)
dir.create(dirname(displacement.out))
write.csv(displacement, displacement.out, row.names = F)
dir.create(dirname(veg_rough.out))
write.csv(veg_rough, veg_rough.out, row.names = F)
