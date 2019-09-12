library(fields)
library(plyr)
rm(list = ls())

# Input
cc.file <- "Saves/MIRCA2000_cropping_calendars_corrected3.csv"
cc.paddy.out <- "Saves/MIRCA2000_cell_calendars_paddy.csv"
cc.irr.out <- "Saves/MIRCA2000_cell_calendars_irrigation.csv"
cc.rain.out <- "Saves/MIRCA2000_cell_calendars_rainfed.csv"

# Load
cc <- read.csv(file = cc.file, header = TRUE, stringsAsFactors = F)

# Setup
for (m in 1:12) {
  cc[, paste0("LAI.", m)] <- cc[, paste0("Fcanopy.", m)] * cc[, paste0("LAI.", m)]
  cc[, paste0("displacement.", m)] <- cc[, paste0("Fcanopy.", m)] * cc[, paste0("displacement.", m)]
  cc[, paste0("veg_rough.", m)] <- cc[, paste0("Fcanopy.", m)] * cc[, paste0("veg_rough.", m)]
  cc[, paste0("albedo.", m)] <- cc[, paste0("Fcanopy.", m)] * cc[, paste0("albedo.", m)]
}

cc[, "root_depth.1"] <- cc$Cc * cc[, "root_depth.1"]
cc[, "root_depth.2"] <- cc$Cc * cc[, "root_depth.2"]
cc[, "root_frac.1"] <- cc$Cc * cc[, "root_frac.1"]
cc[, "root_frac.2"] <- cc$Cc * cc[, "root_frac.2"]
cc[, "RGL"] <- cc$Cc * cc[, "RGL"]
cc[, "rarc"] <- cc$Cc * cc[, "rarc"]
cc[, "rad_atten"] <- cc$Cc * cc[, "rad_atten"]
cc[, "overstory"] <- cc$Cc * cc[, "overstory"]
cc[, "trunk_ratio"] <- cc$Cc * cc[, "trunk_ratio"]
cc[, "wind_atten"] <- cc$Cc * cc[, "wind_atten"]
cc[, "wind_h"] <- cc$Cc * cc[, "wind_h"]
cc[, "rmin"] <- cc$Cc * cc[, "rmin"]

# Calculate
fc.paddy <- aggregate(formula = cbind(Fcanopy.1, Fcanopy.2, Fcanopy.3, Fcanopy.4, Fcanopy.5, Fcanopy.6, Fcanopy.7, Fcanopy.8, Fcanopy.9, Fcanopy.10, Fcanopy.11, Fcanopy.12) ~
cell_ID, data = cc[cc$crop == 3, ], FUN = sum)
fc.irr <- aggregate(formula = cbind(Fcanopy.1, Fcanopy.2, Fcanopy.3, Fcanopy.4, Fcanopy.5, Fcanopy.6, Fcanopy.7, Fcanopy.8, Fcanopy.9, Fcanopy.10, Fcanopy.11, Fcanopy.12) ~
cell_ID, data = cc[cc$crop %in% c(1:2, 4:26), ], FUN = sum)
fc.rain <- aggregate(formula = cbind(Fcanopy.1, Fcanopy.2, Fcanopy.3, Fcanopy.4, Fcanopy.5, Fcanopy.6, Fcanopy.7, Fcanopy.8, Fcanopy.9, Fcanopy.10, Fcanopy.11, Fcanopy.12) ~
cell_ID, data = cc[cc$crop %in% c(27:52), ], FUN = sum)

LAI.paddy <- aggregate(formula = cbind(LAI.1, LAI.2, LAI.3, LAI.4, LAI.5, LAI.6, LAI.7, LAI.8, LAI.9, LAI.10, LAI.11, LAI.12) ~
cell_ID, data = cc[cc$crop == 3, ], FUN = sum)
LAI.irr <- aggregate(formula = cbind(LAI.1, LAI.2, LAI.3, LAI.4, LAI.5, LAI.6, LAI.7, LAI.8, LAI.9, LAI.10, LAI.11, LAI.12) ~
cell_ID, data = cc[cc$crop %in% c(1:2, 4:26), ], FUN = sum)
LAI.rain <- aggregate(formula = cbind(LAI.1, LAI.2, LAI.3, LAI.4, LAI.5, LAI.6, LAI.7, LAI.8, LAI.9, LAI.10, LAI.11, LAI.12) ~
cell_ID, data = cc[cc$crop %in% c(27:52), ], FUN = sum)

displacement.paddy <- aggregate(formula = cbind(displacement.1, displacement.2, displacement.3, displacement.4, displacement.5, displacement.6, displacement.7, displacement.8, displacement.9, displacement.10, displacement.11, displacement.12) ~
cell_ID, data = cc[cc$crop == 3, ], FUN = sum)
displacement.irr <- aggregate(formula = cbind(displacement.1, displacement.2, displacement.3, displacement.4, displacement.5, displacement.6, displacement.7, displacement.8, displacement.9, displacement.10, displacement.11, displacement.12) ~
cell_ID, data = cc[cc$crop %in% c(1:2, 4:26), ], FUN = sum)
displacement.rain <- aggregate(formula = cbind(displacement.1, displacement.2, displacement.3, displacement.4, displacement.5, displacement.6, displacement.7, displacement.8, displacement.9, displacement.10, displacement.11, displacement.12) ~
cell_ID, data = cc[cc$crop %in% c(27:52), ], FUN = sum)

veg_rough.paddy <- aggregate(formula = cbind(veg_rough.1, veg_rough.2, veg_rough.3, veg_rough.4, veg_rough.5, veg_rough.6, veg_rough.7, veg_rough.8, veg_rough.9, veg_rough.10, veg_rough.11, veg_rough.12) ~
cell_ID, data = cc[cc$crop == 3, ], FUN = sum)
veg_rough.irr <- aggregate(formula = cbind(veg_rough.1, veg_rough.2, veg_rough.3, veg_rough.4, veg_rough.5, veg_rough.6, veg_rough.7, veg_rough.8, veg_rough.9, veg_rough.10, veg_rough.11, veg_rough.12) ~
cell_ID, data = cc[cc$crop %in% c(1:2, 4:26), ], FUN = sum)
veg_rough.rain <- aggregate(formula = cbind(veg_rough.1, veg_rough.2, veg_rough.3, veg_rough.4, veg_rough.5, veg_rough.6, veg_rough.7, veg_rough.8, veg_rough.9, veg_rough.10, veg_rough.11, veg_rough.12) ~
cell_ID, data = cc[cc$crop %in% c(27:52), ], FUN = sum)

albedo.paddy <- aggregate(formula = cbind(albedo.1, albedo.2, albedo.3, albedo.4, albedo.5, albedo.6, albedo.7, albedo.8, albedo.9, albedo.10, albedo.11, albedo.12) ~
cell_ID, data = cc[cc$crop == 3, ], FUN = sum)
albedo.irr <- aggregate(formula = cbind(albedo.1, albedo.2, albedo.3, albedo.4, albedo.5, albedo.6, albedo.7, albedo.8, albedo.9, albedo.10, albedo.11, albedo.12) ~
cell_ID, data = cc[cc$crop %in% c(1:2, 4:26), ], FUN = sum)
albedo.rain <- aggregate(formula = cbind(albedo.1, albedo.2, albedo.3, albedo.4, albedo.5, albedo.6, albedo.7, albedo.8, albedo.9, albedo.10, albedo.11, albedo.12) ~
cell_ID, data = cc[cc$crop %in% c(27:52), ], FUN = sum)

fixed.paddy <- aggregate(formula = cbind(Cc, root_depth.1, root_depth.2, root_frac.1, root_frac.2, RGL, rarc, rad_atten, overstory, trunk_ratio, wind_atten, wind_h, rmin) ~
cell_ID, data = cc[cc$crop == 3, ], FUN = sum)
fixed.irr <- aggregate(formula = cbind(Cc, root_depth.1, root_depth.2, root_frac.1, root_frac.2, RGL, rarc, rad_atten, overstory, trunk_ratio, wind_atten, wind_h, rmin) ~
cell_ID, data = cc[cc$crop %in% c(1:2, 4:26), ], FUN = sum)
fixed.rain <- aggregate(formula = cbind(Cc, root_depth.1, root_depth.2, root_frac.1, root_frac.2, RGL, rarc, rad_atten, overstory, trunk_ratio, wind_atten, wind_h, rmin) ~
cell_ID, data = cc[cc$crop %in% c(27:52), ], FUN = sum)

fixed.paddy2 <- aggregate(formula = cbind(Cv) ~
cell_ID, data = cc[cc$crop == 3, ], FUN = mean)
fixed.irr2 <- aggregate(formula = cbind(Cv) ~
cell_ID, data = cc[cc$crop %in% c(1:2, 4:26), ], FUN = mean)
fixed.rain2 <- aggregate(formula = cbind(Cv) ~
cell_ID, data = cc[cc$crop %in% c(27:52), ], FUN = mean)

cc.paddy <- merge(fc.paddy, LAI.paddy)
cc.paddy <- merge(cc.paddy, displacement.paddy)
cc.paddy <- merge(cc.paddy, veg_rough.paddy)
cc.paddy <- merge(cc.paddy, albedo.paddy)
cc.paddy <- merge(cc.paddy, fixed.paddy)
cc.paddy <- merge(cc.paddy, fixed.paddy2)
paddy.cols <- colnames(cc.paddy) %in% c("Cc", "root_depth.1", "root_depth.2", "root_frac.1", "root_frac.2", "RGL", "rarc", "rad_atten", "overstory", "trunk_ratio", "wind_atten", "wind_h", "rmin")
cc.paddy[, paddy.cols] <- cc.paddy[, paddy.cols] / cc.paddy$Cv
cc.paddy$overstory <- floor(cc.paddy$overstory)

cc.paddy[, paste0("albedo.", 1:12)] <- cc.paddy[, paste0("albedo.", 1:12)] + ((1 - cc.paddy[, paste0("Fcanopy.", 1:12)]) * 0.2) # bare soil

cc.irr <- merge(fc.irr, LAI.irr)
cc.irr <- merge(cc.irr, displacement.irr)
cc.irr <- merge(cc.irr, veg_rough.irr)
cc.irr <- merge(cc.irr, albedo.irr)
cc.irr <- merge(cc.irr, fixed.irr)
cc.irr <- merge(cc.irr, fixed.irr2)
irr.cols <- colnames(cc.irr) %in% c("Cc", "root_depth.1", "root_depth.2", "root_frac.1", "root_frac.2", "RGL", "rarc", "rad_atten", "overstory", "trunk_ratio", "wind_atten", "wind_h", "rmin")
cc.irr[, irr.cols] <- cc.irr[, irr.cols] / cc.irr$Cv
cc.irr$overstory <- floor(cc.irr$overstory)
cc.irr[, paste0("albedo.", 1:12)] <- cc.irr[, paste0("albedo.", 1:12)] + ((1 - cc.irr[, paste0("Fcanopy.", 1:12)]) * 0.2) # bare soil

cc.rain <- merge(fc.rain, LAI.rain)
cc.rain <- merge(cc.rain, displacement.rain)
cc.rain <- merge(cc.rain, veg_rough.rain)
cc.rain <- merge(cc.rain, albedo.rain)
cc.rain <- merge(cc.rain, fixed.rain)
cc.rain <- merge(cc.rain, fixed.rain2)
rain.cols <- colnames(cc.rain) %in% c("Cc", "root_depth.1", "root_depth.2", "root_frac.1", "root_frac.2", "RGL", "rarc", "rad_atten", "overstory", "trunk_ratio", "wind_atten", "wind_h", "rmin")
cc.rain[, rain.cols] <- cc.rain[, rain.cols] / cc.rain$Cv
cc.rain$overstory <- floor(cc.rain$overstory)
cc.rain[, paste0("albedo.", 1:12)] <- cc.rain[, paste0("albedo.", 1:12)] + ((1 - cc.rain[, paste0("Fcanopy.", 1:12)]) * 0.2) # bare soil

# Save
dir.create(dirname(cc.paddy.out))
write.csv(cc.paddy, cc.paddy.out, row.names = F)
dir.create(dirname(cc.irr.out))
write.csv(cc.irr, cc.irr.out, row.names = F)
dir.create(dirname(cc.rain.out))
write.csv(cc.rain, cc.rain.out, row.names = F)
