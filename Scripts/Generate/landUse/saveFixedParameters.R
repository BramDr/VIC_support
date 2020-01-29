library(fields)
library(plyr)
rm(list = ls())

# Input
fao.file <- "../../../Data/Primary/FAO/Allen1998/FAO_crop_characteristics.csv"
cc.file <- "../../../Data/Transformed/LandUse/subcropCalendar_30min_global.csv"
fixed.out <- "Saves/croppingCalendars_fixed.csv"

# Load
cc <- read.csv(file = cc.file, header = TRUE, stringsAsFactors = F)
fao <- read.csv(fao.file, stringsAsFactors = F)

# Setup
add.fixed <- function(x, columns) {
  x <- as.numeric(x)
  print(x[columns == "rowname"])

  row <- which((x[columns == "crop"] - 1) %% 26 + 1 == fao$number)

  root_depth.1 <- as.numeric(fao$root_depth.1[row])
  root_depth.2 <- as.numeric(fao$root_depth.2[row])
  root_frac.1 <- as.numeric(fao$root_frac.1[row])
  root_frac.2 <- as.numeric(fao$root_frac.2[row])
  RGL <- as.numeric(fao$RGL[row])
  rarc <- as.numeric(fao$rarc[row])
  rad_atten <- as.numeric(fao$rad_atten[row])
  overstory <- as.numeric(fao$overstory[row])
  trunk_ratio <- as.numeric(fao$trunk_ratio[row])
  wind_atten <- as.numeric(fao$wind_atten[row])
  wind_h <- as.numeric(fao$wind_h[row])
  rmin <- as.numeric(fao$rmin[row])

  out <- c(root_depth.1, root_depth.2, root_frac.1, root_frac.2, RGL, rarc, rad_atten, overstory, trunk_ratio, wind_atten, wind_h, rmin)
  return(out)
}

# Calculate
fixed <- apply(X = cc, MARGIN = 1, FUN = add.fixed, columns = colnames(cc))
fixed <- as.data.frame(t(fixed))
colnames(fixed) <- c("root_depth.1", "root_depth.2", "root_frac.1", "root_frac.2", "RGL", "rarc", "rad_atten", "overstory", "trunk_ratio", "wind_atten", "wind_h", "rmin")

# Save
dir.create(dirname(fixed.out))
write.csv(fixed, fixed.out, row.names = F)
