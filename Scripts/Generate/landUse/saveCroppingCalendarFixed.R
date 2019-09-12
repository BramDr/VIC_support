library(fields)
library(plyr)
rm(list = ls())

# Input
area.file <- "../../../Data/Primary/MIRCA2000/Cell area grid/cell_area_ha_30mn.asc"
fao.file <- "../../../Data/Primary/FAO/Allen1998/FAO_crop_characteristics.csv"
cc.file <- "Saves/MIRCA2000_cropping_calendars_corrected2.csv"
cc.out <- "Saves/MIRCA2000_cropping_calendars_corrected3.csv"

# Load
cc <- read.csv(file = cc.file, header = TRUE, stringsAsFactors = F)
fao <- read.csv(fao.file, stringsAsFactors = F)
area <- as.matrix(read.table(file = area.file, header = FALSE, sep = " ", skip = 6, stringsAsFactors = F))
area <- area[, 1:720]

# Setup
add.fixed <- function(x, columns) {
  x <- as.numeric(x)
  # print(x[columns == "rowname"])

  row <- which((x[columns == "crop"] - 1) %% 26 + 1 == fao$number)

  Cv <- x[columns == "areaMax"] / area[x[columns == "row"], x[columns == "column"]]
  Cc <- x[columns == "cropAreaMax"] / area[x[columns == "row"], x[columns == "column"]]
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

  out <- c(Cv, Cc, root_depth.1, root_depth.2, root_frac.1, root_frac.2, RGL, rarc, rad_atten, overstory, trunk_ratio, wind_atten, wind_h, rmin)
  return(out)
}

# Calculate
fixed <- apply(X = cc, MARGIN = 1, FUN = add.fixed, columns = colnames(cc))
fixed <- t(fixed)

cc <- cbind(cc, fixed)
new.colnames <- colnames(cc)[1:(ncol(cc) - 14)]
new.colnames <- c(new.colnames, c("Cv", "Cc", "root_depth.1", "root_depth.2", "root_frac.1", "root_frac.2", "RGL", "rarc", "rad_atten", "overstory", "trunk_ratio", "wind_atten", "wind_h", "rmin"))
colnames(cc) <- new.colnames

# Save
dir.create(dirname(cc.out))
write.csv(cc, cc.out, row.names = F)
