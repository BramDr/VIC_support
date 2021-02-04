library(fields)
rm(list = ls())

# Input
lai.file <- "../../../Data/Primary/Dahri2020/vic_in/vegparam.txt"
mapping.file <- "../../../Data/Primary/Dahri2020/vic_in/soilparams.txt"
cv.out <- "../../../Data/Transformed/Parameters/Cv_Dahri_5min_Indus.RDS"
lai.out <- "../../../Data/Transformed/Parameters/LAI_Dahri_5min_Indus.RDS"
albedo.out  <- "../../../Data/Transformed/Parameters/albedo_Dahri_5min_Indus.RDS"
root.depth.out <- "../../../Data/Transformed/Parameters/rootDepth_Dahri_5min_Indus.RDS"
root.fract.out <- "../../../Data/Transformed/Parameters/rootFract_Dahri_5min_Indus.RDS"

# Load
lai.text <- readLines(con = lai.file)
mapping <- read.table(mapping.file)

# Setup
resolution = 1 / 12
out.lon.range = c(min = 66, max = 83)
out.lat.range = c(min = 23, max = 38)

global.lons = seq(from = -180 + resolution / 2, to = 180 - resolution / 2, by = resolution)
global.lats = seq(from = -90 + resolution / 2, to = 90 - resolution / 2, by = resolution)
out.lons = global.lons[global.lons <= out.lon.range["max"] & global.lons >= out.lon.range["min"]]
out.lats = global.lats[global.lats <= out.lat.range["max"] & global.lats >= out.lat.range["min"]]
nclasses <- 16
nmonths <- 12
nroot <- 3

# Calculate
cv.map <- array(NA, dim = c(length(out.lons), length(out.lats), nclasses))
lai.map <- array(NA, dim = c(length(out.lons), length(out.lats), nclasses, nmonths))
albedo.map <- array(NA, dim = c(length(out.lons), length(out.lats), nclasses, nmonths))
root.depth.map <- array(NA, dim = c(length(out.lons), length(out.lats), nclasses, nroot))
root.fract.map <- array(NA, dim = c(length(out.lons), length(out.lats), nclasses, nroot))
mapping.row <- FALSE
lai.row <- FALSE
albedo.row <- FALSE
for (i in 1:length(lai.text)) {
  lai.line <- lai.text[i]
  lai.fields <- strsplit(x = lai.line, split = " ")[[1]]
  lai.fields <- as.numeric(lai.fields)

  if (!mapping.row) {
    cell <- lai.fields[1]
    nveg <- lai.fields[2]
    row <- which(mapping[, 2] == cell)
    
    x.diff = abs(out.lons - mapping[row, 4])
    y.diff = abs(out.lats - mapping[row, 3])
    if(min(x.diff) > resolution / 2 || min(y.diff) > resolution / 2){
      next
    }
    
    x = which.min(x.diff)
    y = which.min(y.diff)

    if (nveg == 0) {
      next
    }

    j <- 1
    mapping.row <- TRUE
  } else {
    if (!lai.row && !albedo.row) {
      veg_class <- lai.fields[1]
      cv.map[x,y,veg_class] <- lai.fields[2]
      root.depth.map[x, y, veg_class, 1:2] <- c(lai.fields[3], lai.fields[5])
      root.fract.map[x, y, veg_class, 1:2] <- c(lai.fields[4], lai.fields[6])

      lai.row <- TRUE
    } else if (lai.row) {
      lai.map[x, y, veg_class, ] <- lai.fields

      lai.row <- FALSE
      albedo.row <- TRUE
    } else {
      albedo.map[x,y,veg_class,] <- lai.fields
      albedo.row <- FALSE
      
      if (j >= nveg) {
        mapping.row <- FALSE
      }
      j <- j + 1
    }
  }
}

# Save
dir.create(dirname(cv.out))
saveRDS(cv.map, cv.out)
dir.create(dirname(lai.out))
saveRDS(lai.map, lai.out)
dir.create(dirname(albedo.out))
saveRDS(albedo.map, albedo.out)
dir.create(dirname(root.depth.out))
saveRDS(root.depth.map, root.depth.out)
dir.create(dirname(root.fract.out))
saveRDS(root.fract.map, root.fract.out)
