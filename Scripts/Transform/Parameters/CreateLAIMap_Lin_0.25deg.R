library(fields)
rm(list = ls())

# Input
lai.file <- "../../../Data/Primary/Lin2019/global_lai_0.25deg.txt"
mapping.file <- "../../../Data/Primary/Lin2019/global_VIC_soil_0.25d.txt"
lai.out <- "../../../Data/Transformed/Parameters/LAI_Lin15min_30min_global.RDS"

# Load
lai.text <- readLines(con = lai.file)

mapping <- read.table(mapping.file)
mapping[mapping[, 4] > 180, 4] <- mapping[mapping[, 4] > 180, 4] - 360

# Setup
lons.15min <- seq(from = -179.875, to = 179.875, by = 0.25)
lats.15min <- seq(from = -89.875, to = 89.875, by = 0.25)
lons.30min <- seq(from = -179.75, to = 179.75, by = 0.5)
lats.30min <- seq(from = -89.75, to = 89.75, by = 0.5)
nclasses <- 12
nmonths <- 12

mapping.map <- array(NA, dim = c(length(lons.15min), length(lats.15min), 2))
for (x in 1:dim(mapping.map)[1]) {
  for (y in 1:dim(mapping.map)[2]) {
    x.map <- which.min(abs(lons.30min - lons.15min[x]))
    y.map <- which.min(abs(lats.30min - lats.15min[y]))
    mapping.map[x, y, 1] <- x.map
    mapping.map[x, y, 2] <- y.map
  }
}
# image.plot(mapping.map[, , 1])
# image.plot(mapping.map[, , 2])

averageMap <- function(map, count) {
  for (i in 1:dim(map)[3]) {
    for (j in 1:dim(map)[4]) {
      mapi <- map[, , i, j]
      mapi <- mapi / count[, , i]
      mapi[count[, , i] == 0] <- NA
      map[, , i, j] <- mapi
    }
  }
  return(map)
}

# Calculate
lai.map <- array(0, dim = c(length(lons.30min), length(lats.30min), nclasses, nmonths))
count.map <- array(0, dim = c(length(lons.30min), length(lats.30min), nclasses))
mapping.row <- FALSE
lai.row <- FALSE
for (i in 1:length(lai.text)) {
  lai.line <- lai.text[i]
  lai.fields <- strsplit(x = lai.line, split = " ")[[1]]
  lai.fields <- as.numeric(lai.fields)

  if (!mapping.row) {
    cell <- lai.fields[1]
    nveg <- lai.fields[2]
    row <- which(mapping[, 2] == cell)

    x.15min <- which(lons.15min == mapping[row, 4])
    y.15min <- which(lats.15min == mapping[row, 3])

    x <- mapping.map[x.15min, y.15min, 1]
    y <- mapping.map[x.15min, y.15min, 2]

    if (nveg == 0) {
      next
    }

    j <- 1
    mapping.row <- TRUE
  } else {
    if (!lai.row) {
      veg_class <- lai.fields[1]
      Cv <- lai.fields[2]
      root_depth <- c(lai.fields[3], lai.fields[5])
      root_fract <- c(lai.fields[4], lai.fields[6])

      lai.row <- TRUE
    } else {
      lai.map[x, y, veg_class, ] <- lai.map[x, y, veg_class, ] + lai.fields
      count.map[x, y, veg_class] <- count.map[x, y, veg_class] + 1

      lai.row <- FALSE
      if (j >= nveg) {
        mapping.row <- FALSE
      }
      j <- j + 1
    }
  }
}

lai.map.adj <- averageMap(lai.map, count.map)

for (i in 1:nmonths) {
  image.plot(lai.map.adj[, , 2, i], main = i)
}

# Save
dir.create(dirname(lai.out))
saveRDS(lai.map.adj, lai.out)
