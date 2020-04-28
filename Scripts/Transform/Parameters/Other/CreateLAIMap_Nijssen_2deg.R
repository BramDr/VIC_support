library(fields)
rm(list = ls())

# Input
lai.file <- "../../../Data/Primary/Nijssen2001/2deg/world_veg_parameters.0.01_threshold.txt"
mapping.file <- "../../../Data/Primary/Nijssen2001/2deg/world.soil.parameter.txt"
lai.out <- "./Saves/LAI_Nijssen120min_30min_global.RDS"

# Load
lai.text <- readLines(con = lai.file)
mapping <- read.table(file = mapping.file)
mapping <- mapping[, 2:4]

# Setup
lons.120min <- seq(from = -179, to = 179, by = 2)
lats.120min <- seq(from = -89, to = 89, by = 2)
lons.30min <- seq(from = -179.75, to = 179.75, by = 0.5)
lats.30min <- seq(from = -89.75, to = 89.75, by = 0.5)
nclasses <- 12
nmonths <- 12

mapping.map <- array(NA, dim = c(length(lons.30min), length(lats.30min), 2))
for (x in 1:dim(mapping.map)[1]) {
  for (y in 1:dim(mapping.map)[2]) {
    x.map <- which.min(abs(lons.120min - lons.30min[x]))
    y.map <- which.min(abs(lats.120min - lats.30min[y]))
    mapping.map[x, y, 1] <- x.map
    mapping.map[x, y, 2] <- y.map
  }
}
image.plot(mapping.map[, , 1])
image.plot(mapping.map[, , 2])

# Calculate
lai.map <- array(NA, dim = c(length(lons.120min), length(lats.120min), nclasses, nmonths))
mapping.row <- FALSE
lai.row <- FALSE
for (i in 1:length(lai.text)) {
  lai.line <- lai.text[i]
  lai.fields <- strsplit(x = lai.line, split = " ")[[1]]
  lai.fields <- as.numeric(lai.fields)

  if (!mapping.row) {
    cell <- lai.fields[1]
    nveg <- lai.fields[2]
    row <- which(mapping[, 1] == cell)
    x <- which(lons.120min == mapping[row, 3])
    y <- which(lats.120min == mapping[row, 2])

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
      lai.map[x, y, veg_class, ] <- lai.fields

      lai.row <- FALSE
      if (j >= nveg) {
        mapping.row <- FALSE
      }
      j <- j + 1
    }
  }
}
for (i in 1:nmonths) {
  image.plot(lai.map[, , 10, i], main = i)
}

lai.map.adj <- array(NA, dim = c(length(lons.30min), length(lats.30min), nclasses, nmonths))
for (x in 1:dim(lai.map.adj)[1]) {
  for (y in 1:dim(lai.map.adj)[2]) {
    x.map <- mapping.map[x, y, 1]
    y.map <- mapping.map[x, y, 2]
    lai.map.adj[x, y, , ] <- lai.map[x.map, y.map, , ]
  }
}
for (i in 1:nmonths) {
  image.plot(lai.map.adj[, , 10, i], main = i)
}

# Save
dir.create(dirname(lai.out))
saveRDS(lai.map.adj, lai.out)
