library(fields)
rm(list = ls())

# Input
lai.file <- "../../../Data/Primary/Nijssen2001/0.5deg/global_veg_param_new"
mapping.file <- "../../../Data/Primary/Nijssen2001/0.5deg/global_soil_param_new"
lai.out <- "./Saves/LAI_Nijssen30min_30min_global.RDS"

# Load
lai.text <- readLines(con = lai.file)
mapping <- read.table(mapping.file)

# Setup
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)
nclasses <- 12
nmonths <- 12

# Calculate
lai.map <- array(NA, dim = c(length(lons), length(lats), nclasses, nmonths))
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
    x <- which(lons == mapping[row, 4])
    y <- which(lats == mapping[row, 3])

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
  image.plot(lai.map[, , 1, i], main = i)
}

# Save
dir.create(dirname(lai.out))
saveRDS(lai.map, lai.out)
