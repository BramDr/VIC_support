library(fields)
rm(list = ls())

# Input
root.file <- "../../../Data/Primary/vanVliet2016/global_veg_param.txt"
mapping.file <- "../../../Data/Primary/vanVliet2016/global_soil_file.new.arno.modified.fe.wfd"
root.depth.out <- "../../../Data/Transformed/Parameters/rootDepth_Vliet30min_30min_global.RDS"
root.fract.out <- "../../../Data/Transformed/Parameters/rootFract_Vliet30min_30min_global.RDS"

# Load
root.text <- readLines(con = root.file)
mapping <- read.table(mapping.file)

# Setup
lons <- seq(from = -179.75, to = 179.75, by = 0.5)
lats <- seq(from = -89.75, to = 89.75, by = 0.5)
nclasses <- 12
nroot <- 3

# Calculate
root.depth.map <- array(NA, dim = c(length(lons), length(lats), nclasses, nroot))
root.fract.map <- array(NA, dim = c(length(lons), length(lats), nclasses, nroot))
mapping.row <- FALSE
root.row <- FALSE
for (i in 1:length(root.text)) {
  root.line <- root.text[i]
  root.fields <- strsplit(x = root.line, split = " ")[[1]]
  root.fields <- as.numeric(root.fields)

  if (!mapping.row) {
    cell <- root.fields[1]
    nveg <- root.fields[2]
    row <- which(mapping[, 2] == cell)
    x <- which(lons == mapping[row, 4])
    y <- which(lats == mapping[row, 3])

    if (nveg == 0) {
      next
    }

    j <- 1
    mapping.row <- TRUE
  } else {
    if (!root.row) {
      veg_class <- root.fields[1]
      Cv <- root.fields[2]
      root_depth <- c(root.fields[3], root.fields[5])
      root_fract <- c(root.fields[4], root.fields[6])

      root.depth.map[x, y, veg_class, 1:2] <- root_depth
      root.fract.map[x, y, veg_class, 1:2] <- root_fract

      root.row <- TRUE
    } else {
      root.row <- FALSE
      if (j >= nveg) {
        mapping.row <- FALSE
      }
      j <- j + 1
    }
  }
}
for (i in 1:(nclasses - 1)) {
  image.plot(root.depth.map[, , i, 1], main = i)
  image.plot(root.fract.map[, , i, 1], main = i)
}

# Save
dir.create(dirname(root.depth.out))
saveRDS(root.depth.map, root.depth.out)
dir.create(dirname(root.fract.out))
saveRDS(root.fract.map, root.fract.out)
