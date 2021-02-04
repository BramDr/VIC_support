rm(list = ls())

# Input
nijssen.file <- "../../../../Data/Primary/Nijssen2001/vic_global_2001/config/vicveg/world_newveg_lib.txt/world_newveg_lib.txt"
vegetation.out <- "./Saves/vegetation_mapping.csv"

# Load
nijssen.header = read.table(nijssen.file, nrows = 1, comment.char = "", stringsAsFactors = F)
nijssen = read.table(nijssen.file, sep = "\t", fill = T, header = F, stringsAsFactors = F)
nijssen = nijssen[1:(nrow(nijssen) - 1),]
colnames(nijssen) = as.character(nijssen.header)

vegs = nijssen[,c(ncol(nijssen) - 1, ncol(nijssen))]
vegs = apply(X = vegs, MARGIN = 1, FUN = function(x){paste(x, collapse = " ")})

roughness.columns = grep(x = colnames(nijssen), pattern = "-ROU", value = T)
displacement.columns = grep(x = colnames(nijssen), pattern = "-DIS", value = T)
heights.1 = nijssen[,roughness.columns[1]] / 0.123
heights.2 = nijssen[,displacement.columns[1]] / 0.67
heights = (heights.1 + heights.2) / 2

nijssen[,c(ncol(nijssen) - 1, ncol(nijssen), 3, 4)]
nijssen[,c(ncol(nijssen) - 1, ncol(nijssen), (ncol(nijssen) - 6):(ncol(nijssen) - 2))]

# Setup
vegetation = data.frame(
  name = c("Water", 
           "Evergreen needleleaf forests", 
           "Evergreen broadleaf forests",
           "Deciduous needleleaf forests",
           "Deciduous broadleaf forests",
           "Mixed forests",
           "Closed shrublands",
           "Open shrublands",
           "Woody savannas",
           "Savannas",
           "Grasslands",
           "Permanent wetlands",
           "Croplands",
           "Urband and build-up lands",
           "Cropland/natural vegetation mosaics",
           "Barren"),
  modis = 1:16,
  vic = c(NA, 1:13, 12, 14),
  
  fcanopy = c(0, rep(1, 14), 0),
  lai = c(0, rep(5, 13), 0, 0),
  albedo = c(0.2, rep(0.1,14), 0.2),
  
  height = c(0, 20, 30, 10, 20, 15, 5, 5, 2.5, 1, 0.5, 1, 2, 0.5, 2, 0),
  root.depth.1 = c(0, rep(0.3, 14), 0),
  root.depth.2 = c(0, rep(0.7, 14), 0),
  root.frac.1 = c(0, rep(0.3, 5), 0.6, rep(0.7, 2), 0.6, rep(0.8, 5), 0),
  root.frac.2 = c(0, rep(0.7, 5), 0.4, rep(0.3, 2), 0.4, rep(0.2, 5), 0),
  overstory = c(0, rep(1, 5), rep(0,10)),
  rarc = c(0, 50, 25, 40, 40, 40, 3, 2.5, 3, 2, 2, 2.5, 2, 2, 2, 0),
  rmin = c(0, 120, 80, 120, 80, 100, 110, 110, 110, 110, 80, 110, 80, 80, 80, 0),
  rgl = c(0, rep(30, 4), 50, rep(75, 4), 100, 75, 100, 75, 100, 0),
  sol.atten = rep(0.5, 16),
  wind.atten = rep(0.5, 16),
  trunk.ratio = c(0, rep(0.2, 14), 0),
  wind.h = c(0, 20, 30, 10, 20, 15, 5, 5, 2.5, 1, 0.5, 1, 2, 0.5, 2, 0) + 2
)

# Save
dir.create(dirname(vegetation.out))
write.csv(vegetation, vegetation.out, row.names = F)
