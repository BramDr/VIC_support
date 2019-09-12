library(fields)
library(ncdf4)
rm(list = ls())

# Input
function.script = "generateFunctions.R"
vegetation.file = "Input/VIC_params_global.nc"
Cc.monthly.file = "Saves/CcMonthly_30min_global.RDS"
Cv.monthly.file = "Saves/CvMonthly_30min_global.RDS"
vegetation.out = "Output/VIC_params_MIRCA2000_full_global.nc"

# Load
source(function.script)

Cc.monthly = readRDS(Cc.monthly.file)
Cv.monthly = readRDS(Cv.monthly.file)

# Setup
remove.vars = c("veg_descr", "veg_class","Nveg","Cv","wind_atten",
                "wind_h","rmin","rarc","rad_atten","RGL",
                "trunk_ratio","overstory","root_fract", "root_depth", 
                "LAI", "displacement", "veg_rough", "albedo")
system(command = paste0("ncks -x -v ", paste0(remove.vars, collapse = ","), " ", vegetation.file, " -O ", vegetation.out))

nc = nc_open(vegetation.file)
nc_close(nc)

lon.dim = nc$dim$lon
lat.dim = nc$dim$lat
root.dim = nc$dim$root_zone
month.dim = nc$dim$month
veg.dim = ncdim_def(name = "veg_class", units = "N/A", vals = 1:63, 
                    longname = "Vegetation class: 1 - Evergreen Needleleaf, 2 - Evergreen Broadleaf, 3 - Deciduous Needleaf, 
                    4 - Diciduous Broadleaf, 5 - Mixed Cover, 6 - Woodland, 7 - Wooded Grasslands, 8 - Closed Shrublands, 
                    9 - Open Shrublands, 10 - Grasslands, 11 - 62 MIRCA2000 crops, 63 - Bare Soil")
addVegVars(vegetation.out, lon.dim, lat.dim, veg.dim, root.dim, month.dim)

Cc.max = apply(X = Cc.monthly, MARGIN = c(1,2,3), FUN = max)
Cv.max = apply(X = Cv.monthly, MARGIN = c(1,2,3), FUN = max)
Cv = array(0, dim = c(720, 360, dim(Cc.max)[3] + dim(Cv.max)[3] - 1))
Cv[,,1:10] = Cv.max[,,1:10]
Cv[,,11:62] = Cc.max[,,1:52]
Cv[,,63] = Cv.max[,,11] + Cc.max[,,53]

Nveg = apply(X = Cv[,,1:(dim(Cv)[3] - 1)], MARGIN = c(1,2), FUN = function(x){sum(x > 0, na.rm = T)})
Nlu = apply(X = Cv, MARGIN = c(1,2), FUN = function(x){sum(x > 0, na.rm = T)})
image.plot(Nveg)
image.plot(Nlu)

get.vars = remove.vars[!remove.vars %in% c("veg_descr", "veg_class", "Nveg", "Cv")]
vars = list()
nc = nc_open(vegetation.file)
for(var in get.vars){
  vars[[var]] = ncvar_get(nc, var)
}
nc_close(nc)

# Calculate
Cv.new = Cv
for(x in 1:dim(Cv.new)[1]){
  for(y in 1:dim(Cv.new)[2]){
    for(z in 1:dim(Cv.new)[3]){
      if(Cv[x,y,z] > 0){
        Cv.new[x,y,z] = 1 / Nlu[x,y]
      }
    }
  }
}
Cv.sum = apply(X = Cv.new, MARGIN = c(1,2), FUN = sum)
image.plot(Cv.sum)

nc = nc_open(vegetation.out, write = T)
ncvar_put(nc, "Nveg", Nveg)
ncvar_put(nc, "Cv", Cv.new)

for(var in names(vars)){
  print(paste0("Working on ", var))
  
  var.data = vars[[var]]
  
  # Fixed variables
  if(length(dim(var.data)) == 3){
    var.data.adj = array(NA, dim = c(720, 360, dim(Cv)[3]))
    var.data.adj[,,1:10] = var.data[,,1:10] # Veg
    for(c in 11:62){
      var.data.adj[,,c] = var.data[,,11] # Crop
    }
    var.data.adj[,,63] = var.data[,,12] # Bare
  }
  
  # Root & monthly variables
  else if(length(dim(var.data)) == 4){
    var.data.adj = array(NA, dim = c(720, 360, dim(var.data)[3], dim(Cv)[3]))
    var.data.adj[,,,1:10] = var.data[,,,1:10] # Veg
    for(c in 11:62){
      var.data.adj[,,,c] = var.data[,,,11] # Crop
    }
    var.data.adj[,,,63] = var.data[,,,12] # Bare
  }
    
  # Adjust root
  if(var == "root_fract" || var == "root_depth"){
    var.data.apply = apply(X = var.data.adj, MARGIN = c(3,4), FUN = max, na.rm = T)
    var.data.omit = is.na(var.data.adj)
    
    for(v in 1:dim(var.data.adj)[4]){
      for(z in 1:dim(var.data.adj)[3]){
         var.data.adj[,,z,v] = (!is.na(var.data.adj[,,z,v])) * var.data.apply[z,v]
      }
	  var.data.adj[var.data.omit] = NA
    }
  }
  
  # Adjust LAI
  else if(var == "LAI"){
    var.data.apply = apply(X = var.data.adj, MARGIN = c(4), FUN = max, na.rm = T)
    var.data.omit = is.na(var.data.adj)
    
    for(v in 1:dim(var.data.adj)[4]){
      var.data.adj[,,,v] = (!is.na(var.data.adj[,,,v])) * var.data.apply[v]
    }
	var.data.adj[var.data.omit] = NA
  }
  
  ncvar_put(nc, var, var.data.adj)
}
nc_close(nc)
