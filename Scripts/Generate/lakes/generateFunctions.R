# Add vegetation parameters
AddVegVars = function (file, nveg) {
  # Open
  nc = nc_open(filename = file)
  nc_close(nc = nc)
  
  # Dim
  lon.dim = nc$dim$lon
  lat.dim = nc$dim$lat
  root.dim = nc$dim$root_zone
  month.dim = nc$dim$month
  veg.dim = ncdim_def(name = "veg_class", units = "class", vals = 1:nveg)
  
  # Var
  Nveg.var = ncvar_def(name = "Nveg", units = "", dim = list(lon.dim, lat.dim), missval = -1, prec = "integer", compression = 2)
  # -
  Cv.var = ncvar_def(name = "Cv", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 2)
  wind_atten.var = ncvar_def(name = "wind_atten", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 2)
  wind_h.var = ncvar_def(name = "wind_h", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 2)
  rmin.var = ncvar_def(name = "rmin", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 2)
  rarc.var = ncvar_def(name = "rarc", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 2)
  rad_atten.var = ncvar_def(name = "rad_atten", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 2)
  RGL.var = ncvar_def(name = "RGL", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 2)
  trunk_ratio.var = ncvar_def(name = "trunk_ratio", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 2)
  overstory.var = ncvar_def(name = "overstory", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "integer", compression = 2)
  # -
  root_fract.var = ncvar_def(name = "root_fract", units = "", dim = list(lon.dim, lat.dim, root.dim, veg.dim), missval = -1, prec = "double", compression = 2)
  root_depth.var = ncvar_def(name = "root_depth", units = "", dim = list(lon.dim, lat.dim, root.dim, veg.dim), missval = -1, prec = "double", compression = 2)
  # -
  LAI.var = ncvar_def(name = "LAI", units = "", dim = list(lon.dim, lat.dim, month.dim, veg.dim), missval = -1, prec = "double", compression = 2)
  displacement.var = ncvar_def(name = "displacement", units = "", dim = list(lon.dim, lat.dim, month.dim, veg.dim), missval = -1, prec = "double", compression = 2)
  veg_rough.var = ncvar_def(name = "veg_rough", units = "", dim = list(lon.dim, lat.dim, month.dim, veg.dim), missval = -1, prec = "double", compression = 2)
  albedo.var = ncvar_def(name = "albedo", units = "", dim = list(lon.dim, lat.dim, month.dim, veg.dim), missval = -1, prec = "double", compression = 2)
  
  # Add
  nc = nc_open(filename = file, write = TRUE)
  nc = ncvar_add(nc, Nveg.var)
  nc = ncvar_add(nc, Cv.var)
  nc = ncvar_add(nc, wind_atten.var)
  nc = ncvar_add(nc, wind_h.var)
  nc = ncvar_add(nc, rmin.var)
  nc = ncvar_add(nc, rarc.var)
  nc = ncvar_add(nc, rad_atten.var)
  nc = ncvar_add(nc, RGL.var)
  nc = ncvar_add(nc, trunk_ratio.var)
  nc = ncvar_add(nc, overstory.var)
  nc = ncvar_add(nc, root_fract.var)
  nc = ncvar_add(nc, root_depth.var)
  nc = ncvar_add(nc, LAI.var)
  nc = ncvar_add(nc, displacement.var)
  nc = ncvar_add(nc, veg_rough.var)
  nc = ncvar_add(nc, albedo.var)
  nc_close(nc = nc)
}

# Add lake parameters
AddLakeVars = function (file, nlake) {
  # Open
  nc = nc_open(filename = file)
  nc_close(nc = nc)
  
  # Dim
  lon.dim = nc$dim$lon
  lat.dim = nc$dim$lat
  lake.dim = ncdim_def(name = "lake_class", units = "class", vals = 1:nlake)
  
  # Var
  Nlake.var = ncvar_def(name = "Nlake", units = "", dim = list(lon.dim, lat.dim), missval = -1, prec = "integer", compression = 2)
  lake_veg_class.var = ncvar_def(name = "lake_veg_class", units = "", dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "integer", compression = 2)
  lake_id.var = ncvar_def(name = "lake_id", units = "", dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "integer", compression = 2)
  # -
  source_id.var = ncvar_def(name = "source_id", units = "", dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "integer", compression = 2)
  lake_elevation.var = ncvar_def(name = "lake_elevation", units = "", dim = list(lon.dim, lat.dim, lake.dim), missval = -9999, prec = "double", compression = 2)
  wfrac.var = ncvar_def(name = "wfrac", units = "", dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "double", compression = 2)
  rpercent.var = ncvar_def(name = "rpercent", units = "", dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "double", compression = 2)
  mindepth.var = ncvar_def(name = "mindepth", units = "", dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "double", compression = 2)
  basin_depth.var = ncvar_def(name = "basin_depth", units = "", dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "double", compression = 2)
  depth_in.var = ncvar_def(name = "depth_in", units = "", dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "double", compression = 2)
  basin_frac.var = ncvar_def(name = "basin_area", units = "", dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "double", compression = 2)
  numnod_layer.var = ncvar_def(name = "numnod", units = "", dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "integer", compression = 2)
  numnod_profile.var = ncvar_def(name = "numnod_profile", units = "", dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "integer", compression = 2)
  
  # Add
  nc = nc_open(filename = file, write = TRUE)
  nc = ncvar_add(nc, Nlake.var)
  nc = ncvar_add(nc, lake_veg_class.var)
  nc = ncvar_add(nc, lake_id.var)
  nc = ncvar_add(nc, source_id.var)
  nc = ncvar_add(nc, lake_elevation.var)
  nc = ncvar_add(nc, wfrac.var)
  nc = ncvar_add(nc, rpercent.var)
  nc = ncvar_add(nc, mindepth.var)
  nc = ncvar_add(nc, basin_depth.var)
  nc = ncvar_add(nc, depth_in.var)
  nc = ncvar_add(nc, basin_frac.var)
  nc = ncvar_add(nc, numnod_layer.var)
  nc = ncvar_add(nc, numnod_profile.var)
  nc_close(nc = nc)
}

PutVegVar = function (file, variable, value) {
  print(variable$name)
  
  dims = c(variable$dim[[1]]$len, variable$dim[[2]]$len, variable$dim[[3]]$len)
  if(variable$ndims == 4) {
    dims = c(dims, variable$dim[[4]]$len)
  }
  
  dat = array(data = value, dim = dims)
  if(dims[3] == 3){
    dat[,,3,] = 0
  }
  
  ncvar_put(file, variable, dat)
  rm(dat)
}

# Add vegetation parameters
PutVegVars = function (file, Cv, Nveg) {
  # Open
  nc = nc_open(filename = file, write = TRUE)
  ncvar_put(nc, nc$var$Nveg, Nveg)
  ncvar_put(nc, nc$var$Cv, Cv)
  
  PutVegVar(nc, nc$var$wind_atten, 0.5)
  PutVegVar(nc, nc$var$wind_h, 2)
  PutVegVar(nc, nc$var$rmin, 100)
  PutVegVar(nc, nc$var$rarc, 25)
  PutVegVar(nc, nc$var$rad_atten, 0.5)
  PutVegVar(nc, nc$var$RGL, 100)
  PutVegVar(nc, nc$var$trunk_ratio, 0.52)
  PutVegVar(nc, nc$var$overstory, 0)
  PutVegVar(nc, nc$var$root_fract, 0.5)
  PutVegVar(nc, nc$var$root_depth, 0.3)
  PutVegVar(nc, nc$var$LAI, 5)
  PutVegVar(nc, nc$var$displacement, 1)
  PutVegVar(nc, nc$var$veg_rough, 1)
  PutVegVar(nc, nc$var$albedo, 0.2)
  
  nc_close(nc = nc)
}

# Add lake parameters
PutLakeVars = function (file, lakes, Cl, Nlake, max.surface.depth = 0.6, max.layer.depth = 0.5) {
  # Open
  nc = nc_open(filename = file)
  Cv = ncvar_get(nc, "Cv")
  nc_close(nc = nc)
  
  # Alloc
  lake_veg_class = array(data = -1, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$lake_class$len))
  lake_id = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$lake_class$len))
  source_id = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$lake_class$len))
  lake_elevation = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$lake_class$len))
  wfrac = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$lake_class$len))
  rpercent = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$lake_class$len))
  mindepth = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$lake_class$len))
  basin_depth = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$lake_class$len))
  depth_in = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$lake_class$len))
  numnod_profile = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$lake_class$len))
  numnod = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$lake_class$len))
  
  for(i in 1:nrow(lakes)){
    x = lakes$x[i]
    y = lakes$y[i]
    
    for(z in 1:nc$dim$lake_class$len){
      if(lake_veg_class[x,y,z] == -1){
        lake_veg_class[x,y,z] = z
        lake_id[x,y,z] = lakes$ID[i]
        source_id[x,y,z] = lakes$sourceID[i]
        lake_elevation[x,y,z] = lakes$elevation[i]
        wfrac[x,y,z] = 0.2
        rpercent[x,y,z] = 0.2
        mindepth[x,y,z] = 0.5
        numnod_profile[x,y,z] = 10
        if (lakes$depth[i] < max.surface.depth) {
          numnod[x,y,z] = 1
        } else if (lakes$depth[i] < max.surface.depth + max.layer.depth) {
          numnod[x,y,z] = 2
        } else {
          numnod[x,y,z] = 1 + floor((lakes$depth[i] - max.surface.depth) / max.layer.depth)
        }
        depth_in[x,y,z] = lakes$depth[i]
        basin_depth[x,y,z] = lakes$depth[i] 
        break
      }
    }
  }
  
  # Write
  nc = nc_open(filename = file, write = TRUE)
  print(nc$var$Nlake$name)
  ncvar_put(nc, nc$var$Nlake, Nlake)
  print(nc$var$lake_veg_class$name)
  ncvar_put(nc, nc$var$lake_veg_class, lake_veg_class)
  print(nc$var$lake_id$name)
  ncvar_put(nc, nc$var$lake_id, lake_id)
  print(nc$var$source_id$name)
  ncvar_put(nc, nc$var$source_id, source_id)
  print(nc$var$lake_elevation$name)
  ncvar_put(nc, nc$var$lake_elevation, lake_elevation)
  print(nc$var$numnod_profile$name)
  ncvar_put(nc, nc$var$numnod_profile, numnod_profile)
  print(nc$var$numnod$name)
  ncvar_put(nc, nc$var$numnod, numnod)
  print(nc$var$depth_in$name)
  ncvar_put(nc, nc$var$depth_in, depth_in)
  print(nc$var$basin_depth$name)
  ncvar_put(nc, nc$var$basin_depth, basin_depth)
  print(nc$var$basin_area$name)
  ncvar_put(nc, nc$var$basin_area, Cl)
  print(nc$var$wfrac$name)
  ncvar_put(nc, nc$var$wfrac, wfrac)
  print(nc$var$rpercent$name)
  ncvar_put(nc, nc$var$rpercent, rpercent)
  print(nc$var$mindepth$name)
  ncvar_put(nc, nc$var$mindepth, mindepth)
  nc_close(nc = nc)
}

# Get nearest values
getNearest = function(x, y, vals, na.rm = T){
  for(dis in 1:10){
    x.min = max(x - dis, 1)
    y.min = max(y - dis, 1)
    x.max = min(x + dis, dim(vals)[1])
    y.max = min(y + dis, dim(vals)[2])
    
    val = mean(vals[x.min:x.max, y.min:y.max], na.rm = na.rm)
    
    if(length(val) != 0){
      return(val)
    }
  }
  
  warning("Nearest out of iterations")
  return(NA)
}

# Add other parameters for missing cells
PutOtherVars = function(file, vars, Nlake) {
  # Open
  nc = nc_open(filename = file, write = T)
  for(ivar in 1:nc$nvars){
    var = nc$var[[ivar]]
    
    if(!var$name %in% vars){
      next
    }
    if(var$dim[[1]]$name != "lon" ||
       var$dim[[2]]$name != "lat"){
      next
    }
    print(var$name)
    
    data.orig = ncvar_get(nc, var)
    dims.orig = dim(data.orig)
    dims.new = rep(1, 4)
    for(i in 1:length(dims.orig)){
      dims.new[i] = dims.orig[i]
    }
    data.new = array(NA, dim = dims.new)
    
    if(length(dims.orig) == 4){
      data.new = data.orig
    } else if(length(dims.orig) == 3){
      data.new[,,,1] = data.orig
    } else if(length(dims.orig) == 2){
      data.new[,,1,1] = data.orig
    }
    
    for(x in 1:dims.new[1]){
      for(y in 1:dims.new[2]){
        if(is.na(Nlake[x,y]) || Nlake[x,y] <= 0){
          next
        }
        if(!is.na(data.new[x,y,1,1])){
          next
        }
        for(z in 1:dims.new[3]){
          for(v in 1:dims.new[4]){
            data.new[x,y,z,v] = getNearest(x,y,data.new[,,z,v])   
          }
        }
      }
    }
    
    ncvar_put(nc, var, data.new)
  }
  
  nc_close(nc = nc)
}