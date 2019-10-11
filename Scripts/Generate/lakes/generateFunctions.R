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
  Nveg.var = ncvar_def(name = "Nveg", units = "", dim = list(lon.dim, lat.dim), missval = -1, prec = "integer")
  # -
  Cv.var = ncvar_def(name = "Cv", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double")
  wind_atten.var = ncvar_def(name = "wind_atten", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double")
  wind_h.var = ncvar_def(name = "wind_h", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double")
  rmin.var = ncvar_def(name = "rmin", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double")
  rarc.var = ncvar_def(name = "rarc", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double")
  rad_atten.var = ncvar_def(name = "rad_atten", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double")
  RGL.var = ncvar_def(name = "RGL", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double")
  trunk_ratio.var = ncvar_def(name = "trunk_ratio", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double")
  overstory.var = ncvar_def(name = "overstory", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "integer")
  # -
  root_fract.var = ncvar_def(name = "root_fract", units = "", dim = list(lon.dim, lat.dim, root.dim, veg.dim), missval = -1, prec = "double")
  root_depth.var = ncvar_def(name = "root_depth", units = "", dim = list(lon.dim, lat.dim, root.dim, veg.dim), missval = -1, prec = "double")
  # -
  LAI.var = ncvar_def(name = "LAI", units = "", dim = list(lon.dim, lat.dim, month.dim, veg.dim), missval = -1, prec = "double")
  displacement.var = ncvar_def(name = "displacement", units = "", dim = list(lon.dim, lat.dim, month.dim, veg.dim), missval = -1, prec = "double")
  veg_rough.var = ncvar_def(name = "veg_rough", units = "", dim = list(lon.dim, lat.dim, month.dim, veg.dim), missval = -1, prec = "double")
  albedo.var = ncvar_def(name = "albedo", units = "", dim = list(lon.dim, lat.dim, month.dim, veg.dim), missval = -1, prec = "double")
  
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
AddLakeVars = function (file) {
  # Open
  nc = nc_open(filename = file)
  nc_close(nc = nc)
  
  # Dim
  lon.dim = nc$dim$lon
  lat.dim = nc$dim$lat
  veg.dim = nc$dim$veg_class
  
  # Var
  Nlake.var = ncvar_def(name = "Nlake", units = "", dim = list(lon.dim, lat.dim), missval = -1, prec = "integer")
  lake_id.var = ncvar_def(name = "lake_id", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "integer")
  # -
  source_id.var = ncvar_def(name = "source_id", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "integer")
  lake_elevation.var = ncvar_def(name = "lake_elevation", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -9999, prec = "double")
  wfrac.var = ncvar_def(name = "wfrac", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double")
  rpercent.var = ncvar_def(name = "rpercent", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double")
  mindepth.var = ncvar_def(name = "mindepth", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double")
  basin_depth.var = ncvar_def(name = "basin_depth", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double")
  depth_in.var = ncvar_def(name = "depth_in", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double")
  basin_frac.var = ncvar_def(name = "basin_area", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double")
  numnod_layer.var = ncvar_def(name = "numnod", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "integer")
  numnod_profile.var = ncvar_def(name = "numnod_profile", units = "", dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "integer")
  
  # Add
  nc = nc_open(filename = file, write = TRUE)
  nc = ncvar_add(nc, Nlake.var)
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

# Add vegetation parameters
PutVegVars = function (file, Nveg) {
  # Open
  nc = nc_open(filename = file)
  nc_close(nc = nc)
  
  # Alloc
  Cv = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  wind_atten = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  wind_h = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  rmin = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  rarc = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  rad_atten = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  RGL = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  trunk_ratio = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  overstory = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  root_fract = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$root_zone$len, nc$dim$veg_class$len))
  root_depth = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$root_zone$len, nc$dim$veg_class$len))
  LAI = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$month$len, nc$dim$veg_class$len))
  displacement = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$month$len, nc$dim$veg_class$len))
  veg_rough = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$month$len, nc$dim$veg_class$len))
  albedo = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$month$len, nc$dim$veg_class$len))
  
  for(x in 1:dim(Cv)[1]){
    for(y in 1:dim(Cv)[2]){
      if(is.na(Nveg[x,y]) || Nveg[x,y] <= 0){
        next
      }
      
      for(z in 1:dim(Cv)[3]){
        if(z <= Nveg[x,y]){
          Cv[x,y,z] = 1 / Nveg[x,y]
        } else {
          Cv[x,y,z] = 0
        }
      }
      
      wind_atten[x,y,] = 0.5
      wind_h[x,y,] = 2
      rmin[x,y,] = 100
      rarc[x,y,] = 25
      rad_atten[x,y,] = 0.5
      RGL[x,y,] = 100
      trunk_ratio[x,y,] = 0.2
      overstory[x,y,] = 0
      
      for(l in 1:dim(root_fract)[3]){
        for(v in 1:dim(root_fract)[4]){
          if(!(l == dim(root_fract)[3])){
            root_fract[x,y,l,v] = 0.5
            root_depth[x,y,l,v] = 0.3
          }
        }
      }
      
      LAI[x,y,,] = 5
      displacement[x,y,,] = 1
      veg_rough[x,y,,] = 1
      albedo[x,y,,] = 0.2
    }
  }
  
  # Write
  nc = nc_open(filename = file, write = TRUE)
  ncvar_put(nc, nc$var$Nveg, Nveg)
  ncvar_put(nc, nc$var$Cv, Cv)
  ncvar_put(nc, nc$var$wind_atten, wind_atten)
  ncvar_put(nc, nc$var$wind_h, wind_h)
  ncvar_put(nc, nc$var$rmin, rmin)
  ncvar_put(nc, nc$var$rarc, rarc)
  ncvar_put(nc, nc$var$rad_atten, rad_atten)
  ncvar_put(nc, nc$var$RGL, RGL)
  ncvar_put(nc, nc$var$trunk_ratio, trunk_ratio)
  ncvar_put(nc, nc$var$overstory, overstory)
  ncvar_put(nc, nc$var$root_fract, root_fract)
  ncvar_put(nc, nc$var$root_depth, root_depth)
  ncvar_put(nc, nc$var$LAI, LAI)
  ncvar_put(nc, nc$var$displacement, displacement)
  ncvar_put(nc, nc$var$veg_rough, veg_rough)
  ncvar_put(nc, nc$var$albedo, albedo)
  nc_close(nc = nc)
}

# Add lake parameters
PutLakeVars = function (file, lakes, area, Nlake) {
  # Open
  nc = nc_open(filename = file)
  Cv = ncvar_get(nc, "Cv")
  nc_close(nc = nc)
  
  # Alloc
  lake_id = array(data = -1, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  source_id = array(data = -1, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  lake_elevation = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  wfrac = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  rpercent = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  mindepth = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  basin_depth = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  depth_in = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  basin_area = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  numnod_profile = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  numnod = array(data = 0, dim = c(nc$dim$lon$len, nc$dim$lat$len, nc$dim$veg_class$len))
  
  for(i in 1:nrow(lakes)){
    x = lakes$x[i]
    y = lakes$y[i]
    
    for(z in 1:nc$dim$veg_class$len){
      if(lake_id[x,y,z] == -1){
        lake_id[x,y,z] = lakes$ID[i]
        source_id[x,y,z] = lakes$sourceID[i]
        lake_elevation[x,y,z] = lakes$elevation[i]
        wfrac[x,y,z] = 0.2
        rpercent[x,y,z] = 0.2
        mindepth[x,y,z] = 0.5
        numnod_profile[x,y,z] = 10
        numnod[x,y,z] = ceiling(lakes$depth[i] * 2)
        depth_in[x,y,z] = lakes$depth[i]
        basin_depth[x,y,z] = lakes$depth[i]
        basin_area[x,y,z] = lakes$fraction[i]
        break
      }
    }
  }
  
  # Write
  nc = nc_open(filename = file, write = TRUE)
  ncvar_put(nc, nc$var$Nlake, Nlake)
  ncvar_put(nc, nc$var$lake_id, lake_id)
  ncvar_put(nc, nc$var$source_id, source_id)
  ncvar_put(nc, nc$var$lake_elevation, lake_elevation)
  ncvar_put(nc, nc$var$numnod_profile, numnod_profile)
  ncvar_put(nc, nc$var$numnod, numnod)
  ncvar_put(nc, nc$var$depth_in, depth_in)
  ncvar_put(nc, nc$var$basin_depth, basin_depth)
  ncvar_put(nc, nc$var$basin_area, basin_area)
  ncvar_put(nc, nc$var$wfrac, wfrac)
  ncvar_put(nc, nc$var$rpercent, rpercent)
  ncvar_put(nc, nc$var$mindepth, mindepth)
  nc_close(nc = nc)
}

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