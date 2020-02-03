library(ncdf4)

addLakeVars <- function(nc.file, nlake_class = NULL) {
  # Get dimensions
  nc <- nc_open(filename = nc.file)
  lon.dim <- nc$dim$lon
  lat.dim <- nc$dim$lat
  if (is.null(nlake_class)) {
    lake.dim <- nc$dim$lake_class
  } else {
    lake.dim <- ncdim_def(
      name = "lake_class", units = "class", vals = 1:nlake_class,
      longname = "Lake class"
    )
  }
  nc_close(nc = nc)

  # Create variables
  # -
  Nlake.var <- ncvar_def(
    name = "Nlake", units = "#",
    longname = "Number of grid-cell lake types",
    dim = list(lon.dim, lat.dim), missval = -1, prec = "integer", compression = 2
  )
  # -
  lake_veg_class.var <- ncvar_def(
    name = "lake_veg_class", units = "class", 
    longname = "Vegetation class belonging to lake",
    dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "integer", compression = 2
  )
  lake_id.var <- ncvar_def(
    name = "lake_id", units = "#", 
    longname = "Lake output ID (Note: should contain sequential values starting from 0)",
    dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "integer", compression = 2
  )
  # -
  source_id.var <- ncvar_def(
    name = "source_id", units = "#",
    longname = "Lake source ID (Hydrolake ID)",
    dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "integer", compression = 2
  )
  lake_elevation.var <- ncvar_def(
    name = "lake_elevation", units = "m",
    longname = "Lake elevation",
    dim = list(lon.dim, lat.dim, lake.dim), missval = -9999, prec = "double", compression = 2
  )
  wfrac.var <- ncvar_def(
    name = "wfrac", units = "fraction",
    longname = "Width of lake outlet, expressed as fraction of lake perimeter",
    dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "double", compression = 2
  )
  rpercent.var <- ncvar_def(
    name = "rpercent", units = "fraction",
    longname = "Fraction of wetland surface runoff that flows into lake",
    dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "double", compression = 2
  )
  mindepth.var <- ncvar_def(
    name = "mindepth", units = "m",
    longname = "Minimum allowable depth of liquid portion of lake",
    dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "double", compression = 2
  )
  basin_depth.var <- ncvar_def(
    name = "basin_depth", units = "m",
    longname = "Maximum allowable depth of liquid portion of lake",
    dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "double", compression = 2
  )
  depth_in.var <- ncvar_def(
    name = "depth_in", units = "m",
    longname = "Initial lake depth of liquid portion of lake (distance from surface to deepest point)",
    dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "double", compression = 2
  )
  basin_frac.var <- ncvar_def(
    name = "basin_area", units = "fraction",
    longname = "Maximum fractional coverage of lake basin",
    dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "double", compression = 2
  )
  numnod_layer.var <- ncvar_def(
    name = "numnod", units = "#",
    longname = "Maximum number of lake temperature nodes",
    dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "integer", compression = 2
  )
  numnod_profile.var <- ncvar_def(
    name = "numnod_profile", units = "#",
    longname = "Maximum number of lake profile nodes",
    dim = list(lon.dim, lat.dim, lake.dim), missval = -1, prec = "integer", compression = 2
  )
  
  # Add variables
  nc <- nc_open(filename = nc.file, write = TRUE)
  nc <- ncvar_add(nc, Nlake.var)
  nc <- ncvar_add(nc, lake_veg_class.var)
  nc <- ncvar_add(nc, lake_id.var)
  nc <- ncvar_add(nc, source_id.var)
  nc <- ncvar_add(nc, lake_elevation.var)
  nc <- ncvar_add(nc, wfrac.var)
  nc <- ncvar_add(nc, rpercent.var)
  nc <- ncvar_add(nc, mindepth.var)
  nc <- ncvar_add(nc, basin_depth.var)
  nc <- ncvar_add(nc, depth_in.var)
  nc <- ncvar_add(nc, basin_frac.var)
  nc <- ncvar_add(nc, numnod_layer.var)
  nc <- ncvar_add(nc, numnod_profile.var)
  nc_close(nc = nc)
}

addLakeDefaultDatum <- function(nc.file, nc.var,
                               value = NULL, datum = NULL, na.map = NULL,
                               start = NULL, count = NULL) {
  print(paste0("Adding data for ", nc.var$name))
  
  # Generate dimensions
  dims <- rep(1, nc.var$ndims)
  for (i in 1:nc.var$ndims) {
    if (is.null(count) || count[i] == -1) {
      dims[i] <- nc.var$dim[[i]]$len
    } else {
      dims[i] <- count[i]
    }
  }
  
  # Generate data
  if (!is.null(value)) {
    if (length(value) > 0) {
      datum <- array(data = NA, dim = dims)
      for (i in 1:length(value)) {
        if (length(dim(datum)) == 2) {
          datum[, ] <- value[i]
        } else if (length(dim(datum)) == 3) {
          datum[, , i] <- value[i]
        } else if (length(dim(datum)) == 4) {
          datum[, , , i] <- value[i]
        }
      }
    } else if (length(value) == 1) {
      datum <- array(data = value, dim = dims)
    }
  } else if (is.null(datum)) {
    stop("No value or datum given")
  }
  
  # Omit data outside of mask
  if (!is.null(na.map)) {
    if (max(dim(na.map) != dims[1:2])) {
      print(paste0("na.map dims: ", paste0(dim(na.map), collapse = ", ")))
      print(paste0("variable dims: ", paste0(dims, collapse = ", ")))
      stop("na.map dimensions are not equal to variable dimensions")
    }
    
    for (x in 1:dim(na.map)[1]) {
      for (y in 1:dim(na.map)[2]) {
        if (na.map[x, y]) {
          if (length(dim(datum)) == 2) {
            datum[x, y] <- NA
          } else if (length(dim(datum)) == 3) {
            datum[x, y, ] <- NA
          } else if (length(dim(datum)) == 4) {
            datum[x, y, , ] <- NA
          }
        }
      }
    }
  }
  
  # Put data
  if (is.null(start) || is.null(count)) {
    start <- rep(1, length(dims))
    count <- rep(-1, length(dims))
  }
  nc <- nc_open(filename = nc.file, write = TRUE)
  ncvar_put(nc = nc, varid = nc.var, vals = datum, start = start, count = count)
  nc_close(nc = nc)
  
  # Remove data
  if (is.null(value)) {
    rm(datum)
  }
}

addLakeDefaultData <- function(nc.file, xs, ys, na.map = NULL,
                               lake_veg_class = NULL,
                               lake_id = NULL,
                               source_id = NULL,
                               lake_elevation = NULL,
                               wfrac = NULL,
                               rpercent = NULL,
                               mindepth = NULL,
                               numnod_profile = NULL,
                               numnod = NULL,
                               depth_in = NULL,
                               basin_depth = NULL,
                               basin_area = NULL) {
  # Get dimensions
  nc <- nc_open(filename = nc.file)
  lon.dim <- nc$dim$lon
  lat.dim <- nc$dim$lat
  veg.dim <- nc$dim$veg_class
  lake.dim <- nc$dim$lake_class
  nc_close(nc = nc)
  
  Nlake.map = array(0, dim = c(lon.dim$len, lat.dim$len))
  lake_veg_class.map = array(0, dim = c(lon.dim$len, lat.dim$len, lake.dim$len))
  lake_id.map = array(0, dim = c(lon.dim$len, lat.dim$len, lake.dim$len))
  source_id.map = array(0, dim = c(lon.dim$len, lat.dim$len, lake.dim$len))
  lake_elevation.map = array(0, dim = c(lon.dim$len, lat.dim$len, lake.dim$len))
  wfrac.map = array(0, dim = c(lon.dim$len, lat.dim$len, lake.dim$len))
  rpercent.map = array(0, dim = c(lon.dim$len, lat.dim$len, lake.dim$len))
  mindepth.map = array(0, dim = c(lon.dim$len, lat.dim$len, lake.dim$len))
  numnod_profile.map = array(0, dim = c(lon.dim$len, lat.dim$len, lake.dim$len))
  numnod.map = array(0, dim = c(lon.dim$len, lat.dim$len, lake.dim$len))
  depth_in.map = array(0, dim = c(lon.dim$len, lat.dim$len, lake.dim$len))
  basin_depth.map = array(0, dim = c(lon.dim$len, lat.dim$len, lake.dim$len))
  basin_area.map = array(0, dim = c(lon.dim$len, lat.dim$len, lake.dim$len))
  
  # Check or create parameters
  id = 0
  for(i in 1:length(xs)) {
    x = xs[i]
    y = ys[i]
    
    if (is.null(lake_veg_class)) {
      lake_veg_class.c <- Nlake.map[x, y] + 1
    } else {
      lake_veg_class.c <- lake_veg_class[i]
    }
    if (is.null(lake_id)) {
      lake_id.c = id
    } else {
      lake_id.c <- lake_id[i]
    }
    if (is.null(source_id)) {
      source_id.c <- id
    } else {
      source_id.c <- source_id[i]
    }
    if (is.null(lake_elevation)) {
      lake_elevation.c <- 0
    } else {
      lake_elevation.c <- lake_elevation[i]
    }
    if (is.null(wfrac)) {
      wfrac.c <- 0.1
    } else {
      wfrac.c <- wfrac[i]
    }
    if (is.null(rpercent)) {
      rpercent.c <- 0.1
    } else {
      rpercent.c <- rpercent[i]
    }
    if (is.null(mindepth)) {
      mindepth.c <- 0
    } else {
      mindepth.c <- mindepth[i]
    }
    if (is.null(numnod_profile)) {
      numnod_profile.c <- 10
    } else {
      numnod_profile.c <- numnod_profile[i]
    }
    if (is.null(basin_depth)) {
      basin_depth.c <- 10
    } else {
      basin_depth.c <- basin_depth[i]
    }
    if (is.null(numnod)) {
      numnod.c <- basin_depth.c * 2
    } else {
      numnod.c <- numnod[i]
    }
    if (is.null(depth_in)) {
      depth_in.c <- basin_depth.c
    } else {
      depth_in.c <- depth_in[i]
    }
    if (is.null(basin_area)) {
      basin_area.c <- 10000
    } else {
      basin_area.c <- basin_area[i]
    }
    
    Nlake.map[x,y] = Nlake.map[x,y] + 1
    id = id + 1
    
    if(Nlake.map[x,y] > lake.dim$len) {
      stop(paste0("Number of lakes [", Nlake.map[x,y], "] 
                  is larger than lake_class [", lake.dim$len, "]"))
    }
    
    lake_veg_class.map[x,y,Nlake.map[x,y]] = lake_veg_class.c
    lake_id.map[x,y,Nlake.map[x,y]] = lake_id.c
    source_id.map[x,y,Nlake.map[x,y]] = source_id.c
    lake_elevation.map[x,y,Nlake.map[x,y]] = lake_elevation.c
    wfrac.map[x,y,Nlake.map[x,y]] = wfrac.c
    rpercent.map[x,y,Nlake.map[x,y]] = rpercent.c
    mindepth.map[x,y,Nlake.map[x,y]] = mindepth.c
    numnod_profile.map[x,y,Nlake.map[x,y]] = numnod_profile.c
    numnod.map[x,y,Nlake.map[x,y]] = numnod.c
    basin_depth.map[x,y,Nlake.map[x,y]] = basin_depth.c
    depth_in.map[x,y,Nlake.map[x,y]] = depth_in.c
    basin_area.map[x,y,Nlake.map[x,y]] = basin_area.c
  }
  
  # Check Nlake.map
  if (max(Nlake.map, na.rm = T) != lake.dim$len) {
    stop(paste0("Maximum number of lakes [", Nlake.map, "] 
                  is not equal to lake_class [", lake.dim$len, "]"))
  }
  
  # Check or create na.map
  if (!is.null(na.map)) {
    if (max(dim(na.map) != c(lon.dim$len, lat.dim$len))) {
      print(paste0("na.map dims: ", paste0(dim(na.map), collapse = ", ")))
      print(paste0("File dims: ", paste0(c(lon.dim$len, lat.dim$len), collapse = ", ")))
      stop("na.map dimensions are not equal to file dimensions")
    }
  } else {
    na.map = Nlake.map == 0
  }
  
  # Add data
  addLakeDefaultDatum(nc.file = nc.file, nc.var = nc$var$Nlake, datum = Nlake.map, na.map = na.map)
  # -
  addLakeDefaultDatum(nc.file = nc.file, nc.var = nc$var$lake_veg_class, datum = lake_veg_class.map, na.map = na.map)
  addLakeDefaultDatum(nc.file = nc.file, nc.var = nc$var$lake_id, datum = lake_id.map, na.map = na.map)
  # -
  addLakeDefaultDatum(nc.file = nc.file, nc.var = nc$var$source_id, datum = source_id.map, na.map = na.map)
  addLakeDefaultDatum(nc.file = nc.file, nc.var = nc$var$lake_elevation, datum = lake_elevation.map, na.map = na.map)
  addLakeDefaultDatum(nc.file = nc.file, nc.var = nc$var$wfrac, datum = wfrac.map, na.map = na.map)
  addLakeDefaultDatum(nc.file = nc.file, nc.var = nc$var$rpercent, datum = rpercent.map, na.map = na.map)
  addLakeDefaultDatum(nc.file = nc.file, nc.var = nc$var$mindepth, datum = mindepth.map, na.map = na.map)
  addLakeDefaultDatum(nc.file = nc.file, nc.var = nc$var$numnod_profile, datum = numnod_profile.map, na.map = na.map)
  addLakeDefaultDatum(nc.file = nc.file, nc.var = nc$var$numnod, datum = numnod.map, na.map = na.map)
  addLakeDefaultDatum(nc.file = nc.file, nc.var = nc$var$depth_in, datum = depth_in.map, na.map = na.map)
  addLakeDefaultDatum(nc.file = nc.file, nc.var = nc$var$basin_depth, datum = basin_depth.map, na.map = na.map)
  addLakeDefaultDatum(nc.file = nc.file, nc.var = nc$var$basin_area, datum = basin_area.map, na.map = na.map)
}

fillSoilDefaultData <- function(nc.file, na.map, map.script) {
  soil.vars = c("AreaFract", "Ds", "Dsmax", "Ksat", "Pfactor",
                "Wcr_FRACT", "Wpwp_FRACT", "Ws", "annual_prec",
                "avg_T", "bubble", "bulk_density", "c", "cellnum",
                "depth", "dp", "elev", "elevation", "expt", "fs_active",
                "gridcell", "infilt", "init_moist", "mask", "off_gmt",
                "phi_s", "quartz", "resid_moist", "rough", "run_cell", 
                "snow_rough", "soil_density", "root_depth", "root_fract")
  
  source(map.script)
  
  for(var in soil.vars) {
    print(paste0("Filling data for ", var))
    nc = nc_open(nc.file)
    data = ncvar_get(nc = nc, varid = var)
    nc_close(nc)
    
    data.filled = fillMap(data, na.map, nearest.function = getNearestMean)
    
    nc = nc_open(nc.file, write = T)
    ncvar_put(nc = nc, varid = var, vals = data.filled)
    nc_close(nc)
  }
}
