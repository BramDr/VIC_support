library(ncdf4)
library(here)

source("/home/bram/VIC_support/Scripts/Support/generateSoilFunctions.R")
source("/home/bram/VIC_support/Scripts/Support/generateElevationFunctions.R")

misc.vars <- c(
  "annual_prec", "avg_T", "run_cell"
)
soil.vars <- c(
  "Ksat", "Wcr_FRACT", "Wpwp_FRACT", "bubble", "bulk_density",
  "expt", "init_moist", "phi_s", "quartz", "resid_moist",
  "soil_density", "rough", "snow_rough", "dp", "fs_active", "depth"
)
veg.vars <- c(
  "Nveg", "Cv", "wind_atten",
  "wind_h", "rmin", "rarc", "rad_atten", "RGL",
  "trunk_ratio", "overstory", "root_fract", "root_depth",
  "LAI", "displacement", "veg_rough", "albedo"
)
calib.vars <- c(
  "Ds", "Dsmax", "Ws", "c", "infilt"
)
elev.vars <- c(
  "AreaFract", "Pfactor", "elev", "elevation"
)
unused.vars <- c(
  "off_gmt", "cellnum", "gridcell", "mask", "lats", "lons", "veg_descr"
)

unused.atts <- c(
  "source", "username", "host", "nco_openmp_thread_number",
  "NCO", "history_of_appended_files", "history"
)

addVegVars <- function(nc.file, nveg_class = NULL) {
  # Get dimensions
  nc <- nc_open(filename = nc.file)
  lon.dim <- nc$dim$lon
  lat.dim <- nc$dim$lat
  if (is.null(nveg_class)) {
    veg.dim <- nc$dim$veg_class
  } else {
    veg.dim <- ncdim_def(
      name = "veg_class", units = "class", vals = 1:nveg_class,
      longname = "Vegetation class"
    )
  }
  root.dim <- nc$dim$root_zone
  month.dim <- nc$dim$month
  nc_close(nc = nc)

  # Create variables
  # -
  Nveg.var <- ncvar_def(
    name = "Nveg", units = "#",
    longname = "Number of grid-cell vegetation types",
    dim = list(lon.dim, lat.dim), missval = -1, prec = "integer", compression = 1
  )
  # -
  Cv.var <- ncvar_def(
    name = "Cv", units = "-",
    longname = "Fraction of grid-cell vegetation cover",
    dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 1
  )
  wind_atten.var <- ncvar_def(
    name = "wind_atten", units = "-",
    longname = "Wind speed attenuation through the overstory. The default value has been 0.5",
    dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 1
  )
  wind_h.var <- ncvar_def(
    name = "wind_h", units = "m",
    longname = "Height at which wind speed is measured",
    dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 1
  )
  rmin.var <- ncvar_def(
    name = "rmin", units = "s m-1",
    longname = "Vegetation minimum stomatal resistance",
    dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 1
  )
  rarc.var <- ncvar_def(
    name = "rarc", units = "s m-1",
    longname = "Vegetation architectural resistance",
    dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 1
  )
  rad_atten.var <- ncvar_def(
    name = "rad_atten", units = "-",
    longname = "Radiation attenuation factor. Normally set to 0.5, though may need to be adjusted for high latitudes",
    dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 1
  )
  RGL.var <- ncvar_def(
    name = "RGL", units = "W m-2",
    longname = "Minimum incoming shortwave radiation afor transpiration",
    dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 1
  )
  trunk_ratio.var <- ncvar_def(
    name = "trunk_ratio", units = "-",
    longname = "Ratio of total tree height that is trunk (no branches). The default value has been 0.2",
    dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 1
  )
  overstory.var <- ncvar_def(
    name = "overstory", units = "flag",
    longname = "Vegetation overstory: 1 - present [e.g. trees], 2 - not present [e.g. grass]",
    dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "integer", compression = 1
  )
  # -
  root_fract.var <- ncvar_def(
    name = "root_fract", units = "-",
    longname = "Vegetation root fraction",
    dim = list(lon.dim, lat.dim, root.dim, veg.dim), missval = -1, prec = "double", compression = 1
  )
  root_depth.var <- ncvar_def(
    name = "root_depth", units = "m",
    longname = "Vegetation root depth",
    dim = list(lon.dim, lat.dim, root.dim, veg.dim), missval = -1, prec = "double", compression = 1
  )
  # -
  LAI.var <- ncvar_def(
    name = "LAI", units = "m2 m-2",
    longname = "Vegetation leaf area index",
    dim = list(lon.dim, lat.dim, month.dim, veg.dim), missval = -1, prec = "double", compression = 1
  )
  displacement.var <- ncvar_def(
    name = "displacement", units = "m",
    longname = "Vegetation displacement height (typically 0.67 * vegetation height)",
    dim = list(lon.dim, lat.dim, month.dim, veg.dim), missval = -1, prec = "double", compression = 1
  )
  veg_rough.var <- ncvar_def(
    name = "veg_rough", units = "m",
    longname = "Vegetation roughness length (typically 0.123 * vegetation height)",
    dim = list(lon.dim, lat.dim, month.dim, veg.dim), missval = -1, prec = "double", compression = 1
  )
  albedo.var <- ncvar_def(
    name = "albedo", units = "-",
    longname = "Vegetation shortwave albedo",
    dim = list(lon.dim, lat.dim, month.dim, veg.dim), missval = -1, prec = "double", compression = 1
  )
  fcanopy.var <- ncvar_def(
    name = "fcanopy", units = "-",
    longname = "Vegetation canopy cover",
    dim = list(lon.dim, lat.dim, month.dim, veg.dim), missval = -1, prec = "double", compression = 1
  )

  # Add variables
  nc <- nc_open(filename = nc.file, write = TRUE)
  nc <- ncvar_add(nc, Nveg.var)
  nc <- ncvar_add(nc, Cv.var)
  nc <- ncvar_add(nc, wind_atten.var)
  nc <- ncvar_add(nc, wind_h.var)
  nc <- ncvar_add(nc, rmin.var)
  nc <- ncvar_add(nc, rarc.var)
  nc <- ncvar_add(nc, rad_atten.var)
  nc <- ncvar_add(nc, RGL.var)
  nc <- ncvar_add(nc, trunk_ratio.var)
  nc <- ncvar_add(nc, overstory.var)
  nc <- ncvar_add(nc, root_fract.var)
  nc <- ncvar_add(nc, root_depth.var)
  nc <- ncvar_add(nc, LAI.var)
  nc <- ncvar_add(nc, displacement.var)
  nc <- ncvar_add(nc, veg_rough.var)
  nc <- ncvar_add(nc, albedo.var)
  nc <- ncvar_add(nc, fcanopy.var)
  nc_close(nc = nc)
}

addCo2Vars <- function(nc.file, nveg_class = NULL) {
  # Get dimensions
  nc <- nc_open(filename = nc.file)
  lon.dim <- nc$dim$lon
  lat.dim <- nc$dim$lat
  if (is.null(nveg_class)) {
    veg.dim <- nc$dim$veg_class
  } else {
    veg.dim <- ncdim_def(
      name = "veg_class", units = "class", vals = 1:nveg_class,
      longname = "Vegetation class"
    )
  }
  nc_close(nc = nc)

  # Create variables
  # -
  b.co2.var <- ncvar_def(
    name = "b_co2", units = "fraction",
    longname = "CO2 transpiration parameter",
    dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 1
  )

  # Add variables
  nc <- nc_open(filename = nc.file, write = TRUE)
  nc <- ncvar_add(nc, b.co2.var)
  nc_close(nc = nc)
}

addVegDefaultDatum <- function(nc.file, nc.var,
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

addVegDefaultData <- function(nc.file, Cv = NULL, na.map = NULL,
                              wind_atten = NULL,
                              rad_atten = NULL,
                              rmin = NULL,
                              rarc = NULL,
                              RGL = NULL,
                              overstory = NULL,
                              trunk_ratio = NULL,
                              wind_h = NULL,
                              root_fract = NULL,
                              root_depth = NULL,
                              LAI = NULL,
                              height = NULL,
                              albedo = NULL,
                              fcanopy = NULL) {
  # Get dimensions
  nc <- nc_open(filename = nc.file)
  lon.dim <- nc$dim$lon
  lat.dim <- nc$dim$lat
  veg.dim <- nc$dim$veg_class
  root.dim <- nc$dim$root_zone
  month.dim <- nc$dim$month
  nc_close(nc = nc)

  # Check or create parameters
  if (is.null(wind_atten)) {
    wind_atten <- c(rep(0.5, veg.dim$len - 1), 0)
  }
  if (is.null(rad_atten)) {
    rad_atten <- c(rep(0.5, veg.dim$len - 1), 0)
  }
  if (is.null(rmin)) {
    rmin <- c(rep(80, veg.dim$len - 1), 0)
  }
  if (is.null(rarc)) {
    rarc <- c(rep(2, veg.dim$len - 1), 0)
  }
  if (is.null(RGL)) {
    RGL <- c(rep(100, veg.dim$len - 1), 0)
  }
  if (is.null(overstory)) {
    overstory <- c(rep(0, veg.dim$len - 1), 0)
  }
  if (is.null(trunk_ratio)) {
    trunk_ratio <- c(rep(0.2, veg.dim$len - 1), 0)
  }
  if (is.null(wind_h)) {
    wind_h <- rep(2, veg.dim$len)
  }
  # -
  if (is.null(root_fract)) {
    root_fract <- matrix(c(rep(0.5, veg.dim$len - 1), 0), nrow = veg.dim$len, ncol = 2)
    root_fract <- cbind(root_fract, rep(0, veg.dim$len))
  }
  if (is.null(root_depth)) {
    root_depth <- matrix(c(rep(0.3, veg.dim$len - 1), 0), nrow = veg.dim$len, ncol = 1)
    root_depth <- cbind(root_depth, c(rep(0.7, veg.dim$len - 1), 0))
    root_depth <- cbind(root_depth, rep(0, veg.dim$len))
  }
  # -
  if (is.null(LAI)) {
    LAI <- c(rep(3, veg.dim$len - 1), 0)
    for (i in 2:12) {
      LAI <- cbind(LAI, c(rep(3, veg.dim$len - 1), 0))
    }
  }
  if (is.null(height)) {
    height <- c(rep(1, veg.dim$len - 1), 0)
    for (i in 2:12) {
      height <- cbind(height, c(rep(1, veg.dim$len - 1), 0))
    }
  }
  if (is.null(albedo)) {
    albedo <- c(rep(0.1, veg.dim$len - 1), 0.2)
    for (i in 2:12) {
      albedo <- cbind(albedo, c(rep(0.1, veg.dim$len - 1), 0.2))
    }
  }
  if (is.null(fcanopy)) {
    fcanopy <- c(rep(1, veg.dim$len - 1), 0.0001)
    for (i in 2:12) {
      fcanopy <- cbind(fcanopy, c(rep(1, veg.dim$len - 1), 0.0001))
    }
  }

  # Check or create Cv
  if (is.null(Cv)) {
    Cv <- array(data = 1 / veg.dim$len, dim = c(lon.dim$len, lat.dim$len, veg.dim$len))
  }
  if (max(dim(Cv) != c(lon.dim$len, lat.dim$len, veg.dim$len))) {
    print(paste0("Cv dims: ", paste0(dim(Cv), collapse = ", ")))
    print(paste0("File dims: ", paste0(c(lon.dim$len, lat.dim$len, veg.dim$len), collapse = ", ")))
    stop("Cv dimensions are not equal to file dimensions")
  }

  # Check or create na.map
  if (!is.null(na.map)) {
    if (max(dim(na.map) != c(lon.dim$len, lat.dim$len))) {
      print(paste0("na.map dims: ", paste0(dim(na.map), collapse = ", ")))
      print(paste0("File dims: ", paste0(c(lon.dim$len, lat.dim$len), collapse = ", ")))
      stop("na.map dimensions are not equal to file dimensions")
    }

    for (x in 1:dim(na.map)[1]) {
      for (y in 1:dim(na.map)[2]) {
        if (na.map[x, y]) {
          Cv[x, y, ] <- NA
        } else if (is.na(Cv[x, y, 1])) {
          Cv[x, y, ] <- 0
        }
      }
    }
  }
  na.map <- is.na(Cv[, , 1])
  dim(na.map) <- dim(Cv)[1:2]

  # Create Nveg
  Nveg <- apply(X = Cv, MARGIN = c(1, 2), FUN = function(x) {
    sum(x[1:(length(x) - 1)] > 0, na.rm = T)
  })
  # Add data
  addVegDefaultDatum(nc.file = nc.file, nc.var = nc$var$Nveg, datum = Nveg, na.map = na.map)
  rm(Nveg)
  addVegDefaultDatum(nc.file = nc.file, nc.var = nc$var$Cv, datum = Cv, na.map = na.map)
  rm(Cv)
  # -
  addVegDefaultDatum(nc.file = nc.file, nc.var = nc$var$wind_atten, value = wind_atten, na.map = na.map)
  addVegDefaultDatum(nc.file = nc.file, nc.var = nc$var$rad_atten, value = rad_atten, na.map = na.map)
  addVegDefaultDatum(nc.file = nc.file, nc.var = nc$var$rmin, value = rmin, na.map = na.map)
  addVegDefaultDatum(nc.file = nc.file, nc.var = nc$var$rarc, value = rarc, na.map = na.map)
  addVegDefaultDatum(nc.file = nc.file, nc.var = nc$var$RGL, value = RGL, na.map = na.map)
  addVegDefaultDatum(nc.file = nc.file, nc.var = nc$var$overstory, value = overstory, na.map = na.map)
  addVegDefaultDatum(nc.file = nc.file, nc.var = nc$var$trunk_ratio, value = trunk_ratio, na.map = na.map)
  addVegDefaultDatum(nc.file = nc.file, nc.var = nc$var$wind_h, value = wind_h, na.map = na.map)
  # -
  for (i in 1:nc$dim$root_zone$len) {
    addVegDefaultDatum(nc.file = nc.file, nc.var = nc$var$root_fract, value = root_fract[, i], na.map = na.map, start = c(1, 1, i, 1), count = c(-1, -1, 1, -1))
    addVegDefaultDatum(nc.file = nc.file, nc.var = nc$var$root_depth, value = root_depth[, i], na.map = na.map, start = c(1, 1, i, 1), count = c(-1, -1, 1, -1))
  }
  # -
  for (i in 1:nc$dim$month$len) {
    addVegDefaultDatum(nc.file = nc.file, nc.var = nc$var$LAI, value = LAI[, i], na.map = na.map, start = c(1, 1, i, 1), count = c(-1, -1, 1, -1))
    addVegDefaultDatum(nc.file = nc.file, nc.var = nc$var$displacement, value = height[, i] * 0.67, na.map = na.map, start = c(1, 1, i, 1), count = c(-1, -1, 1, -1))
    addVegDefaultDatum(nc.file = nc.file, nc.var = nc$var$veg_rough, value = height[, i] * 0.123, na.map = na.map, start = c(1, 1, i, 1), count = c(-1, -1, 1, -1))
    addVegDefaultDatum(nc.file = nc.file, nc.var = nc$var$albedo, value = albedo[, i], na.map = na.map, start = c(1, 1, i, 1), count = c(-1, -1, 1, -1))
    if("fcanopy" %in% names(nc$var)){
      addVegDefaultDatum(nc.file = nc.file, nc.var = nc$var$fcanopy, value = fcanopy[, i], na.map = na.map, start = c(1, 1, i, 1), count = c(-1, -1, 1, -1))
    }
  }
}

cropyCreateVariable = function(base.file = NULL, base.name = NULL, base.variable = NULL,
                               name = NULL, units = NULL, 
                               dim = NULL, missval = NULL, longname = NULL, 
                               prec = NULL, shuffle = NULL, compression = NULL, 
                               chunksizes = NULL){

  if((is.null(base.file) || is.null(base.name)) && is.null(base.variable)){
    stop("Need more data")
    return(NULL)
  }
  
  if(is.null(base.variable)){
    nc = nc_open(base.file)
    base.variable = nc$var[[base.name]]
    nc_close(nc)
  }
  
  if(is.null(name)){
    name = base.variable$name
  }
  if(is.null(units)){
    units = base.variable$units
  }
  if(is.null(dim)){
    dim = base.variable$dim
  }
  if(is.null(missval)){
    missval = base.variable$missval
  }
  if(is.null(longname)){
    longname = base.variable$longname
  }
  if(is.null(prec)){
    prec = base.variable$prec
    if(prec == "int"){
      prec = "integer"
    }
  }
  if(is.null(shuffle)){
    shuffle = base.variable$shuffle
  }
  if(is.null(compression)){
    compression = base.variable$compression
  }
  if(is.null(chunksizes)){
    chunksizes = base.variable$chunksizes
  }
  new.variable = ncvar_def(name = name,
                            units = units, 
                            dim = dim, 
                            missval = missval, 
                            longname = longname, 
                            prec = prec, 
                            shuffle = shuffle, 
                            compression = compression, 
                            chunksizes = chunksizes)
  return(new.variable)
}
