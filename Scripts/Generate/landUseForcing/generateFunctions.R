
addVegVars = function(veg.file, lon.dim, lat.dim, veg.dim, root.dim, month.dim){
  # Var
  # -
  Nveg.var = ncvar_def(name = "Nveg", units = "#", 
                       longname = "Number of grid-cell vegetation types", 
                       dim = list(lon.dim, lat.dim), missval = -1, prec = "integer", compression = 9)
  # -
  Cv.var = ncvar_def(name = "Cv", units = "-", 
                     longname = "Fraction of grid-cell vegetation cover", 
                     dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 9)
  wind_atten.var = ncvar_def(name = "wind_atten", units = "-", 
                             longname = "Wind speed attenuation through the overstory. The default value has been 0.5", 
                             dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 9)
  wind_h.var = ncvar_def(name = "wind_h", units = "m", 
                         longname = "Height at which wind speed is measured",
                         dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 9)
  rmin.var = ncvar_def(name = "rmin", units = "s m-1", 
                       longname = "Vegetation minimum stomatal resistance", 
                       dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 9)
  rarc.var = ncvar_def(name = "rarc", units = "s m-1", 
                       longname = "Vegetation architectural resistance",
                       dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 9)
  rad_atten.var = ncvar_def(name = "rad_atten", units = "-",
                            longname = "Radiation attenuation factor. Normally set to 0.5, though may need to be adjusted for high latitudes", 
                            dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 9)
  RGL.var = ncvar_def(name = "RGL", units = "W m-2",
                      longname = "Minimum incoming shortwave radiation afor transpiration", 
                      dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 9)
  trunk_ratio.var = ncvar_def(name = "trunk_ratio", units = "-", 
                              longname = "Ratio of total tree height that is trunk (no branches). The default value has been 0.2", 
                              dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "double", compression = 9)
  overstory.var = ncvar_def(name = "overstory", units = "flag",
                            longname = "Vegetation overstory: 1 - present [e.g. trees], 2 - not present [e.g. grass]", 
                            dim = list(lon.dim, lat.dim, veg.dim), missval = -1, prec = "integer", compression = 9)
  # -
  root_fract.var = ncvar_def(name = "root_fract", units = "-", 
                             longname = "Vegetation root fraction", 
                             dim = list(lon.dim, lat.dim, root.dim, veg.dim), missval = -1, prec = "double", compression = 9)
  root_depth.var = ncvar_def(name = "root_depth", units = "m", 
                             longname = "Vegetation root depth", 
                             dim = list(lon.dim, lat.dim, root.dim, veg.dim), missval = -1, prec = "double", compression = 9)
  # -
  LAI.var = ncvar_def(name = "LAI", units = "m2 m-2", 
                      longname = "Vegetation leaf area index", 
                      dim = list(lon.dim, lat.dim, month.dim, veg.dim), missval = -1, prec = "double", compression = 9)
  displacement.var = ncvar_def(name = "displacement", units = "m", 
                               longname = "Vegetation displacement height (typically 0.67 * vegetation height)", 
                               dim = list(lon.dim, lat.dim, month.dim, veg.dim), missval = -1, prec = "double", compression = 9)
  veg_rough.var = ncvar_def(name = "veg_rough", units = "m", 
                            longname = "Vegetation roughness length (typically 0.123 * vegetation height)", 
                            dim = list(lon.dim, lat.dim, month.dim, veg.dim), missval = -1, prec = "double", compression = 9)
  albedo.var = ncvar_def(name = "albedo", units = "-", 
                         longname = "Vegetation shortwave albedo", 
                         dim = list(lon.dim, lat.dim, month.dim, veg.dim), missval = -1, prec = "double", compression = 9)
  
  # Add
  nc = nc_open(filename = veg.file, write = TRUE)
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

