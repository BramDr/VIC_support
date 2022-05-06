library(ncdf4)

loadSoilVars = function(dim.lon, dim.lat, dim.layer = NULL){
  if(is.null(dim.layer)){
    dim.layer = ncdim_def(name = "nlayer",
                          units = "#",
                          vals = 1:3,
                          longname = "Soil layer")
  }
  
  Nlayer.var <- ncvar_def(name = "Nlayer", 
                      units = "#", 
                      dim = list(dim.lon, dim.lat), 
                      longname = "Number of active soil layers", 
                      prec = "integer", missval = -1)
  quartz.var <- ncvar_def(name = "quartz", 
                        units = "fraction", 
                        dim = list(dim.lon, dim.lat, dim.layer), 
                        longname = "Soil quartz content", 
                        prec = "double", missval = -1)
  expt.var <- ncvar_def(name = "expt", 
                        units = "N/A", 
                        dim = list(dim.lon, dim.lat, dim.layer), 
                        longname = "Exponent n (=3+2/lambda) in Campbells eqn for hydraulic conductivity, HBH 5.6 (where lambda = soil pore size distribution parameter).  Values should be > 3.0.", 
                        prec = "double", missval = -1)
  Ksat.var <- ncvar_def(name = "Ksat", 
                        units = "mm day-1", 
                        dim = list(dim.lon, dim.lat, dim.layer), 
                        longname = "Saturated hydrologic conductivity", 
                        prec = "double", missval = -1)
  bulk_density.var <- ncvar_def(name = "bulk_density", 
                      units = "kg m-3", 
                      dim = list(dim.lon, dim.lat, dim.layer), 
                      longname = "Soil bulk density", 
                      prec = "double", missval = -1)
  Wfc.var <- ncvar_def(name = "Wfc_FRACT", 
                      units = "fraction of maximum moisture", 
                      dim = list(dim.lon, dim.lat, dim.layer), 
                      longname = "Soil moisture content at field capacity", 
                      prec = "double", missval = -1)
  Wcr.var <- ncvar_def(name = "Wcr_FRACT", 
                      units = "fraction of maximum moisture", 
                      dim = list(dim.lon, dim.lat, dim.layer), 
                      longname = "Soil moisture content at stress point", 
                      prec = "double", missval = -1)
  Wpwp.var <- ncvar_def(name = "Wpwp_FRACT", 
                      units = "fraction", 
                      dim = list(dim.lon, dim.lat, dim.layer), 
                      longname = "Soil moisture content at wilting point", 
                      prec = "double", missval = -1)
  bubble.var <- ncvar_def(name = "bubble", 
                              units = "cm", 
                              dim = list(dim.lon, dim.lat, dim.layer), 
                              longname = "Soil bubbling pressure", 
                              prec = "double", missval = -1)
  soil_density.var <- ncvar_def(name = "soil_density", 
                              units = "kg m-3", 
                              dim = list(dim.lon, dim.lat, dim.layer), 
                              longname = "Soil particle density", 
                              prec = "double", missval = -1)
  resid_moist.var <- ncvar_def(name = "resid_moist", 
                              units = "fraction of soil depth", 
                              dim = list(dim.lon, dim.lat, dim.layer), 
                              longname = "Residual soil moisture", 
                              prec = "double", missval = -1)
  init_moist.var <- ncvar_def(name = "init_moist", 
                              units = "mm", 
                              dim = list(dim.lon, dim.lat, dim.layer), 
                              longname = "Initial soil moisture", 
                              prec = "double", missval = -1)
  depth.var <- ncvar_def(name = "depth", 
                              units = "m", 
                              dim = list(dim.lon, dim.lat, dim.layer), 
                              longname = "Soil layer depth", 
                              prec = "double", missval = -1)
  fs_active.var <- ncvar_def(name = "fs_active", 
                            units = "flag", 
                            dim = list(dim.lon, dim.lat), 
                            longname = "If set to 1, then frozen soil algorithm is activated for the grid cell. A 0 indicates that frozen soils are not computed even if soil temperatures fall below 0C.", 
                            prec = "integer", missval = -1)
  phi_s.var <- ncvar_def(name = "phi_s", 
                            units = "mm mm-1", 
                            dim = list(dim.lon, dim.lat, dim.layer), 
                            longname = "Soil moisture diffusion parameter", 
                            prec = "double", missval = -1)
  rough.var <- ncvar_def(name = "rough", 
                            units = "m", 
                            dim = list(dim.lon, dim.lat), 
                            longname = "Soil surface roughness", 
                            prec = "double", missval = -1)
  snow_rough.var <- ncvar_def(name = "snow_rough", 
                            units = "m", 
                            dim = list(dim.lon, dim.lat), 
                            longname = "Snow surface roughness", 
                            prec = "double", missval = -1)
  dp.var <- ncvar_def(name = "dp", 
                            units = "m", 
                            dim = list(dim.lon, dim.lat), 
                            longname = "Soil thermal damping depth", 
                            prec = "double", missval = -1)
                            
  soil.vars = list(Nlayer = Nlayer.var,
                  quartz = quartz.var,
                  expt = expt.var,
                  Ksat = Ksat.var,
                  bulk_density = bulk_density.var,
                  Wfc = Wfc.var,
                  Wcr = Wcr.var,
                  Wpwp = Wpwp.var,
                  bubble = bubble.var,
                  soil_density = soil_density.var,
                  resid_moist = resid_moist.var,
                  init_moist = init_moist.var,
                  depth = depth.var,
                  fs_active = fs_active.var,
                  phi_s = phi_s.var,
                  rough = rough.var,
                  snow_rough = snow_rough.var,
                  dp = dp.var)
  return(soil.vars)
}
