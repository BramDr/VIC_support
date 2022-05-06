library(ncdf4)

loadElevationVars = function(dim.lon, dim.lat, dim.band = NULL){
  if(is.null(dim.band)){
    dim.band = ncdim_def(name = "snow_band",
                         units = "#",
                         vals = 0,
                         longname = "Elevation band")
  }

  Nelev.var <- ncvar_def(name = "Nelev", 
                      units = "#", 
                      dim = list(dim.lon, dim.lat), 
                      longname = "Number of active elevation bands", 
                      prec = "integer", missval = -1)
  elev.var <- ncvar_def(name = "elev", 
                        units = "m", 
                        dim = list(dim.lon, dim.lat), 
                        longname = "Elevation", 
                        prec = "double", missval = -1)
  elevation.var <- ncvar_def(name = "elevation", 
                        units = "m", 
                        dim = list(dim.lon, dim.lat, dim.band), 
                        longname = "Elevation band elevation", 
                        prec = "double", missval = -1)
  AreaFract.var <- ncvar_def(name = "AreaFract", 
                        units = "fraction", 
                        dim = list(dim.lon, dim.lat, dim.band), 
                        longname = "Elevation band area", 
                        prec = "double", missval = -1)
  Pfactor.var <- ncvar_def(name = "Pfactor", 
                        units = "fraction", 
                        dim = list(dim.lon, dim.lat, dim.band), 
                        longname = "Elevation band precipitation", 
                        prec = "double", missval = -1)
                            
  elevation.vars = list(Nelev = Nelev.var,
                  elev = elev.var,
                  elevation = elevation.var,
                  AreaFract = AreaFract.var,
                  Pfactor = Pfactor.var)
  return(elevation.vars)
}
