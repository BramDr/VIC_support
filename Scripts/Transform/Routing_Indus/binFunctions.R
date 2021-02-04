
loadHeader = function(file){
  conn = file(file,"rb")
  name = readChar(conn, nchar=7)
  version = readBin(conn, integer(), n=1, size=4)
  order = readBin(conn, integer(), n=1, size=4)
  firstyear = readBin(conn, integer(), n=1, size=4)
  nyears = readBin(conn, integer(), n=1, size=4)
  firstcell = readBin(conn, integer(), n=1, size=4)   
  ncells = readBin(conn, integer(), n=1, size=4)
  nbands = readBin(conn, integer(), n=1, size=4)
  cellsize = readBin(conn, numeric(), n=1, size=4)
  scalar = readBin(conn, numeric(), n=1, size=4)
  close(conn)
  
  header = data.frame(name, version, order, firstyear, nyears, firstcell, ncells, nbands, cellsize, scalar)
  return(header)
}

loadBin = function(file, type, size, firstyear = NULL, nyears = NULL, ncells = NULL, nbands = NULL, scalar = NULL){
  if(is.null(firstyear) || is.null(nyears) || is.null(ncells) || is.null(nbands)){
    header = loadHeader(file)
    firstyear = header$firstyear
    nyears = header$nyears
    ncells = header$ncells
    nbands = header$nbands
    scalar = header$scalar
  }
  
  conn = file(file, "rb")
  if(exists("header")){
    seek(conn, 43, origin = "start")
  }
  data = readBin(con = conn, what = type, n = nyears * ncells * nbands, size = size) * scalar
  close(conn)
  
  years = (firstyear - 1) + 1:nyears
  matrix = matrix(data, ncol = nbands, byrow = T)
  
  output = list("matrix" = matrix, "years" = rep(years, each = ncells))
  if(exists("header")){
    output[["header"]] = header
  }
  return(output)
}

mapBin = function(grid, data){
  years = unique(data$years)
  nbands = ncol(data$matrix)

  lons = grid$matrix[,1]
  lats = grid$matrix[,2]
    
  output = list()
  
  year = years[1]
  for(year in years){
    
    output[[as.character(year)]] = list()
    
    band = 1
    for(band in 1:nbands){
      
      data.band = data$matrix[,band]
      data.band = data.band[data$years == year]
      
      raster.data = data.frame(x = lons,
                               y = lats,
                               z = data.band)
      raster.extent <- extent(raster.data[,1:2])
      
      raster.points = SpatialPointsDataFrame(coords = raster.data[,c("x", "y")],
                                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"),
                                             data = raster.data[,"z", drop = F])
      
      raster.template = raster(points2grid(raster.points, tolerance = 0.1))
      raster.map = rasterize(raster.points, raster.template, field = "z", fun = mean)
      #plot(raster.map)
      
      output[[as.character(year)]][[band]] = raster.map
    }
  }
  
  return(output)
}
