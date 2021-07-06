library(raster)

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

loadBin = function(file, type, size = NA_integer_, 
                   header = NULL, firstyear = NULL, nyears = NULL, 
                   ncells = NULL, nbands = NULL, scalar = NULL, 
                   colnames = NULL){
  if(is.null(header)){
    header = loadHeader(file)
  }
  if(is.null(firstyear) || is.null(nyears) || is.null(ncells) || is.null(nbands)){
    firstyear = header$firstyear
    nyears = header$nyears
    ncells = header$ncells
    nbands = header$nbands
    scalar = header$scalar
  }
  if(is.null(colnames)){
    colnames = 1:nbands
  }
  
  conn = file(file, "rb")
  if(exists("header")){
    seek(conn, 43, origin = "start")
  }
  data = readBin(con = conn, what = type, n = nyears * ncells * nbands, size = size) * scalar
  close(conn)
  
  years = (firstyear - 1) + 1:nyears
  data.matrix = matrix(data, nrow = nyears * ncells, ncol = nbands, byrow = T)
  data.df = as.data.frame(data.matrix)
  colnames(data.df) = colnames
  
  output = list("data" = data.df, "years" = rep(years, each = ncells))
  if(exists("header")){
    output[["header"]] = header
  }
  return(output)
}

mapBin = function(grid, data){
  years = unique(data$years)
  nbands = ncol(data$data)

  lons = grid$data[,1]
  lats = grid$data[,2]
  
  resolution = 1 / 12
  lons.seq = seq(from = min(lons), to = max(lons), by = resolution)
  lons.seq = round(lons.seq, digits = 2)
  lats.seq = seq(from = min(lats), to = max(lats), by = resolution)
  lats.seq = round(lats.seq, digits = 2)
  
  x.seq = rep(NA, length(lons))
  y.seq = rep(NA, length(lats))
  for(i in 1:length(lons)){
    x.seq[i] = which.min(abs(lons.seq - lons[i]))
  }
  for(i in 1:length(lats)){
    y.seq[i] = which.min(abs(lats.seq - lats[i]))
  }
    
  output = list()
  
  year = years[1]
  for(year in years){
    
    output[[as.character(year)]] = list()
    
    band = 1
    for(band in 1:nbands){
      band.name = colnames(data$data)[band]
      
      data.band = data$data[,band]
      data.band = data.band[data$years == year]
      
      matrix.data = matrix(nrow = length(lons.seq), ncol = length(lats.seq))
      for(i in 1:length(data.band)){
        matrix.data[x.seq[i], y.seq[i]] = data.band[i]
      }
      
      output[[as.character(year)]][[band.name]] = matrix.data
    }
  }
  
  return(output)
}
