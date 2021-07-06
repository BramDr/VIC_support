library(fields)
library(ncdf4)
rm(list = ls())

# Input
ldam.file <- "Saves/localDamsMerge.csv"
gdam.file <- "Saves/globalDamsMerge.csv"
accumulation.file <- "../../../Data/Transformed/Routing/accumulation_5min_Indus.RDS"
downstream.file <- "../../../Data/Transformed/Routing/downstream_5min_Indus.RDS"
id.file = "./Saves/idMap.RDS"
wateruse.id.file = "../waterUse_Indus/Saves/idMap.RDS"
wateruse.1.file = "../waterUse_Indus/Saves/receivingCommand.RDS"
wateruse.2.file = "../waterUse_Indus/Saves/receivingDelta.RDS"
ldam.service.out <- "Saves/localDamsService.RDS"
gdam.service.out <- "Saves/globalDamsService.RDS"
ldam.service.fraction.out <- "Saves/localDamsServiceFraction.RDS"
gdam.service.fraction.out <- "Saves/globalDamsServiceFraction.RDS"

# Load
accumulation = readRDS(accumulation.file)
downstream <- readRDS(downstream.file)
ldam <- read.csv(ldam.file, stringsAsFactors = F)
gdam <- read.csv(gdam.file, stringsAsFactors = F)
id = readRDS(id.file)

wateruse.id = readRDS(wateruse.id.file)
wateruse.1 = readRDS(wateruse.1.file)
wateruse.2 = readRDS(wateruse.2.file)

# Calculate
Ndams = array(0, dim = dim(id))
for (i in order(gdam$CAP_MCM)) {
  print(i)
  
  x <- gdam$MODEL_X[i]
  y <- gdam$MODEL_Y[i]
  Ndams[x,y] = Ndams[x,y] + 1
}
image.plot(Ndams)

global.service.map = array(NA, dim = c(dim(id), nrow(gdam)))
local.service.map = array(NA, dim = c(dim(id), nrow(ldam)))
global.service.fraction.map = array(NA, dim = c(dim(id), nrow(gdam)))
local.service.fraction.map = array(NA, dim = c(dim(id), nrow(ldam)))
i = 31
for (i in order(gdam$CAP_MCM)) {
  print(i)
  
  x <- gdam$MODEL_X[i]
  y <- gdam$MODEL_Y[i]
  dam.id <- id[x,y]

  if (is.na(downstream[x, y, 1]) || !(gdam$USE_IRR[i] == 1)) {
    next
  }

  cur <- c(x, y)
  nex <- downstream[x, y, ]
  while (TRUE) {
    frac <- accumulation[x, y] / accumulation [cur[1], cur[2]]
    
    # exclude fractions < 0.25
    if (frac < 0.1) {
      break
    }
    
    # Include cell
    global.service.map[cur[1],cur[2], i] = dam.id
    global.service.fraction.map[cur[1],cur[2], i] = frac
    
    # Include all cells that receive water from this cell
    wateruse.id.cur = wateruse.id[cur[1], cur[2]]
    wateruse.sel.1 = !is.na(wateruse.1) & wateruse.1 == wateruse.id.cur
    wateruse.sel.2 = !is.na(wateruse.2) & wateruse.2 == wateruse.id.cur
    
    global.service.map.tmp = global.service.map[,,i]
    global.service.map.tmp[wateruse.sel.1] = dam.id
    global.service.map.tmp[wateruse.sel.2] = dam.id
    global.service.map[,,i] = global.service.map.tmp
    
    global.service.fraction.map.tmp = global.service.fraction.map[,,i]
    global.service.fraction.map.tmp[wateruse.sel.1] = frac
    global.service.fraction.map.tmp[wateruse.sel.2] = frac
    global.service.fraction.map[,,i] = global.service.fraction.map.tmp

    if (cur[1] == nex[1] && cur[2] == nex[2]) {
      break
    }

    cur <- nex
    nex <- downstream[cur[1], cur[2], ]
  }
}
global.service.map.sum = apply(X = global.service.map, MARGIN = c(1,2), FUN = function(x){sum(!is.na(x))})
global.service.fraction.map.sum = apply(X = global.service.fraction.map, MARGIN = c(1,2), FUN = function(x){sum(x, na.rm = T)})
image.plot(global.service.map.sum)
image.plot(global.service.fraction.map.sum)

for(i in order(gdam$CAP_MCM)){
  x <- gdam$MODEL_X[i]
  y <- gdam$MODEL_Y[i]
  
  global.service.tmp = global.service.map[,,i] > 0
  global.service.tmp[x,y] = 2
  #image.plot(global.service.tmp, main = gdam$CAP_MCM[i])
}

# Save
dir.create(dirname(gdam.service.out))
saveRDS(global.service.map, gdam.service.out)
dir.create(dirname(ldam.service.out))
saveRDS(local.service.map, ldam.service.out)

dir.create(dirname(gdam.service.fraction.out))
saveRDS(global.service.fraction.map, gdam.service.fraction.out)
dir.create(dirname(ldam.service.fraction.out))
saveRDS(local.service.fraction.map, ldam.service.fraction.out)

