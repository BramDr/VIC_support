library(fields)
library(plyr)
rm(list = ls())

# Input
fao.file = "Input/FAO_crop_characteristics.csv"
cc.file = "Saves/MIRCA2000_cropping_calendars_corrected.csv"
cc.out = "Saves/MIRCA2000_cropping_calendars_corrected2.csv"

# Load
cc = read.csv(file = cc.file, header = TRUE, stringsAsFactors = F)
fao = read.csv(fao.file, stringsAsFactors = F)

# Setup
add.fc = function(x, columns){
  x = as.numeric(x)
  #print(x[columns == "rowname"])
  
  maxValue = x[columns == "areaMax"]
  out = x[columns %in% paste0("Area.", 1:12)] / maxValue
  
  return(out)
}
add.height = function(x, columns){
  x = as.numeric(x)
  #print(x[columns == "rowname"])
  
  row = which((x[columns == "crop"] - 1) %% 26 + 1 == fao$number)
  maxValue = as.numeric(fao[row, c("height")])
  out = x[columns %in% paste0("Kc.", 1:12)] * maxValue
  
  return(out)
}
add.albedo = function(x, columns){
  x = as.numeric(x)
  #print(x[columns == "rowname"])
  
  row = which((x[columns == "crop"] - 1) %% 26 + 1 == fao$number)
  maxValue = as.numeric(fao[row, c("albedo")])
  out = (x[columns %in% paste0("Kc.", 1:12)] > 0) * maxValue
  
  return(out)
}
add.displacement = function(x, columns){
  x = as.numeric(x)
  #print(x[columns == "rowname"])
  
  out = x[columns %in% paste0("height.", 1:12)] * 0.67
  
  return(out)
}
add.veg_rough = function(x, columns){
  x = as.numeric(x)
  #print(x[columns == "rowname"])
  
  out = x[columns %in% paste0("height.", 1:12)] * 0.123
  
  return(out)
}
add.LAI = function(x, columns){
  x = as.numeric(x)
  #print(x[columns == "rowname"])
  
  row = which((x[columns == "crop"] - 1) %% 26 + 1 == fao$number)
  maxValue = as.numeric(fao[row, c("LAI")])
  out = x[columns %in% paste0("Kc.", 1:12)] * maxValue
  
  return(out)
}

# Calculate
## Calculate crop area as the mean monthly crop area fraction of the maximum crop area
cropArea = apply(X = cc[,paste0("Area.",1:12)], MARGIN = 1, FUN = mean)
cc.tmp = cbind(cc, cropArea)

cc.cell.paddy = aggregate(formula = cropArea ~ cell_ID, data = cc.tmp[cc.tmp$crop == 3,], FUN = sum)
cc.cell.paddy$paddyCropArea = cc.cell.paddy$cropArea
cc.cell.irr = aggregate(formula = cropArea ~ cell_ID, data = cc.tmp[cc.tmp$crop %in% c(1:2, 4:26),], FUN = sum)
cc.cell.irr$irrCropArea = cc.cell.irr$cropArea
cc.cell.rain = aggregate(formula = cropArea ~ cell_ID, data = cc.tmp[cc.tmp$crop %in% c(27:52),], FUN = sum)
cc.cell.rain$rainCropArea = cc.cell.rain$cropArea

cc.merge = cc.tmp[, c("cell_ID", "cropArea", "areaMax", "crop")]
cc.merge = join(cc.merge, cc.cell.paddy[,c("cell_ID", "paddyCropArea")])
cc.merge = join(cc.merge, cc.cell.irr[,c("cell_ID", "irrCropArea")])
cc.merge = join(cc.merge, cc.cell.rain[,c("cell_ID", "rainCropArea")])
cc.merge$cellCropArea = NA
cc.merge$cellCropArea[cc.merge$crop == 3] = cc.merge$paddyCropArea[cc.merge$crop == 3]
cc.merge$cellCropArea[cc.merge$crop %in% c(1:2, 4:26)] = cc.merge$irrCropArea[cc.merge$crop %in% c(1:2, 4:26)]
cc.merge$cellCropArea[cc.merge$crop %in% c(27:52)] = cc.merge$rainCropArea[cc.merge$crop %in% c(27:52)]
cc.merge$cropAreaMax = cc.merge$areaMax * (cc.merge$cropArea / cc.merge$cellCropArea)

cc = cbind(cc, cc.merge[,c("cropAreaMax")])
new.colnames = colnames(cc)[1:(ncol(cc) - 1)]
new.colnames = c(new.colnames, "cropAreaMax")
colnames(cc) = new.colnames

fc = apply(X = cc, MARGIN = 1, FUN = add.fc, columns = colnames(cc))
fc = t(fc)

cc = cbind(cc, fc)
new.colnames = colnames(cc)[1:(ncol(cc) - 12)]
new.colnames = c(new.colnames, paste0("Fcanopy.", 1:12))
colnames(cc) = new.colnames

height = apply(X = cc, MARGIN = 1, FUN = add.height, columns = colnames(cc))
height = t(height)

cc = cbind(cc, height)
new.colnames = colnames(cc)[1:(ncol(cc) - 12)]
new.colnames = c(new.colnames, paste0("height.", 1:12))
colnames(cc) = new.colnames

albedo = apply(X = cc, MARGIN = 1, FUN = add.albedo, columns = colnames(cc))
albedo = t(albedo)

cc = cbind(cc, albedo)
new.colnames = colnames(cc)[1:(ncol(cc) - 12)]
new.colnames = c(new.colnames, paste0("albedo.", 1:12))
colnames(cc) = new.colnames

displacement = apply(X = cc, MARGIN = 1, FUN = add.displacement, columns = colnames(cc))
displacement = t(displacement)

cc = cbind(cc, displacement)
new.colnames = colnames(cc)[1:(ncol(cc) - 12)]
new.colnames = c(new.colnames, paste0("displacement.", 1:12))
colnames(cc) = new.colnames

veg_rough = apply(X = cc, MARGIN = 1, FUN = add.veg_rough, columns = colnames(cc))
veg_rough = t(veg_rough)

cc = cbind(cc, veg_rough)
new.colnames = colnames(cc)[1:(ncol(cc) - 12)]
new.colnames = c(new.colnames, paste0("veg_rough.", 1:12))
colnames(cc) = new.colnames

LAI = apply(X = cc, MARGIN = 1, FUN = add.LAI, columns = colnames(cc))
LAI = t(LAI)

cc = cbind(cc, LAI)
new.colnames = colnames(cc)[1:(ncol(cc) - 12)]
new.colnames = c(new.colnames, paste0("LAI.", 1:12))
colnames(cc) = new.colnames

cc.sel = subset(cc, select = -c(Kc.1,Kc.2,Kc.3,Kc.4,Kc.5,Kc.6,Kc.7,Kc.8,Kc.9,Kc.10,Kc.11,Kc.12,
                                      Area.1,Area.2,Area.3,Area.4,Area.5,Area.6,Area.7,Area.8,Area.9,Area.10,Area.11,Area.12,
                                      height.1,height.2,height.3,height.4,height.5,height.6,height.7,height.8,height.9,height.10,height.11,height.12))

# Save
write.csv(cc.sel, cc.out, row.names = F)
