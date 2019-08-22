library(openxlsx)
library(zoo)
rm(list = ls())

# Input
iso.file = "Input/ISO3166_dev.csv"
pop.file = "Input/country_population_global.csv"
gva.file = "Output/country_GVA_global.csv"
gva.out = "Output/country_GVA_interpolated_global.csv"
years = 1979:2016

# Load
iso = read.csv(iso.file, stringsAsFactors = F, na.strings = "NA")
iso = iso[complete.cases(iso$Country_code3),]
pop = read.csv(pop.file, stringsAsFactors = F)
gva = read.csv(gva.file, stringsAsFactors = F)

# Setup
int.data = data.frame(Country_number = rep(unique(iso$Country_number), each = length(years)), 
                      Year = rep(years, length(unique(iso$Country_number))))

int.data = merge(int.data, gva, by = c("Country_number", "Year"), all.x = T)
int.data = merge(int.data, pop, by = c("Country_number", "Year"), all.x = T)
int.data = merge(int.data, iso, by = c("Country_number"))

# Calculate
subregion.pc = data.frame(Subregion_number = numeric(), Year = numeric(), Average = numeric())
region.pc = data.frame(Region_number = numeric(), Year = numeric(), Average = numeric())

for(region in unique(int.data$Subregion_number)){
  subdata = int.data[int.data$Subregion_number == region,]
  if(nrow(subdata) == 0){
    next
  }
  
  for(year in years){
    subdata2 = subdata[subdata$Year == year,]
    if(nrow(subdata2) == 0){
      next
    }
    
    subregion.pc[nrow(subregion.pc) + 1,] = c(region, year, mean(subdata2$GvaPGdp, na.rm = T))
  }
}

for(region in unique(int.data$Region_number)){
  subdata = int.data[int.data$Region_number == region,]
  if(nrow(subdata) == 0){
    next
  }
  
  for(year in years){
    subdata2 = subdata[subdata$Year == year,]
    if(nrow(subdata2) == 0){
      next
    }
    
    region.pc[nrow(region.pc) + 1,] = c(region, year, mean(subdata2$GvaPGdp, na.rm = T))
  }
}

# Extrapolation is population is present
sel = is.na(int.data$GvaPGdp)
sel2 = match(int.data$Subregion_number[sel], subregion.pc$Subregion_number)
int.data$GvaPGdp[sel] = subregion.pc$Average[sel2]

sel = is.na(int.data$GvaPGdp)
sel2 = match(int.data$Region_number[sel], region.pc$Region_number)
int.data$GvaPGdp[sel] = region.pc$Average[sel2]

# Test if all countries are done, if not they do not receive any GVA
test = aggregate(formula = GvaPGdp ~ Country_number, data = int.data, FUN = mean, na.rm = T, na.action = na.pass)
print(sum(is.na(test$GvaPGdp)))

# Now all counties are assigned, interpolate in time
for(code in unique(int.data$Country_number)){
  code.data = int.data[int.data$Country_number == code,]
  code.data = code.data[order(code.data$Year),]
  
  if(sum(is.na(code.data$GvaPGdp)) == 0){
    next
  }
  
  # enter in between years
  code.data.old = code.data
  
  fill.values.GdpPc = is.na(code.data$GvaPGdp)
  
  if(sum(fill.values.GdpPc) > 0){
    gva.cal = rollapply(data = code.data$GvaPGdp, width = 5, FUN = mean, na.rm = T)
    gva.cal[gva.cal <= 0] = NA
    code.data$GvaPGdp[fill.values.GdpPc] = gva.cal[fill.values.GdpPc]
    
    if(sum(is.na(code.data.old$GvaPGdp)) != sum(is.na(code.data$GvaPGdp))){
      #plot(code.data$GvaPGdp, main = "rollapply-Pc", ylim = c(0, max(code.data$GvaPGdp, na.rm = T)))
      #points(code.data.old$GvaPGdp, col = "orange")
    }
  }
  
  # enter in before/after years
  code.data.old = code.data
  
  fill.values.GdpPc = is.na(code.data$GvaPGdp)
  
  if(sum(fill.values.GdpPc) > 0){
    gva.idx = which(!is.na(code.data$GvaPGdp))
    gva.idx = gva.idx[1:min(5, length(gva.idx))]
    gva.cal = mean(code.data$GvaPGdp[gva.idx])
    
    fill.idx = which(is.na(code.data$GvaPGdp))
    fill.idx = fill.idx[fill.idx < min(gva.idx)]
    
    code.data$GvaPGdp[fill.idx] = gva.cal
    
    gva.idx = which(!is.na(code.data$GvaPGdp))
    gva.idx = gva.idx[max((length(gva.idx) - 5), 1):length(gva.idx)]
    gva.cal = mean(code.data$GvaPGdp[gva.idx])
    
    fill.idx = which(is.na(code.data$GvaPGdp))
    fill.idx = fill.idx[fill.idx > max(gva.idx)]
    
    code.data$GvaPGdp[fill.idx] = gva.cal
    
    if(sum(is.na(code.data.old$GvaPGdp)) != sum(is.na(code.data$GvaPGdp))){
      plot(code.data$GvaPGdp, main = "before/after-Pc", ylim = c(0, max(code.data$GvaPGdp, na.rm = T)))
      points(code.data.old$GvaPGdp, col = "orange")
    }
  }
  
  sel = as.numeric(rownames(code.data))
  int.data[sel,] = code.data
}

int.data$GvaPGdp[is.na(int.data$GvaPGdp)] = 0
int.data = int.data[,c("Country_number", "Year", "GvaPGdp")]

# Save
write.csv(x = int.data, file = gva.out, row.names = F)
