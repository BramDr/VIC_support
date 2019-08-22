library(ggplot2)
rm(list = ls())

# Input
iso.file = "Input/ISO3166_dev.csv"
pop.file = "Input/country_population_global.csv"
with.file = "Input/IndPerCountry.csv"
gva.file = "Input/country_GVA_global.csv"
gdp.file = "Input/country_GDP_global.csv"
fit.out = "Output/country_industrial_global_fitting_MC"

# load
iso = read.csv(file = iso.file, stringsAsFactors = F)
gdp = read.csv(gdp.file, stringsAsFactors = F)
gva = read.csv(gva.file, stringsAsFactors = F)
with = read.csv(with.file, stringsAsFactors = F)

gva = merge(gva, gdp, by = c("Country_number", "Year"))
gva$Gva = gva$Gdp * (gva$GvaPGdp / 100)
gva = gva[,c("Country_number", "Year", "Gva")]

# Setup
itirations = 1000

data = merge(gva, with, by = c("Country_number", "Year"))
data = merge(data, pop, by = c("Country_number", "Year"))
data = merge(data, iso, by = c("Country_number"))

data = data[data$With > 1,]
data = data[data$Gva > 1,]
data$lWith = log(data$With, 10)
data$lGva = log(data$Gva, 10)

data$row = 1:nrow(data)

# Calculate
for(itiration in 1:itirations){
  sel = c()
  isel = c()
  for(cn in unique(data$Country_number)){
    sd = data[data$Country_number == cn,]
    
    if(nrow(sd) == 1){
      sel = c(sel, sd$row)
      isel = c(isel, sd$row)
    } else {
      set.seed(23021992)
      ss = sample(x = 1:nrow(sd), size = ceiling(nrow(sd) / 2))
      iss = 1:nrow(sd)
      iss = iss[!iss %in% ss]
      
      sel = c(sel, sd$row[ss])
      isel = c(isel, sd$row[iss])
    }
  }
  sum(sel %in% isel)
  
  data.fit = data[sel,]
  data.val = data[isel,]
  
  # Save
  write.csv(x = data.fit, file = paste0(fit.out, "_", itiration, ".csv"), row.names = F)
}
