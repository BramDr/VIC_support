library(ggplot2)
rm(list = ls())

# Input
iso.file = "Input/ISO3166_dev.csv"
pop.file = "Input/country_population_global.csv"
with.file = "Input/DomPerCountry.csv"
gdp.file = "Input/country_GDP_global.csv"
fit.out = "Output/country_domestic_global_fitting_MC"
total.out = "Output/country_domestic_global_fitting_total.csv"

# Load
iso = read.csv(file = iso.file, stringsAsFactors = F)
gdp = read.csv(gdp.file, stringsAsFactors = F)
pop = read.csv(pop.file, stringsAsFactors = F)
with = read.csv(with.file, stringsAsFactors = F)

# Setup
set.seed(23021992)
itirations = 100

data = merge(gdp, with, by = c("Country_number", "Year"))
data = merge(data, pop, by = c("Country_number", "Year"))
data = merge(data, iso, by = c("Country_number"))

data = data[data$WithPc > 1,]
data = data[data$GdpPc > 1,]
data$lWithPc = log(data$WithPc, 10)
data$lGdpPc = log(data$GdpPc, 10)

data$row = 1:nrow(data)

# Calculate
for(itiration in 1:itirations){
  sel = c()
  isel = c()
  for(cn in unique(data$Subregion_number)){
	sd = data[data$Subregion_number == cn,]
	
	if(nrow(sd) == 1){
	  sel = c(sel, sd$row)
	  isel = c(isel, sd$row)
	} else {
	  ss = sample(x = 1:nrow(sd), size = ceiling(nrow(sd) / 2))
	  iss = 1:nrow(sd)
	  iss = iss[!iss %in% ss]
	  
	  sel = c(sel, sd$row[ss])
	  isel = c(isel, sd$row[iss])
	}
  }
  sum(sel %in% isel)

  data.fit = data[sel,]
  data.validation = data[isel,]

  # Save
  write.csv(x = data.fit, file = paste0(fit.out, "_", itiration, ".csv"), row.names = F)
}

write.csv(x = data, file = total.out, row.names = F)
