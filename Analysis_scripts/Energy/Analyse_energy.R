rm(list = ls())

# Input
plants.file = "Input/location_energy_global.csv"
capacity.dir = "Input"
generation.dir = "Input"
years = 1980:2011

# Load
plants = read.csv(plants.file, stringsAsFactors = F)
capacity.files = list.files(capacity.dir, pattern = "InstalCap", full.names = T)
generation.files = list.files(generation.dir, pattern = "Generation", full.names = T)

# Setup
capacity = data.frame(year = years, capacity = 0)
for(capacity.file in capacity.files){
  year = gsub(capacity.file, pattern = ".*_", replacement = "")
  year = as.numeric(gsub(year, pattern = ".txt", replacement = ""))
  
  if(! year %in% years){
    next
  }
  
  row = which(capacity$year == year)
  
  capacity.data = read.table(capacity.file, header = T)
  capacity.data[capacity.data == -9999] = NA
  capacity.data = capacity.data[capacity.data$Continent_Country == "WORLD",]
  
  #capacity$capacity[row] = capacity.data$Nuclear + capacity.data$Total_Fossil_Fuels
  capacity$capacity[row] = capacity.data$Total_Fossil_Fuels
}

generation = data.frame(year = years, generation = 0)
for(generation.file in generation.files){
  year = gsub(generation.file, pattern = ".*_", replacement = "")
  year = as.numeric(gsub(year, pattern = ".txt", replacement = ""))
  
  if(! year %in% years){
    next
  }
  
  row = which(capacity$year == year)
  
  generation.data = read.table(generation.file, header = T)
  generation.data[generation.data == -9999] = NA
  generation.data = generation.data[generation.data$Continent_Country == "WORLD",]
  
  generation$generation[row] = generation.data$Nuclear + generation.data$Total_Fossil_Fuels
}

included = data.frame(year = years, generation = 0, capacity = 0)
for(i in 1:nrow(included)){
  included$capacity[i] = sum(plants$Installed_Cap.MW.[plants$Year_operational <= included$year[i]])
  included$generation[i] = sum(plants$Installed_Cap.MW.[plants$Year_operational <= included$year[i]])
}

# Calculate
## generated percentage
(generation$generation * 1e9) / (capacity$capacity * 1e6 * 365 * 24)

# included generation
included$generation / generation$generation
mean(included$generation / generation$generation)

# included capacity
included$capacity / (capacity$capacity * 1e3)
mean(included$capacity / (capacity$capacity * 1e3))
