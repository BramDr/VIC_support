library(openxlsx)
rm(list = ls())

observed.a.file <- "../../../../Data/Primary/FACE/Yang2006/Shizukuishi_FACE_98_00/Crop_soil_Shizukuishi_98_00_ambient.xlsx"
observed.e.file <- "../../../../Data/Primary/FACE/Yang2006/Shizukuishi_FACE_98_00/Crop_soil_Shizukuishi_98_00_elevated.xlsx"
out.phenology <- "./Saves/phenology_Shizukuishi.RDS"

treatment = data.frame(year = rep(1998:2000, each = 6),
                       nutrient = rep(rep(c("LN", "MN", "HN"), each = 2), 3),
                       co2 = c("A", "E"))
treatment$treatment = paste0(substr(treatment$year, 3, 4), treatment$nutrient, treatment$co2)
treatment = treatment[!(treatment$year == 1998 & treatment$nutrient == "LN"),]

observed.a = read.xlsx(observed.a.file, sheet = "Crop calendar", startRow = 2)
observed.e = read.xlsx(observed.e.file, sheet = "Crop calendar", startRow = 2)

phenology = data.frame(treatment = character(), 
                       plant = character(), 
                       transplant = character(), 
                       emergence = character(),
                       anthesis = character(),
                       maturity = character(),
                       stringsAsFactors = F)

i = 1
for(i in 1:nrow(treatment)) {
  if(treatment$co2[i] == "A"){
    observed = observed.a
  } else if(treatment$co2[i] == "E"){
    observed = observed.e
  }
  observed$year[2:3] = observed$year[1]
  observed$year[5:6] = observed$year[4]
  observed$year[8:9] = observed$year[7]
  
  observed.sel = observed[observed$year == treatment$year[i] & observed$N.level == treatment$nutrient[i],]
  
  plant = as.Date(observed.sel$seeding, origin = "1899-12-30")
  emergence = as.Date(observed.sel$emergence, origin = "1899-12-30")
  transplant = as.Date(observed.sel$`transplant.(tp)`, origin = "1899-12-30")
  anthesis = as.Date(observed.sel$heading, origin = "1899-12-30")
  maturity = as.Date(observed.sel$maturity, origin = "1899-12-30")
  
  plant = as.Date(paste0(treatment$year[i], format.Date(plant, "-%m-%d")))
  emergence = as.Date(paste0(treatment$year[i], format.Date(emergence, "-%m-%d")))
  transplant = as.Date(paste0(treatment$year[i], format.Date(transplant, "-%m-%d")))
  anthesis = as.Date(paste0(treatment$year[i], format.Date(anthesis, "-%m-%d")))
  maturity = as.Date(paste0(treatment$year[i], format.Date(maturity, "-%m-%d")))
  
  phenology[nrow(phenology) + 1,] = c(treatment$treatment[i], 
                                      as.character(plant),
                                      as.character(transplant),
                                     as.character(emergence),
                                      as.character(anthesis),
                                     as.character(maturity))
}

dir.create(dirname(out.phenology))
saveRDS(phenology, out.phenology)
