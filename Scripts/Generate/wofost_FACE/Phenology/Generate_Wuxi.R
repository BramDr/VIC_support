library(openxlsx)
rm(list = ls())

observed.file <- "../../../../Data/Primary/FACE/Hasegawa2017/Wuxi_FACE_R/Crop_soil_Wuxi_01_03_ambient_corrected_130502.xlsx"
out.phenology <- "./Saves/phenology_Wuxi.RDS"

treatment = data.frame(year = rep(2001:2003, each = 6),
                       nutrient = rep(rep(c("LN", "MN", "HN"), each = 2), 3),
                       co2 = c("A", "E"))
treatment$treatment = paste0(substr(treatment$year, 3, 4), treatment$nutrient, treatment$co2)
treatment = treatment[!(treatment$year == 2001 & treatment$nutrient == "HN"),]

observed = read.xlsx(observed.file, sheet = "China crop calendar")


phenology = data.frame(treatment = character(), 
                       plant = character(), 
                       transplant = character(),
                       emergence = character(),
                       anthesis = character(),
                       maturity = character(),
                       stringsAsFactors = F)

i = 1
for(i in 1:nrow(treatment)) {
  plant = as.Date(paste0(treatment$year[i], "-05-17"))
  transplant = as.Date(paste0(treatment$year[i], "-06-13"))
  
  col.idx = 3 + treatment$year[i] - 2001
  if(treatment$nutrient[i] == "HN"){
    row.idx = 2
  } else if (treatment$nutrient[i] == "MN"){
    row.idx = 4
  } else if (treatment$nutrient[i] == "LN"){
    row.idx = 6
  }
  if(treatment$co2[i] == "A"){
    row.idx = row.idx + 1
  }
  anthesis = plant + as.numeric(observed[row.idx, col.idx])
  
  if(treatment$nutrient[i] == "HN"){
    row.idx = 13
  } else if (treatment$nutrient[i] == "MN"){
    row.idx = 15
  } else if (treatment$nutrient[i] == "LN"){
    row.idx = 17
  }
  if(treatment$co2[i] == "A"){
    row.idx = row.idx + 1
  }
  maturity = plant + as.numeric(observed[row.idx, col.idx])
  
  phenology[nrow(phenology) + 1,] = c(treatment$treatment[i], 
                                      as.character(plant),
                                      as.character(transplant),
                                      NA,
                                      as.character(anthesis),
                                      as.character(maturity))
}

dir.create(dirname(out.phenology))
saveRDS(phenology, out.phenology)
