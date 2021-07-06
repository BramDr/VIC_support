library(openxlsx)
rm(list = ls())

observed.file <- "../../../../../Data/Primary/FACE/Yang2006/Shizukuishi_FACE_98_00/Crop_soil_Shizukuishi_98_00_elevated.xlsx"
out.fertilizer <- "./Saves/fertilizer_Shizukuishi.RDS"

treatment = data.frame(year = rep(1998:2000, each = 6),
                       nutrient = rep(rep(c("LN", "MN", "HN"), each = 2), 3),
                       co2 = c("A", "E"))
treatment$treatment = paste0(substr(treatment$year, 3, 4), treatment$nutrient, treatment$co2)
treatment = treatment[!(treatment$year == 1998 & treatment$nutrient == "LN"),]

fertilizer = read.xlsx(observed.file, sheet = "Agronomy", startRow = 21)
fertilizer = fertilizer[1:9,2:ncol(fertilizer)]
fertilizer$year[2:3] = fertilizer$year[1]
fertilizer$year[5:6] = fertilizer$year[4]
fertilizer$year[8:9] = fertilizer$year[7]

fertilizer.df = data.frame(treatment = character(), 
                        date = character(), 
                        N = numeric(), 
                        P = numeric(), 
                        stringsAsFactors = F)

i = 1
for(i in 1:nrow(treatment)) {
  row = which(fertilizer$year == treatment$year[i] & fertilizer$N.level == treatment$nutrient[i])
  
  date.idxs = 3:5
  n.idxs = 3:5
  
  j = 1
  for(j in 1:3){
    date = fertilizer[row, date.idxs[j]]
    date = strsplit(x = date, split = "\\(")[[1]][2]
    date = gsub(x = date, pattern = "\\)", replacement = "")
    date = as.Date(paste0(treatment$year[i], "/", date), format = "%Y/%d/%m")
    
    amount = fertilizer[row, n.idxs[j]]
    amount = strsplit(x = amount, split = "\\(")[[1]][1]
    amount = as.numeric(amount)
    amount.n = amount
    
    amount.p = 0
    if(j == 1){
      amount.p = 30
      if(treatment$year[i] == 1999){
        amount.p = 48
      }
    }
    
    amount.n = amount.n * 1e-3 * 1e4 # g m2 to kg ha-1
    amount.p = amount.p * 1e-3 * 1e4 # g m2 to kg ha-1
    
    fertilizer.df[nrow(fertilizer.df) + 1,] = c(treatment$treatment[i], as.character(date), amount.n, amount.p)
  }
}

dir.create(dirname(out.fertilizer))
saveRDS(fertilizer.df, out.fertilizer)
