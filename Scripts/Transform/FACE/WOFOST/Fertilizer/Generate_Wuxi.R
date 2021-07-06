library(openxlsx)
rm(list = ls())

observed.file <- "../../../../../Data/Primary/FACE/Hasegawa2017/Wuxi_FACE_R/Crop_soil_Wuxi_01_03_ambient_corrected_130502.xlsx"
out.fertilizer <- "./Saves/fertilizer_Wuxi.RDS"

treatment = data.frame(year = rep(2001:2003, each = 6),
                       nutrient = rep(rep(c("LN", "MN", "HN"), each = 2), 3),
                       co2 = c("A", "E"))
treatment$treatment = paste0(substr(treatment$year, 3, 4), treatment$nutrient, treatment$co2)
treatment = treatment[!(treatment$year == 2001 & treatment$nutrient == "HN"),]

fertilizer = read.xlsx(observed.file, sheet = "Agronomy", startRow = 18)
fertilizer = fertilizer[1:15,2:ncol(fertilizer)]
fertilizer$year[3:4] = fertilizer$year[2]
fertilizer$year[6:7] = fertilizer$year[5]
fertilizer$year[9:10] = fertilizer$year[8]

fertilizer.df = data.frame(treatment = character(), 
                        date = character(), 
                        N = numeric(), 
                        P = numeric(), 
                        stringsAsFactors = F)

i = 2
for(i in 1:nrow(treatment)) {
  row = which(fertilizer$year == treatment$year[i] & fertilizer$N.level == treatment$nutrient[i])
  row2 = max(which(fertilizer$year == treatment$year[i]))
  
  date.idxs = c(3, 5, 7)
  n.idxs = c(3, 5, 7)
  p.idxs = c(2,3)
  
  j = 1
  for(j in 1:3){
    date = fertilizer[row, date.idxs[j]]
    date = strsplit(x = date, split = "\\(")[[1]][2]
    date = gsub(x = date, pattern = "\\)", replacement = "")
    date = as.Date(paste0(treatment$year[i], "/", date), format = "%Y/%d/%m")
    
    k.idxs = 1:2
    if(j >2){
      k.idxs = 1
    }
    
    amount.n = 0
    for(k in k.idxs){
      amount = fertilizer[row, n.idxs[j] + k - 1]
      amount = strsplit(x = amount, split = "\\(")[[1]][1]
      amount = as.numeric(amount)
      
      amount.n = sum(amount.n, amount, na.rm = T)
    }
    
    amount.p = 0
    if(j < 2 || (j < 3 && treatment$year[i] == 2001)) {
      amount = fertilizer[row2, p.idxs[j]]
      amount = strsplit(x = amount, split = "\\(")[[1]][1]
      amount = as.numeric(amount)
      
      amount.p = sum(amount.p, amount, na.rm = T)
    }
    
    amount.n = amount.n * 1e-3 * 1e4 # g m2 to kg ha-1
    amount.p = amount.p * 1e-3 * 1e4 # g m2 to kg ha-1
    
    fertilizer.df[nrow(fertilizer.df) + 1,] = c(treatment$treatment[i], as.character(date), amount.n, amount.p)
  }
}

dir.create(dirname(out.fertilizer))
saveRDS(fertilizer.df, out.fertilizer)
