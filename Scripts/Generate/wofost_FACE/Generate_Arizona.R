rm(list = ls())

fertilizer.file = "./Fertilizer/Saves/fertilizer_Arizona.RDS"
irrigation.file = "./Irrigation/Saves/irrigation_Arizona.RDS"
tsum.file = "./TSUM/Saves/tsum_Arizona.RDS"
phenology.file = "./Phenology/saves/phenology_Arizona.RDS"
crop.param.file = "./Parameters_base/crop_params_wheat.txt"
management.param.file = "./Parameters_base/management_params_wheat.txt"
param.file = "./Parameters_base/wofost_params_wheat_global.txt"
crop.param.out = "../../../Data/WOFOST/Parameters/Crop/FACE/Arizona/crop_params_Arizona.txt"
management.param.out = "../../../Data/WOFOST/Parameters/Management/FACE/Arizona/management_params_Arizona.txt"
param.out = "../../../Data/VIC/Parameters/FACE/Arizona/wofost_params_Arizona.txt"

fertilizer = readRDS(fertilizer.file)
irrigation = readRDS(irrigation.file)
tsum = readRDS(tsum.file)
phenology = readRDS(phenology.file)
crop.param = readLines(crop.param.file)
management.param = readLines(management.param.file)
param = readLines(param.file)

treatment = tsum$treatment[1]
for(treatment in tsum$treatment){
   crop.param.out.tmp = gsub(x = crop.param.out, pattern = "_Arizona", replacement = paste0("_", treatment, "_Arizona"))
   management.param.out.tmp = gsub(x = management.param.out, pattern = "_Arizona", replacement = paste0("_", treatment, "_Arizona"))
   param.out.tmp = gsub(x = param.out, pattern = "_Arizona", replacement = paste0("_", treatment, "_Arizona"))
   
   crop.param.tmp = crop.param
   tem.line = grep(x = crop.param, pattern = "^TSUMEM ")
   tsum1.line = grep(x = crop.param, pattern = "^TSUM1 ")
   tsum2.line = grep(x = crop.param, pattern = "^TSUM2 ")
   crop.param.tmp[tem.line] = paste0("TSUMEM = ", 
                                       round(tsum$tem[tsum$treatment == treatment]), 
                                       " ! Daily temperature sum from sowing to emergence [C day-1]")
   crop.param.tmp[tsum1.line] = paste0("TSUM1 = ", 
                                       round(tsum$tsum1[tsum$treatment == treatment]), 
                                       " ! Daily temperature sum from emergence to anthesis [C day-1]")
   crop.param.tmp[tsum2.line] = paste0("TSUM2 = ", 
                                       round(tsum$tsum2[tsum$treatment == treatment]), 
                                       " ! Daily temperature sum from emergence to anthesis [C day-1]")
   
   management.param.tmp = management.param
   fert.line = grep(x = management.param, pattern = "^FERNTAB")
   management.param.tmp = 
      management.param.tmp[1:length(management.param.tmp) != fert.line]
   management.param.tmp = 
      management.param.tmp[1:length(management.param.tmp) != fert.line]
   fert.sel = fertilizer[fertilizer$treatment == treatment,]
   
   i = 1
   for(i in 1:nrow(fert.sel)){
      management.param.pre = management.param.tmp[1:(fert.line - 1)]
      management.param.post = management.param.tmp[fert.line:length(management.param.tmp)]
      management.param.add = paste0(format.Date(fert.sel$date[i], "%m-%d"), " ", fert.sel$N[i])
      if(i == 1){
         management.param.add = paste0("FERNTAB =   ", management.param.add, " ! N fertiliser [date; kg ha-1]")
      } else {
         management.param.add = paste0("			", management.param.add)
      }
      management.param.tmp = c(management.param.pre, management.param.add, management.param.post)
      fert.line = fert.line + 1
   }
   
   irr.line = grep(x = management.param.tmp, pattern = "^IRRTAB")
   management.param.tmp = 
      management.param.tmp[1:length(management.param.tmp) != irr.line]
   management.param.tmp = 
      management.param.tmp[1:length(management.param.tmp) != irr.line]
   irr.sel = irrigation[irrigation$treatment == treatment,]
   
   i = 1
   for(i in 1:nrow(irr.sel)){
      management.param.pre = management.param.tmp[1:(irr.line - 1)]
      #management.param.post = management.param.tmp[irr.line:length(management.param.tmp)]
      management.param.post = ""
      management.param.add = paste0(format.Date(irr.sel$date[i], "%m-%d"), " ", irr.sel$irr[i] * 1e-1)
      if(i == 1){
         management.param.add = paste0("IRRTAB =   ", management.param.add, " ! irrigation [date; cm]")
      } else {
         management.param.add = paste0("			", management.param.add)
      }
      management.param.tmp = c(management.param.pre, management.param.add, management.param.post)
      irr.line = irr.line + 1
   }
   
   param.tmp = param
   param.tmp = gsub(x = param.tmp, pattern = "global/", replacement = "FACE/Arizona/")
   param.tmp = gsub(x = param.tmp, pattern = basename(crop.param.file), replacement = basename(crop.param.out.tmp))
   param.tmp = gsub(x = param.tmp, pattern = basename(management.param.file), replacement = basename(management.param.out.tmp))
   param.tmp = gsub(x = param.tmp, pattern = "12-15", replacement = format.Date(phenology$plant[phenology$treatment == treatment], "%m-%d"))
   
   dir.create(dirname(crop.param.out.tmp))
   dir.create(dirname(management.param.out.tmp))
   dir.create(dirname(param.out.tmp))
   writeLines(crop.param.tmp, crop.param.out.tmp)
   writeLines(management.param.tmp, management.param.out.tmp)
   writeLines(param.tmp, param.out.tmp)
}
