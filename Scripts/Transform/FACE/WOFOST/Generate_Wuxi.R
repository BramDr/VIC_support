rm(list = ls())

fertilizer.file = "./Fertilizer/Saves/fertilizer_Wuxi.RDS"
irrigation.file = "./Irrigation/Saves/irrigation_Wuxi.RDS"
initial.file = "./Initialization/Saves/initial_Wuxi.RDS"
tsum.file = "./TSUM/Saves/tsum_Wuxi.RDS"
phenology.file = "./Phenology/saves/phenology_Wuxi.RDS"
crop.param.file = "./Parameters_base/crop_params_rice.txt"
management.param.file = "./Parameters_base/management_params_default.txt"
param.file = "./Parameters_base/wofost_params_rice_global.txt"
crop.param.out = "../../../../Data/WOFOST/Parameters/Crop/FACE/Wuxi/crop_params_Wuxi.txt"
management.param.out = "../../../../Data/WOFOST/Parameters/Management/FACE/Wuxi/management_params_Wuxi.txt"
param.out = "../../../../Data/VIC/Parameters/FACE/Wuxi/wofost_params_Wuxi.txt"

fertilizer = readRDS(fertilizer.file)
irrigation = readRDS(irrigation.file)
initial = readRDS(initial.file)
tsum = readRDS(tsum.file)
phenology = readRDS(phenology.file)
crop.param = readLines(crop.param.file)
management.param = readLines(management.param.file)
param = readLines(param.file)

treatment = tsum$treatment[1]
for(treatment in tsum$treatment){
   crop.param.out.tmp = gsub(x = crop.param.out, pattern = "_Wuxi", replacement = paste0("_", treatment, "_Wuxi"))
   management.param.out.tmp = gsub(x = management.param.out, pattern = "_Wuxi", replacement = paste0("_", treatment, "_Wuxi"))
   param.out.tmp = gsub(x = param.out, pattern = "_Wuxi", replacement = paste0("_", treatment, "_Wuxi"))
   
   crop.param.tmp = crop.param
   tem.line = grep(x = crop.param, pattern = "^TSUMEM ")
   tsum1.line = grep(x = crop.param, pattern = "^TSUM1 ")
   tsum2.line = grep(x = crop.param, pattern = "^TSUM2 ")
   dvsi.line = grep(x = crop.param, pattern = "^DVSI ")
   tdwi.line = grep(x = crop.param, pattern = "^TDWI ")
   
   tsumem = round(tsum$tem[tsum$treatment == treatment])
   tsum1 = round(tsum$tsum1[tsum$treatment == treatment]) + round(initial$tsum[initial$treatment == treatment])
   tsum2 = round(tsum$tsum2[tsum$treatment == treatment])
   dvsi = round(initial$tsum[initial$treatment == treatment] / tsum1, digits = 2)
   tdwi = round(initial$dry.matter[initial$treatment == treatment])
   
   crop.param.tmp[tem.line] = paste0("TSUMEM = ", 
                                       tsumem, 
                                       " ! Daily temperature sum from sowing to emergence [C day-1]")
   crop.param.tmp[tsum1.line] = paste0("TSUM1 = ", 
                                       tsum1, 
                                       " ! Daily temperature sum from emergence to anthesis [C day-1]")
   crop.param.tmp[tsum2.line] = paste0("TSUM2 = ", 
                                       tsum2, 
                                       " ! Daily temperature sum from emergence to anthesis [C day-1]")
   crop.param.tmp[dvsi.line] = paste0("DVSI = ", 
                                       dvsi, 
                                       " ! Initial development stage [0 = emergence 1 = anthesis 2 = maturity]")
   crop.param.tmp[tdwi.line] = paste0("TDWI = ", 
                                       tdwi, 
                                       " ! Initial crop dry weight [kg ha-1]")
   
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
   param.tmp = gsub(x = param.tmp, pattern = "global/", replacement = "FACE/Wuxi/")
   param.tmp = gsub(x = param.tmp, pattern = basename(crop.param.file), replacement = basename(crop.param.out.tmp))
   param.tmp = gsub(x = param.tmp, pattern = basename(management.param.file), replacement = basename(management.param.out.tmp))
   param.tmp = gsub(x = param.tmp, pattern = "12-15", replacement = format.Date(phenology$transplant[phenology$treatment == treatment], "%m-%d"))
   
   dir.create(dirname(crop.param.out.tmp))
   dir.create(dirname(management.param.out.tmp))
   dir.create(dirname(param.out.tmp))
   writeLines(crop.param.tmp, crop.param.out.tmp)
   writeLines(management.param.tmp, management.param.out.tmp)
   writeLines(param.tmp, param.out.tmp)
}
