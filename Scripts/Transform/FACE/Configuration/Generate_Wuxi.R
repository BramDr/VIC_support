rm(list = ls())

phenology.file = "../WOFOST/Phenology/Saves/phenology_Wuxi.RDS"
options.file = "./options.template.txt"
options.out = "../../../../Data/VIC/Configuration/FACE/Wuxi/options_Wuxi.txt"

phenology = readRDS(phenology.file)
options = readLines(options.file)

phenology$transplant = as.Date(phenology$transplant)
phenology$emergence = as.Date(phenology$emergence)
phenology$anthesis = as.Date(phenology$anthesis)
phenology$maturity = as.Date(phenology$maturity)

treatment = phenology$treatment[1]
for(treatment in unique(phenology$treatment)){
   options.out.tmp = gsub(x = options.out, pattern = "options_", replacement = paste0("options_", treatment, "_"))
   
   options.text = readLines(options.file)
   options.text = gsub(x = options.text, pattern = "DOMAIN_TEMPLATE", replacement = "Wuxi")
   options.text = gsub(x = options.text, pattern = "TREATMENT_TEMPLATE", replacement = treatment)
   
   param.line = grep(x = options.text, patter = "^PARAMETERS ")
   start.y.line = grep(x = options.text, patter = "^STARTYEAR")
   start.m.line = grep(x = options.text, patter = "^STARTMONTH")
   start.d.line = grep(x = options.text, patter = "^STARTDAY")
   end.y.line = grep(x = options.text, patter = "^ENDYEAR")
   end.m.line = grep(x = options.text, patter = "^ENDMONTH")
   end.d.line = grep(x = options.text, patter = "^ENDDAY")
   pot.irr.line = grep(x = options.text, patter = "^WOFOST_POTENTIAL_IRRIGATION")
   pot.fert.line = grep(x = options.text, patter = "^WOFOST_POTENTIAL_FERTILIZER")
   
   options.text[param.line] = paste0("PARAMETERS",
                                       "\t",
                                       paste0("/home/bram/Data/VIC/Parameters/FACE/Wuxi/VIC_params_Wuxi.nc"))
   options.text[start.y.line] = paste0("STARTYEAR",
                                  "\t",
                                  format.Date(phenology$transplant[phenology$treatment == treatment], "%Y"))
   options.text[start.m.line] = paste0("STARTMONTH",
                                  "\t",
                                  format.Date(phenology$transplant[phenology$treatment == treatment], "%m"))
   options.text[start.d.line] = paste0("STARTDAY",
                                  "\t",
                                  format.Date(phenology$transplant[phenology$treatment == treatment], "%d"))
   options.text[end.y.line] = paste0("ENDYEAR",
                                      "\t",
                                      format.Date(phenology$maturity[phenology$treatment == treatment], "%Y"))
   options.text[end.m.line] = paste0("ENDMONTH",
                                      "\t",
                                      format.Date(phenology$maturity[phenology$treatment == treatment], "%m"))
   options.text[end.d.line] = paste0("ENDDAY",
                                      "\t",
                                      format.Date(phenology$maturity[phenology$treatment == treatment], "%d"))
   options.text[pot.irr.line] = paste0("WOFOST_POTENTIAL_IRRIGATION",
                                      "\t",
                                      "TRUE")
   options.text[pot.fert.line] = paste0("WOFOST_POTENTIAL_FERTILIZER",
                                      "\t",
                                      "FALSE")
   
   dir.create(dirname(options.out.tmp))
   writeLines(options.text, options.out.tmp)
}
