rm(list = ls())

phenology.file = "../WOFOST/Phenology/Saves/phenology_observed.RDS"
options.file = "./options.template.txt"
options.out = "../../../../Data/VIC/Configuration/FACE/options_point.txt"

phenology = readRDS(phenology.file)
options = readLines(options.file)

treatment = phenology$treatment[1]
for(treatment in phenology$treatment){
   options.out.tmp = gsub(x = options.out, pattern = "_point", replacement = paste0("_", treatment, "_point"))
   
   options.tmp = options
   vic.param.line = grep(x = options.tmp, patter = "^PARAMETERS")
   irr.param.line = grep(x = options.tmp, patter = "^IRRIGATION_PARAMETERS")
   wofost.param.line = grep(x = options.tmp, patter = "^WOFOST_TEXT_PARAMETERS")
   crop.param.line = grep(x = options.tmp, patter = "^CROP_PARAMETERS")
   force.lines = grep(x = options.tmp, patter = "^FORCE_TYPE")
   co2.line = grep(x = options.tmp, patter = "^PLUGIN_FORCE_TYPE")
   out.line = grep(x = options.tmp, patter = "^OUTFILE")
   start.y.line = grep(x = options.tmp, patter = "^STARTYEAR")
   start.m.line = grep(x = options.tmp, patter = "^STARTMONTH")
   start.d.line = grep(x = options.tmp, patter = "^STARTDAY")
   end.y.line = grep(x = options.tmp, patter = "^ENDYEAR")
   end.m.line = grep(x = options.tmp, patter = "^ENDMONTH")
   end.d.line = grep(x = options.tmp, patter = "^ENDDAY")
   
   options.tmp[vic.param.line] = paste0("PARAMETERS",
                                        "\t",
                                        "/home/bram/Data/VIC/Parameters/FACE/VIC_params_", treatment, "_point.nc")
   options.tmp[irr.param.line] = paste0("IRRIGATION_PARAMETERS",
                                        "\t",
                                        "/home/bram/Data/VIC/Parameters/FACE/irr_params_point.nc")
   options.tmp[wofost.param.line] = paste0("WOFOST_TEXT_PARAMETERS",
                                           "\t",
                                           "/home/bram/Data/VIC/Parameters/FACE/wofost_params_", treatment, "_point.txt")
   options.tmp[crop.param.line] = paste0("CROP_PARAMETERS",
                                         "\t",
                                         "/home/bram/Data/VIC/Parameters/FACE/crop_params_point.nc")
   for(force.line in force.lines){
      options.tmp[force.line] = gsub(x = options.tmp[force.line], pattern = "_point", replacement = "_point")
   }
   options.tmp[co2.line] = gsub(x = options.tmp[co2.line], pattern = "_observed", replacement = paste0("_", treatment, "_observed"))
   options.tmp[out.line] = paste0("OUTFILE",
                                         "\t",
                                         "fluxes_", treatment, "_point")
   options.tmp[start.y.line] = paste0("STARTYEAR",
                                  "\t",
                                  format.Date(phenology$plant[phenology$treatment == treatment], "%Y"))
   options.tmp[start.m.line] = paste0("STARTMONTH",
                                  "\t",
                                  format.Date(phenology$plant[phenology$treatment == treatment], "%m"))
   options.tmp[start.d.line] = paste0("STARTDAY",
                                  "\t",
                                  format.Date(phenology$plant[phenology$treatment == treatment], "%d"))
   options.tmp[end.y.line] = paste0("ENDYEAR",
                                      "\t",
                                      format.Date(phenology$maturity[phenology$treatment == treatment], "%Y"))
   options.tmp[end.m.line] = paste0("ENDMONTH",
                                      "\t",
                                      format.Date(phenology$maturity[phenology$treatment == treatment], "%m"))
   options.tmp[end.d.line] = paste0("ENDDAY",
                                      "\t",
                                      format.Date(phenology$maturity[phenology$treatment == treatment], "%d"))
   
   dir.create(dirname(options.out.tmp))
   writeLines(options.tmp, options.out.tmp)
}
