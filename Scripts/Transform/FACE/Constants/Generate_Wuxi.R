rm(list = ls())

constants.file = "./VIC_constants_default.txt"
constants.out = "../../../../Data/VIC/Constants/FACE/Wuxi/constants_Wuxi.txt"

constants.text = readLines(constants.file)
   
miner.line = grep(x = constants.text, patter = "^MINER_PERIOD")
constants.text[miner.line] = paste0("MINER_PERIOD",
                              "\t", "120")   
                              
dir.create(dirname(constants.out))
writeLines(constants.text, constants.out)
