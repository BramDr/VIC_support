rm(list = ls())

constants.file = "./VIC_constants_default.txt"
constants.out = "../../../../Data/VIC/Constants/FACE/Arizona/constants_Arizona.txt"

constants.text = readLines(constants.file)
   
miner.line = grep(x = constants.text, patter = "^MINER_PERIOD")
constants.text[miner.line] = paste0("MINER_PERIOD",
                              "\t", "30")   
                              
dir.create(dirname(constants.out))
writeLines(constants.text, constants.out)
