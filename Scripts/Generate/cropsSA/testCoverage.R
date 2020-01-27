library(fields)
rm(list = ls())

crop.file = "./Saves/crop_mapping_MIRCA.csv"
Cc.dir = "./Saves"
plant.file = "./Saves/plantDay_MIRCA_30min_global.RDS"
harvest.file = "./Saves/harvestDay_MIRCA_30min_global.RDS"

crops = read.csv(crop.file, stringsAsFactors = F)
plant = readRDS(plant.file)
harvest = readRDS(harvest.file)

Cc.files = list.files(path = Cc.dir, pattern = "Cc_.*_MIRCA_", full.names = T)
for(i in 1:nrow(crops)){
  crop.name = crops$mirca.name[i]
  print(crop.name)
  
  Cc.file = grep(x = Cc.files, pattern = paste0("Cc_", i, "_"), value = T)
  print(basename(Cc.file))
  
  Cc.c = readRDS(Cc.file)
  plant.c <- plant[,,i]
  harvest.c <- harvest[,,i]
  
  for(plant.offset in c(5,15,25)) {
    print(plant.offset)
    
    plant.c.adj = plant.c + plant.offset
    plant.c.adj[!is.na(plant.c.adj) & plant.c.adj > 365] = plant.c.adj[!is.na(plant.c.adj) & plant.c.adj > 365] - 365
    image.plot(plant.c.adj)
    
    for(x in 1:dim(plant.c.adj)[1]) {
      for(y in 1:dim(plant.c.adj)[2]) {
        if(is.na(harvest.c[x,y])) {
          next
        }
        
        s.month = as.numeric(format.Date(as.Date(plant.c.adj[x,y] - 1, origin = "1970-01-01"), "%m"))
        e.month = as.numeric(format.Date(as.Date(harvest.c[x,y] - 1, origin = "1970-01-01"), "%m"))
        if (Cc.c[x,y,s.month] != 1) {
          print("ERR")
          print(paste0("Start - J: ", plant.c.adj[x,y], " M: ", s.month, " Cc: ", Cc.c[x,y,s.month]))
          print(paste0("End - J: ", harvest.c[x,y], " M: ", e.month, " Cc: ", Cc.c[x,y,e.month]))
          stop()
        } else if (Cc.c[x,y,e.month] != 1) {
          print("ERR")
          print(paste0("Start - J: ", plant.c.adj[x,y], " M: ", s.month, " Cc: ", Cc.c[x,y,s.month]))
          print(paste0("End - J: ", harvest.c[x,y], " M: ", e.month, " Cc: ", Cc.c[x,y,e.month]))
          stop()
        }
      }
    }
  }
}
