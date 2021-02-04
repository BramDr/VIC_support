library(raster)
rm(list = ls())

in.dir = "./Saves/LAI"
plot.out = "./LAI_difference.pdf"

sum.bohn.files = list.files(in.dir, pattern = "bohn_sum", full.names = T)
count.bohn.files = list.files(in.dir, pattern = "bohn_count", full.names = T)
sum.quality.files = list.files(in.dir, pattern = "quality_sum", full.names = T)
count.quality.files = list.files(in.dir, pattern = "quality_count", full.names = T)

tiles = gsub(x = basename(sum.bohn.files), pattern = "_500m.*", replacement = "")
tiles = gsub(x = tiles, pattern = ".*_", replacement = "")
tiles = unique(tiles)

pdf(plot.out)
for(tile in tiles) {
  sum.bohn.file = grep(x = sum.bohn.files, pattern = tile, value = T)
  count.bohn.file = grep(x = count.bohn.files, pattern = tile, value = T)
  sum.quality.file = grep(x = sum.quality.files, pattern = tile, value = T)
  count.quality.file = grep(x = count.quality.files, pattern = tile, value = T)
  
  sum.bohn = readRDS(sum.bohn.file)
  count.bohn = readRDS(count.bohn.file)
  sum.quality = readRDS(sum.quality.file)
  count.quality = readRDS(count.quality.file)
  
  avg.bohn = sum.bohn / count.bohn
  avg.bohn[count.bohn < 5] = NA
  avg.quality = sum.quality / count.quality
  avg.quality[count.quality < 5] = NA
  
  plot(raster(avg.bohn[,,6]), main = "bohn")
  plot(raster(avg.quality[,,6]), main = "quality")
  plot(raster(avg.bohn[,,6] - avg.quality[,,6]), main = "diff", zlim = c(-0.5, 0.5))
  plot(raster(is.na(avg.bohn[,,6]) & !is.na(avg.quality[,,6])), main = "omit")
}
dev.off()