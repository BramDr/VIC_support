library(fields)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
rm(list = ls())

# Input
iso.file = "Input/ISO3166_dev.csv"
isimip.time.file = "Saves/ISIMIP_time.csv"
isimip.space.file = "Saves/ISIMIP_space.RDS"
watch.time.file = "Saves/WATCH_time.csv"
watch.space.file = "Saves/WATCH_space.RDS"
vic.time.file = "Saves/VIC_time.csv"
vic.space.file = "Saves/VIC_space.RDS"
plot.out = "Output/Nonirrigation_results.pdf"

# Load
iso = read.csv(iso.file, stringsAsFactors = F)
isimip.time = read.csv(isimip.time.file)
watch.time = read.csv(watch.time.file)
vic.time = read.csv(vic.time.file)
isimip.space = readRDS(isimip.space.file)
watch.space = readRDS(watch.space.file)
vic.space = readRDS(vic.space.file)

# Setup
time = rbind(isimip.time, watch.time)
time = rbind(time, vic.time)

levels(time$variable)
time$variable = factor(time$variable, labels = c("(a) Domestic", "(b) Industrial", "(c) Livestock"), 
                       levels = c("adomww", "aindww", "aliveww"))
time$year = as.numeric(time$year)
time = time[time$year >= 1980 & time$year <= 2005,]
time$value = as.numeric(time$value) * 1e-9
time = time[complete.cases(time),]

space = c(isimip.space, watch.space, vic.space)
names(space)

region.number = data.frame(Region_number = unique(iso$Region_number))
region.number = merge(region.number, iso[,c("Region_number", "Region")], by = "Region_number")
region.number = region.number[!duplicated(region.number),]
region.number$Region[region.number$Region_number == 0] = "World"

fao.years = c(2005)
fao.live = c(22.802)
fao.live.time = data.frame(year = fao.years, value_live = fao.live)

sk.years = c(1980, 1990, 1995, 2000)
sk.ind = c(713, 735, 752, 776)
sk.ind.time = data.frame(year = sk.years, value_ind = sk.ind)

sk.years = c(1980, 1990, 1995, 2000)
sk.dom = c(219, 305, 344, 384)
sk.dom.time = data.frame(year = sk.years, value_dom = sk.dom)

time = merge(time, region.number, by.x = "region", by.y = "Region_number")
time$Region = factor(time$Region, levels = c("World", "Africa", "Americas", "Asia", "Europe", "Oceania"))

time = merge(time, fao.live.time, by = "year", all.x = T)
time = merge(time, sk.ind.time, by = "year", all.x = T)
time = merge(time, sk.dom.time, by = "year", all.x = T)
time$value_dom[!(time$region == 0 & time$model == "vic-wur" & time$variable == "(a) Domestic")] = NA
time$value_ind[!(time$region == 0 & time$model == "vic-wur" & time$variable == "(b) Industrial")] = NA
time$value_live[!(time$region == 0 & time$model == "vic-wur" & time$variable == "(c) Livestock")] = NA
time$reported[!is.na(time$value_live)] = "Steinfeld (2006)"
time$reported[!is.na(time$value_ind)] = "Shiklomanov (2000)"
time$reported[!is.na(time$value_dom)] = "Shiklomanov (2000)"
sel = sample(which(is.na(time$reported)), 100)
time$reported[sel] = "Shiklomanov (2000)"
time$reported[is.na(time$reported)] = "Steinfeld (2006)"

col.models = unique(as.character(time$model))
model.colors = brewer.pal(length(col.models), "Dark2")
names(model.colors) = col.models

shape.reported = unique(as.character(time$reported))
reported.shape = c(16,17)
names(reported.shape) = shape.reported

# Calculate
## Domestic slope
sel = time$region == 0 & time$model == "vic-wur" & time$variable == "(a) Domestic" & time$year >= 1980 & time$year <= 2000
slope.time = time[sel,]
mean(diff(slope.time$value))
sel = time$region == 0 & time$model == "h08" & time$variable == "(a) Domestic" & time$year >= 1980 & time$year <= 2000
slope.time = time[sel,]
mean(diff(slope.time$value))
sel = time$region == 0 & time$model == "watergap" & time$variable == "(a) Domestic" & time$year >= 1980 & time$year <= 2000
slope.time = time[sel,]
mean(diff(slope.time$value))
sel = time$region == 0 & time$model == "pcr-globwb" & time$variable == "(a) Domestic" & time$year >= 1980 & time$year <= 2000
slope.time = time[sel,]
mean(diff(slope.time$value))
sk.diff = diff(sk.dom.time$value_dom)
sk.diff = sk.diff / c(10,5,5)
mean(sk.diff)

## Industrial slope
sel = time$region == 0 & time$model == "vic-wur" & time$variable == "(b) Industrial" & time$year >= 1980 & time$year <= 2000
slope.time = time[sel,]
mean(diff(slope.time$value))
sel = time$region == 0 & time$model == "watergap" & time$variable == "(b) Industrial" & time$year >= 1980 & time$year <= 2000
slope.time = time[sel,]
mean(diff(slope.time$value))
sk.diff = diff(sk.ind.time$value_ind)
sk.diff = sk.diff / c(10,5,5)
mean(sk.diff)

## Plot
gg.plots = list()
for(var in c("(a) Domestic", "(b) Industrial", "(c) Livestock")){
  sel = time$variable == var
  
  gg.plot = ggplot(data = time[sel,], 
                   mapping = aes(x = year, y = value)) +
    geom_line(mapping = aes(colour = model)) +
    ggtitle(label = var) +
    scale_y_continuous(limits = c(0,NA), name = bquote("Withdrawn ["*km^3*"]")) + 
    facet_wrap(~Region, nrow = 6, scales = "free_y", strip.position = "left") +
    scale_x_continuous(name = "Year") + 
    scale_color_manual(name = "Impact model", values = model.colors) +
    scale_shape_manual(name = "Reported", values = reported.shape, labels = names(reported.shape)) + 
    theme_bw() +
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=12))
  
  if(var == "(a) Domestic"){
    gg.plot = gg.plot + geom_point(mapping = aes(y = value_dom, shape = reported)) +
      scale_x_continuous(name = " ")
  } else if(var == "(b) Industrial"){
    gg.plot = gg.plot + geom_point(mapping = aes(y = value_ind, shape = reported)) +
      scale_y_continuous(limits = c(0,NA), name = " ") + 
      theme(strip.background = element_blank(),
            strip.text = element_blank())
  } else if(var == "(c) Livestock"){
    gg.plot = gg.plot + geom_point(mapping = aes(y = value_live, shape = reported)) +
      scale_x_continuous(name = " ") +
      scale_y_continuous(limits = c(0,NA), name = " ") + 
      theme(strip.background = element_blank(),
            strip.text = element_blank())
  }
  
  gg.plots[[length(gg.plots) + 1]] = gg.plot
}

gg.arr.plot = ggarrange(plotlist = gg.plots, ncol = 3, common.legend = T, legend = "bottom")
plot(gg.arr.plot)

# Save
pdf(plot.out, width = 9, height = 8)
plot(gg.arr.plot)
dev.off()