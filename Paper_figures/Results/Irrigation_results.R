library(ncdf4)
library(fields)
library(ggplot2)
library(ggpubr)
rm(list = ls())

# Input
iso.file = "Input/ISO3166_dev.csv"
isimip.time.file = "Saves/ISIMIP_time.csv"
isimip.space.file = "Saves/ISIMIP_space.RDS"
watch.time.file = "Saves/WATCH_time.csv"
watch.space.file = "Saves/WATCH_space.RDS"
vic.time.file = "Saves/VIC_time.csv"
vic.space.file = "Saves/VIC_space.RDS"
crop.file = "Input/VIC_params_MIRCA2000_global.nc"
plot.out = "Output/Irrigation_results.pdf"

# Load
iso = read.csv(iso.file, stringsAsFactors = F)
isimip.time = read.csv(isimip.time.file)
watch.time = read.csv(watch.time.file)
vic.time = read.csv(vic.time.file)
isimip.space = readRDS(isimip.space.file)
watch.space = readRDS(watch.space.file)
vic.space = readRDS(vic.space.file)

nc = nc_open(crop.file)
rice = ncvar_get(nc, nc$var$Cv, start = c(1,1,13), count = c(-1,-1,1))
crop = ncvar_get(nc, nc$var$Cv, start = c(1,1,12), count = c(-1,-1,1))
nc_close(nc)

# Setup
time = rbind(isimip.time, watch.time)
time = rbind(time, vic.time)

levels(time$variable)
time$variable = factor(time$variable, labels = c("Potential", "Actual (without EFR)", "Actual (with EFR)"), 
                       levels = c("pirrww", "airrww", "airrww_efr"))
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

region.number$Region[region.number$Region == "World"] = "(a) World"
region.number$Region[region.number$Region == "Africa"] = "(b) Africa"
region.number$Region[region.number$Region == "Americas"] = "(c) Americas"
region.number$Region[region.number$Region == "Asia"] = "(d) Asia"
region.number$Region[region.number$Region == "Europe"] = "(e) Europe"
region.number$Region[region.number$Region == "Oceania"] = "(f) Oceania"

sk.years = c(1980, 1990, 1995, 2000, 2010)
sk.irr = c(2112, 2425, 2504, 2605, 2817)
sk.irr.time = data.frame(years = sk.years, value = sk.irr)

rice.frac = array(NA, dim = dim(rice))
for(x in 1:dim(rice.frac)[1]){
  for(y in 1:dim(rice.frac)[2]){
    if(is.na(crop[x,y])){
      next
    }
    if(crop[x,y] + rice[x,y] > 0){
      rice.frac[x,y] = rice[x,y] / (rice[x,y] + crop[x,y])
    } else {
      rice.frac[x,y] = rice[x,y]
    }
  }
}
image.plot(rice.frac)

# Calculate
## VIC-WUR mean and sd
sel = time$model == "vic-wur" & time$variable == "Actual (without EFR)" & time$region == 0
mean(time$value[sel])
sd(time$value[sel])
sel = time$model == "vic-wur" & time$variable == "Potential" & time$region == 0
mean(time$value[sel])
sd(time$value[sel])

## Ensamble mean and sd
sel = time$model != "vic-wur" & time$variable == "Actual (without EFR)" & time$region == 0
mean(time$value[sel])
sd(time$value[sel])
sel = time$model != "vic-wur" & time$variable == "Potential" & time$region == 0
mean(time$value[sel])
sd(time$value[sel])

## VIC-WUR and ensamble deficit
sel = time$model == "vic-wur" & time$variable == "Actual (without EFR)" & time$region == 0
median(time$value[sel]) - sk.irr.time$value[sk.irr.time$years == 2000]
sel = time$model != "vic-wur" & time$variable == "Actual (without EFR)" & time$region == 0
median(time$value[sel]) - sk.irr.time$value[sk.irr.time$years == 2000]

## VIC & VIC-WUR potential irrigation differences
space.vic = space[["vic_pirrww"]]
space.vicwur = space[["vic-wur_pirrww_tot"]]
sel = rice.frac == 0
(sum(space.vic[sel], na.rm = T) - sum(space.vicwur[sel], na.rm = T)) / sum(space.vicwur[sel], na.rm = T)
sel = rice.frac > 0.5
(sum(space.vicwur[sel], na.rm = T) - sum(space.vic[sel], na.rm = T)) / sum(space.vic[sel], na.rm = T)

## Plot
gg.plots = list()
for(ireg in 1:nrow(region.number)){
  reg.nr = region.number$Region_number[ireg]
  reg.name = region.number$Region[ireg]
  sel = time$region == reg.nr
  
  gg.plot = ggplot(data = time[sel,], 
                   mapping = aes(x = variable, y = value)) +
    geom_violin(mapping = aes(fill = variable, colour = variable), 
                linetype = 1, trim = F, scale = "width", draw_quantiles = 0.5) +
    facet_wrap(~model, nrow = 1) + 
    ggtitle(label = reg.name) +
    scale_x_discrete(name = "Impact model") + 
    scale_y_continuous(name = " ", limits = c(0, max(time$value[sel]) * 1.1)) +
    scale_fill_brewer(palette="Dark2") +
    scale_colour_brewer(palette="Dark2") +
    labs(fill = "Type", linetype = "Reported") + 
    guides(colour = F) + 
    theme_bw() +
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(),
          plot.title = element_text(size=12))
  
  if(ireg == 1 || ireg == 2){
  } else {
    if(ireg == 3) {
      gg.plot = gg.plot + 
        scale_y_continuous(name = bquote("Water withdrawal ["*km^3*"]"))
    }
    gg.plot = gg.plot +
      theme(strip.background = element_blank(),
            strip.text.x = element_blank())
    
  }
  
  if(reg.nr == 0){
    gg.plot = gg.plot + 
      geom_hline(mapping = aes(yintercept = sk.irr[sk.years == 2000], 
                               linetype = "Shiklomanov (2000)"))
  }
  
  gg.plots[[length(gg.plots) + 1]] = gg.plot
}

ggarrange(plotlist = gg.plots, ncol = 2, nrow = 3, common.legend = T, legend = "bottom")

# Save
pdf(plot.out, width = 10, height = 6)
ggarrange(plotlist = gg.plots, ncol = 2, nrow = 3, common.legend = T, legend = "bottom")
dev.off()
