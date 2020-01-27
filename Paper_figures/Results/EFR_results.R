library(fields)
library(ggplot2)
library(ggpubr)
library(rgdal)
library(ggnewscale)
rm(list = ls())

# Input
iso.file = "Input/ISO3166_dev.csv"
isimip.time.file = "Saves/ISIMIP_time.csv"
isimip.space.file = "Saves/ISIMIP_space.RDS"
watch.time.file = "Saves/WATCH_time.csv"
watch.space.file = "Saves/WATCH_space.RDS"
vic.time.file = "Saves/VIC_time.csv"
vic.space.file = "Saves/VIC_space.RDS"
coastline.file = "Input/ne_50m_coastline"
plot.out = "Output/EFR_results.pdf"

# Load
iso = read.csv(iso.file, stringsAsFactors = F)
isimip.time = read.csv(isimip.time.file)
watch.time = read.csv(watch.time.file)
vic.time = read.csv(vic.time.file)
isimip.space = readRDS(isimip.space.file)
watch.space = readRDS(watch.space.file)
vic.space = readRDS(vic.space.file)

coastline = readOGR(dsn=path.expand(dirname(coastline.file)), layer=basename(coastline.file))

# Setup
time = rbind(isimip.time, watch.time)
time = rbind(time, vic.time)

space = c(isimip.space, watch.space, vic.space)
names(space)

coastline.df = SpatialLinesDataFrame(coastline, coastline@data)

lons = seq(-179.75, 179.75, by = 0.5)
lats = seq(-89.75, 89.75, by = 0.5)
space.df = data.frame(lat = rep(lats, each = length(lons)), 
                             lon = rep(lons, times = length(lats)), 
                             nefr = c(space[["vic-wur_airrww_tot"]]), 
                             efr = c(space[["vic-wur_airrww_efr_tot"]]),
                             hefr = c(space[["vic-wur_airrww_hefr_tot"]]))
space.df = space.df[complete.cases(space.df),]

space.df$irr_red = space.df$efr - space.df$nefr
space.df$irr_red[space.df$irr_red > 0] = 0
space.df$irr_hred = space.df$efr - space.df$hefr
space.df$red_frac = space.df$irr_hred / space.df$irr_red
space.df$red_frac[is.na(space.df$red_frac)] = 0
space.df$red_bool[space.df$red_frac < 0.75] = 0
space.df$red_bool[space.df$red_frac >= 0.75] = 1

sel = time$model == "vic-wur" & time$variable %in% c("airrww", "airrww_efr", "airrww_hefr")
time.wide = reshape(data = time[sel,], direction = "wide", idvar = c("model", "region", "year"), timevar = c("variable"))
time.wide = time.wide[time.wide$year > 1980 & time.wide$region == 0,]
time.wide = aggregate(formula = cbind(value.airrww, value.airrww_hefr, value.airrww_efr) ~ model, data = plot.data, FUN = mean, na.rm = T)

# Calculate
## Relative reductions
(mean(time.wide$value.airrww) - mean(time.wide$value.airrww_efr)) / 
  mean(time.wide$value.airrww)
(mean(time.wide$value.airrww_hefr) - mean(time.wide$value.airrww_efr)) / 
  (mean(time.wide$value.airrww) - mean(time.wide$value.airrww_efr))

## Bar plot
bar.plot = ggplot(data = time.wide) +
  new_scale("fill") +
  geom_bar(mapping = aes(x = model, y = value.airrww * 1e-9, fill = "nefr"), stat = "identity") +
  geom_bar(mapping = aes(x = model, y = value.airrww_hefr * 1e-9, fill = "hefr"), stat = "identity") +
  geom_bar(mapping = aes(x = model, y = value.airrww_efr * 1e-9, fill = "efr"), stat = "identity") +
  scale_x_discrete(name = "") + 
  scale_y_continuous(name = bquote("Withdrawal ["*km^3*"]")) +
  scale_fill_manual(name = "", breaks = c("nefr", "hefr", "efr"), 
                    labels = c("No EFRs", "EFRs\nfor streamflow", "EFRs\nfor streamflow\nand baseflow"),
                    values = c(rgb(0.5,0.5,1), rgb(1,0.5,1), rgb(1,0.5,0.5))) +
  theme_bw() +
  ggtitle(label = "(a) Global\n     water\n     withdrawal") +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank())
plot(bar.plot)

## Map plot
map.plot = ggplot(data = space.df, mapping = aes(x = lon, y = lat)) + 
  geom_path(data = coastline.df, 
            mapping = aes(x = long, y = lat, group = group)) +
  scale_x_continuous(name = "Longitude [degrees East]", limits = c(-180, 180)) + 
  scale_y_continuous(name = "Latitude [degrees North]", limits = c(-90, 90)) +
  geom_raster(mapping = aes(fill = abs(irr_red) * 1e-9)) + 
  scale_fill_gradient(bquote("Withdrawal\nreduction ["*km^3*"]   "), low = "transparent", high = "red", 
                      trans = "log",
                      limits = c(5e-2, 3.01),
                      breaks = c(0.1, 1, 3),
                      na.value = "transparent") +
  
  new_scale("fill") +
  geom_raster(mapping = aes(fill = red_bool), alpha = 0.2) +
  scale_fill_gradient("Groundwater dependence", low = "transparent", high = "blue", limits = c(0,1)) +
  guides(fill = F) +
  theme_bw() + 
  ggtitle(label = "(b) Water withdrawal reduction due to EFRs") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot(map.plot)

## Plot
ggarrange(plotlist = list(bar.plot,map.plot), ncol = 2, nrow = 1, widths = c(1,5.5))

# Save
pdf(plot.out, width = 12, height = 6.5)
ggarrange(plotlist = list(bar.plot,map.plot), ncol = 2, nrow = 1, widths = c(1,5.5))
dev.off()
