library(fields)
library(ggplot2)
library(ggpubr)
rm(list = ls())

# Input
dom.dir = "./Input/"
ind.dir = "./Input/"
dom.total.file = "./Input/domestic_demand_global_total.RDS"
ind.total.file = "./Input/industrial_demand_global_total.RDS"
plot.out = "./Output/sectoral_split_validation.pdf"

# Load
dom.files = list.files(dom.dir, pattern = "domestic", full.names = T)
ind.files = list.files(ind.dir, pattern = "industrial", full.names = T)
dom.total = readRDS(dom.total.file)
ind.total = readRDS(ind.total.file)

# Setup
years = 1979:2016
dom.data = data.frame(year = years)
ind.data = data.frame(year = years)
for(i in 1:length(dom.files)){
  dom.file = dom.files[i]
  dom = readRDS(dom.file)
  
  dom.years = apply(X = dom, MARGIN = 3, FUN = sum, na.rm = T) * 1e-9
  dom.data = cbind(dom.data, dom.years)
  colnames(dom.data)[ncol(dom.data)] = i
}

for(i in 1:length(ind.files)){
  ind.file = ind.files[i]
  ind = readRDS(ind.file)
  
  ind.years = apply(X = ind, MARGIN = 3, FUN = sum, na.rm = T) * 1e-9
  ind.data = cbind(ind.data, ind.years)
  colnames(ind.data)[ncol(ind.data)] = i
}

dom.total.data = apply(X = dom.total, MARGIN = 3, FUN = sum, na.rm = T) * 1e-9
ind.total.data = apply(X = ind.total, MARGIN = 3, FUN = sum, na.rm = T) * 1e-9

# Calculate
#dom.mean = apply(X = dom.data[,2:ncol(dom.data)], MARGIN = 1, mean)
dom.mean = dom.total.data
dom.min = apply(X = dom.data[,2:ncol(dom.data)], MARGIN = 1, sd)
dom.max = apply(X = dom.data[,2:ncol(dom.data)], MARGIN = 1, sd)
dom.min = dom.mean - dom.min
dom.max = dom.mean + dom.max

#ind.mean = apply(X = ind.data[,2:ncol(ind.data)], MARGIN = 1, mean)
ind.mean = ind.total.data
ind.min = apply(X = ind.data[,2:ncol(ind.data)], MARGIN = 1, sd)
ind.max = apply(X = ind.data[,2:ncol(ind.data)], MARGIN = 1, sd)
ind.min = ind.mean - ind.min
ind.max = ind.mean + ind.max

plot.data = data.frame(year = years, 
                       dom.total = dom.mean, dom.min = dom.min, dom.max = dom.max,
                       ind.total = ind.mean, ind.min = ind.min, ind.max = ind.max)

mean((dom.mean - dom.min) / dom.mean)
mean((ind.mean - ind.min) / ind.mean)

dom.plot = ggplot(data = plot.data) + 
  geom_line(mapping = aes(x = year, y = dom.total, lty = "Total dataset")) + 
  geom_ribbon(mapping = aes(x = year, ymin = dom.min, ymax = dom.max, fill = "Uncertainty band"), alpha = 0.3) +
  scale_x_continuous(name = "Year") + 
  scale_y_continuous(name = bquote("Demand ["*km^3*"]"), limits = c(0,NA)) + 
  scale_fill_manual("",values="grey12") + 
  scale_linetype_manual("", values = 1) + 
  ggtitle("(a) Domestic") + 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text("")
  )

plot(dom.plot)

ind.plot = ggplot(data = plot.data) + 
  geom_line(mapping = aes(x = year, y = ind.total, lty = "Total dataset")) + 
  geom_ribbon(mapping = aes(x = year, ymin = ind.min, ymax = ind.max, fill = "Uncertainty band"), alpha = 0.3) +
  scale_x_continuous(name = "Year") + 
  scale_y_continuous(name = " ", limits = c(0,NA)) + 
  scale_fill_manual("",values="grey12") + 
  scale_linetype_manual("", values = 1) + 
  ggtitle("(b) Industrial") + 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text("")
  )

plot(ind.plot)

ggarrange(
  plotlist = list(dom.plot, ind.plot),
  nrow = 1, ncol = 2, common.legend = T, legend = "bottom"
)

pdf(plot.out, width = 8, height = 4)
ggarrange(
  plotlist = list(dom.plot, ind.plot),
  nrow = 1, ncol = 2, common.legend = T, legend = "bottom"
)
dev.off()
