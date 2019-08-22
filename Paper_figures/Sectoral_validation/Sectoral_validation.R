library(ggplot2)
library(ggpubr)
rm(list = ls())

# Input
val.dom.file = "Input/country_domestic_global_validation.csv"
val.ind.file = "Input/country_industrial_global_validation.csv"
val.irr.file = "Input/country_irrigation_global_validation.csv"
coef.dom.file = "Input/country_domestic_global.csv"
coef.ind.file = "Input/country_industrial_global.csv"
coef.irr.file = "Saves/country_irrigation_global.csv"
plot.out = "Output/sectoral_demand_validation.pdf"

# Load
coef.dom = read.csv(file = coef.dom.file, stringsAsFactors = F)
coef.ind = read.csv(file = coef.ind.file, stringsAsFactors = F)
coef.irr = read.csv(file = coef.irr.file, stringsAsFactors = F)
val.irr = read.csv(val.irr.file, stringsAsFactors = F)
val.dom = read.csv(val.dom.file, stringsAsFactors = F)
val.ind = read.csv(val.ind.file, stringsAsFactors = F)

# Setup
## Irrigation
val.irr = merge(val.irr, coef.irr, by = c("Country_number", "Year"));
val.irr = val.irr[val.irr$fWith > 1,]
val.irr$flWith = log(val.irr$fWith, 10)

## Domestic
val.dom$Int = 1
val.dom$Int[val.dom$Development_number == 3 & val.dom$Year > 1980] = 0.995
val.dom$Int[val.dom$Development_number == 2 & val.dom$Year > 1980] = 0.99
val.dom$Int[val.dom$Development_number == 1 & val.dom$Year > 1980] = 0.98

val.dom = merge(val.dom, coef.dom, by = "Country_number")
val.dom$flWithPc = log(10^(val.dom$lPar + (val.dom$uPar - val.dom$lPar) / 
                             (1 + exp(-val.dom$fPar * (val.dom$lGdpPc - val.dom$oPar)))) * 
                         val.dom$Int ^ (val.dom$Year - 1980), 10)
val.dom$fWithPc = 10^val.dom$flWithPc

## Industrial
val.ind$Int = 1
val.ind$Int[val.ind$OECD == 1 & val.ind$Year > 1960 & val.ind$Year < 2000] = 0.976
val.ind$Int[val.ind$OECD == 1 & val.ind$Year >= 2000] = 0.99
val.ind$Int[val.ind$OECD == 0 & val.ind$Year > 1980 & val.ind$Year < 2000] = 0.976
val.ind$Int[val.ind$OECD == 0 & val.ind$Year >= 2000] = 0.99

val.ind = merge(val.ind, coef.ind, by = "Country_number")
val.ind$fWith = val.ind$Intense * val.ind$Gva * 
  val.ind$Int ^ (val.ind$Year - val.ind$BaseYear)
val.ind$flWith = log(val.ind$fWith, 10)

# Calculate
## Irrigation
fit = lm(flWith ~ lWith, data = val.irr)
summary(fit)

lim = c(min(c(val.irr$fWith, val.irr$With) * 1e-9), 
        max(c(val.irr$fWith, val.irr$With) * 1e-9))
irr.plot = ggplot(data = val.irr,
                  mapping = aes(x = With * 1e-9,
                                y = fWith * 1e-9,
                                color = Region)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  scale_x_log10(name = " \n", limit = lim, labels = function(x)format(x, scientific = TRUE)) +
  scale_y_log10(name = bquote("Simulated water demand ["*km^3*"]"), limit = lim, labels = function(x)format(x, scientific = TRUE)) +
  scale_color_brewer(palette = "Set1") +
  ggtitle(label = "(a) Irrigation sector",
          subtitle = bquote("N: "*.(nrow(val.irr))*" - adjusted "*R^2*": "*.(round(summary(fit)$adj.r.squared, digits = 2)))) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text("")
  )

plot(irr.plot)

## Domestic
fit = lm(log(10^flWithPc * Pop, 10) ~ log(10^lWithPc * Pop, 10), data = val.dom)
summary(fit)

lim = c(min(val.dom$fWithPc * val.dom$Pop, val.dom$WithPc * val.dom$Pop) * 1e-9, 
        max(val.dom$fWithPc * val.dom$Pop, val.dom$WithPc * val.dom$Pop) * 1e-9)
dom.plot = ggplot(data = val.dom,
                  mapping = aes(x = WithPc * Pop * 1e-9,
                                y = fWithPc * Pop * 1e-9,
                                color = Region)) +
  geom_point() +
  geom_abline(mapping = aes(intercept = 0, slope = 1), lty = 2) +
  scale_x_log10(name = bquote("Reported water withdrawal ["*km^3*"]"), limits = lim, labels = function(x)format(x, scientific = TRUE)) +
  scale_y_log10(name = "", limits = lim, labels = function(x)format(x, scientific = TRUE)) +
  scale_color_brewer(palette = "Set1") +
  ggtitle(label = "(b) Domestic sector",
          subtitle = bquote("N: "*.(nrow(val.dom))*" - adjusted "*R^2*": "*.(round(summary(fit)$adj.r.squared, digits = 2)))) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text("")
  )

plot(dom.plot)

## Industrial
fit = lm(flWith ~ lWith, data = val.ind)
summary(fit)

lim = c(min(c(val.ind$fWith, val.ind$With) * 1e-9), 
        max(c(val.ind$fWith, val.ind$With) * 1e-9))
ind.plot = ggplot(data = val.ind,
                  mapping = aes(x = With * 1e-9,
                                y = fWith * 1e-9,
                                color = Region)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  scale_x_log10(name = " \n", limit = lim, labels = function(x)format(x, scientific = TRUE)) +
  scale_y_log10(name = "", limit = lim, labels = function(x)format(x, scientific = TRUE)) +
  scale_color_brewer(palette = "Set1") +
      ggtitle(label = "(c) Industrial sector",
              subtitle = bquote("N: "*.(nrow(val.ind))*" - adjusted "*R^2*": "*.(round(summary(fit)$adj.r.squared, digits = 2)))) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text("")
  )

plot(ind.plot)

# Save
pdf(plot.out, width = 12, height = 5)
ggarrange(
  plotlist = list(irr.plot, dom.plot, ind.plot),
  nrow = 1, ncol = 3, common.legend = T, legend = "bottom"
)
dev.off()
