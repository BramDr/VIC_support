library(openxlsx)
library(plyr)
rm(list = ls())

# Input
iso.file = "Input/ISO3166_dev.csv"
gdp.file1 = "Input/mpd2018.xlsx"
gdp.file2 = "Input/pwt90.xlsx"
gdp.file3 = "Input/GDP2011_Data.csv"
gdp.out = "Output/country_GDP_global.csv"

# Load
iso.data = read.csv(iso.file, stringsAsFactors = F, na.strings = "NA")
iso.data = iso.data[complete.cases(iso.data$Country_code3),]
iso.data = iso.data[,c("Country_code3", "Country_number")]

gdp.data1 = read.xlsx(xlsxFile = gdp.file1, sheet = 2)
gdp.data2 = read.xlsx(xlsxFile = gdp.file2, sheet = 3)
gdp.data3 = read.csv(file = gdp.file3, stringsAsFactors = F, header = T)
gdp.data3 = gdp.data3[1:217,]

# Setup
gdp.data1$GdpPc = gdp.data1$rgdpnapc
gdp.data1$Gdp = gdp.data1$rgdpnapc * gdp.data1$pop * 1e3
gdp.data1 = gdp.data1[,c("countrycode", "year", "Gdp", "GdpPc")]
gdp.data1 = gdp.data1[complete.cases(gdp.data1),]

gdp.data2$GdpPc = (gdp.data2$rgdpna * 1e6) / (gdp.data2$pop * 1e6)
gdp.data2$Gdp = gdp.data2$rgdpna * 1e6
gdp.data2 = gdp.data2[,c("countrycode", "year", "Gdp", "GdpPc")]
gdp.data2 = gdp.data2[complete.cases(gdp.data2),]

gdp.data3.gdppc = gdp.data3[gdp.data3$Series.Code == "NY.GDP.PCAP.PP.KD",]
colnames(gdp.data3.gdppc) = c("countryname", "countrycode", "varname", "varcode", 
                        paste0("GdpPc.",1960:2018))
gdp.data3.gdppc = reshape(data = gdp.data3.gdppc, 
                    varying = c(paste0("GdpPc.",1960:2018)),
                    direction = "long", 
                    timevar = "year",
                    sep = ".", 
                    idvar = "countrycode")

gdp.data3.gdp = gdp.data3[gdp.data3$Series.Code == "NY.GDP.MKTP.PP.KD",]
colnames(gdp.data3.gdp) = c("countryname", "countrycode", "varname", "varcode", 
                        paste0("Gdp.",1960:2018))
gdp.data3.gdp = reshape(data = gdp.data3.gdp, 
                    varying = c(paste0("Gdp.",1960:2018)),
                    direction = "long", 
                    timevar = "year",
                    sep = ".", 
                    idvar = "countrycode")

gdp.data3 = join(gdp.data3.gdppc, gdp.data3.gdp, by = c("countrycode", "year"))
gdp.data3 = gdp.data3[,c("countrycode", "year", "Gdp", "GdpPc")]
gdp.data3[gdp.data3 == ".."] = NA
gdp.data3 = gdp.data3[complete.cases(gdp.data3),]

# Calculate
out.data = rbind(gdp.data1, gdp.data2, gdp.data3)
out.data$year = as.numeric(out.data$year)
out.data$Gdp = as.numeric(out.data$Gdp)
out.data$GdpPc = as.numeric(out.data$GdpPc)
out.data = aggregate(formula = cbind(Gdp, GdpPc) ~ countrycode + year, data = out.data, FUN = mean, na.rm = T)
colnames(out.data) = c("Country_code3", "Year", "Gdp", "GdpPc")

out.data = join(out.data, iso.data, by = "Country_code3")
out.data = out.data[,c("Country_number", "Year", "Gdp", "GdpPc")]

# Save
write.csv(x = out.data, file = gdp.out, row.names = F)
