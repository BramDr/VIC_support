library(openxlsx)
rm(list = ls())

# Input
iso.file = "Input/ISO3166_dev.csv"
pop.file1 = "Input/mpd2018.xlsx"
pop.file2 = "Input/pwt90.xlsx"
pop.file3 = "Input/Population_Data.csv"
pop.out = "Output/country_population_global.csv"

# Load
iso.data = read.csv(iso.file, stringsAsFactors = F, na.strings = "NA")
iso.data = iso.data[complete.cases(iso.data$Country_code3),]
iso.data = iso.data[,c("Country_code3", "Country_number")]

pop.data1 = read.xlsx(xlsxFile = pop.file1, sheet = 2)
pop.data2 = read.xlsx(xlsxFile = pop.file2, sheet = 3)
pop.data3 = read.csv(file = pop.file3, stringsAsFactors = F, header = T)
pop.data3 = pop.data3[1:217,]

# Setup
pop.data1$pop = pop.data1$pop * 1e3
pop.data1 = pop.data1[,c("countrycode", "year", "pop")]
pop.data1 = pop.data1[complete.cases(pop.data1),]

pop.data2$pop = pop.data2$pop * 1e6
pop.data2 = pop.data2[,c("countrycode", "year", "pop")]
pop.data2 = pop.data2[complete.cases(pop.data2),]

colnames(pop.data3) = c("countryname", "countrycode", "varname", "varcode", 
                        paste0("pop.",1960:2018))
pop.data3 = reshape(data = pop.data3, 
                    varying = c(paste0("pop.",1960:2018)),
                    direction = "long", 
                    timevar = "year",
                    sep = ".", 
                    idvar = "countrycode")
pop.data3 = pop.data3[,c("countrycode", "year", "pop")]
pop.data3[pop.data3 == ".."] = NA
pop.data3 = pop.data3[complete.cases(pop.data3),]

# Calculate
out.data = rbind(pop.data1, pop.data2, pop.data3)
out.data$year = as.numeric(out.data$year)
out.data$pop = as.numeric(out.data$pop)
out.data = aggregate(formula = pop ~ countrycode + year, data = out.data, FUN = mean, na.rm = T)
colnames(out.data) = c("Country_code3", "Year", "Pop")

out.data = merge(out.data, iso.data, by = "Country_code3")
out.data = out.data[,c("Country_number", "Year", "Pop")]

# Save
write.csv(x = out.data, file = pop.out, row.names = F)
