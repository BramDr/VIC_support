rm(list = ls())

# Input
aqu.file <- "../../../Data/Primary/AQUASTAT/aquastat2.csv"
eur.file <- "../../../Data/Primary/EUROSTAT/eurostat2.csv"
wwd.file <- "../../../Data/Primary/WWDR/WWDR-15_waterUse.csv"
merge.file <- "../../../Data/Transformed/Merging/mergeData.csv"
dom.out <- "../../../Data/Transformed/Domestic/domesticWithdrawal_country.csv"

# Load & Setup
# merge
merge.data <- read.csv(merge.file, stringsAsFactors = F)
merge.data$geo.time <- trimws(merge.data$geo.time)

# eurostat
eur.data <- read.csv(eur.file, sep = ";", stringsAsFactors = F)
selection <- eur.data$wat_proc == "ABS_PWS" & eur.data$wat_src == "FRW" & eur.data$unit == "M3_HAB"
eur.data <- eur.data[selection, ]
for (i in 1:nrow(eur.data)) {
  eur.data[i, 5:35] <- as.numeric(gsub("[^0-9\\.]", "", eur.data[i, 5:35]))
}

colnames(eur.data) <- c(
  "process", "source", "unit", "countrycode",
  paste0("with.", gsub(x = colnames(eur.data[5:35]), pattern = "X", replacement = ""))
)
eur.data <- reshape(
  data = eur.data,
  varying = colnames(eur.data[5:35]),
  direction = "long",
  timevar = "year",
  sep = ".",
  idvar = c("countrycode", "process")
)
eur.data <- eur.data[complete.cases(eur.data), ]

eur.data <- eur.data[, c("countrycode", "year", "with")]

# aquastat
aqu.data <- read.table(file = aqu.file, stringsAsFactors = F, header = T, sep = ";")
aqu.data <- aqu.data[, c("Area.Id", "Year", "Variable.Name", "Value")]

selection <- aqu.data$Variable.Name == "Municipal water withdrawal"
aqu.data.with <- aqu.data[selection, ]
selection <- aqu.data$Variable.Name == "Total population"
aqu.data.pop <- aqu.data[selection, ]

aqu.data <- merge(aqu.data.with, aqu.data.pop, by = c("Area.Id", "Year"))
aqu.data$With <- (aqu.data$Value.x * 1e9) / (aqu.data$Value.y * 1e3)
aqu.data <- aqu.data[complete.cases(aqu.data), ]

aqu.data <- aqu.data[, c("Area.Id", "Year", "With")]
colnames(aqu.data) <- c("countrycode", "year", "with")

# World Water Database
wwd.data <- read.table(file = wwd.file, stringsAsFactors = F, header = T, sep = ",")
wwd.data <- wwd.data[, c("Country", "Year", "domestic_m3_p_yr")]
colnames(wwd.data) <- c("countrycode", "year", "with")
wwd.data <- wwd.data[complete.cases(wwd.data), ]

# Calcualte

eur.data <- merge(merge.data, eur.data, by.x = "geo.time", by.y = "countrycode")
aqu.data <- merge(merge.data, aqu.data, by.x = "Area.Id", by.y = "countrycode")
wwd.data <- merge(merge.data, wwd.data, by.x = "Country", by.y = "countrycode")
out.data <- rbind(
  eur.data[, c("Country_number", "year", "with")],
  aqu.data[, c("Country_number", "year", "with")],
  wwd.data[, c("Country_number", "year", "with")]
)

out.data$Country_number <- as.numeric(out.data$Country_number)
out.data$year <- as.numeric(out.data$year)
out.data$with <- as.numeric(out.data$with)
out.data <- aggregate(formula = with ~ Country_number + year, data = out.data, FUN = median)
colnames(out.data) <- c("Country_number", "Year", "WithPc")

# Save
dir.create(dirname(dom.out))
write.csv(x = out.data, file = dom.out, row.names = F)
