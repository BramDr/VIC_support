rm(list = ls())

# Input
aqu.file <- "../../../Data/Primary/AQUASTAT/aquastat2.csv"
eur.file <- "../../../Data/Primary/EUROSTAT/eurostat2.csv"
wwd.file <- "../../../Data/Primary/WWDR/WWDR-15_waterUse.csv"
merge.file <- "../../../Data/Transformed/Merging/mergeData.csv"
ind.out <- "../../../Data/Transformed/Industrial/industrialWithdrawal_country.csv"

# Load & Setup
# Merge
merge.data <- read.csv(merge.file, stringsAsFactors = F)
merge.data$geo.time <- trimws(merge.data$geo.time)

# eurostat
eur.data <- read.csv(eur.file, sep = ";", stringsAsFactors = F)
eur.data[eur.data == ": "] <- NA
for (i in 1:nrow(eur.data)) {
  eur.data[i, 5:35] <- as.numeric(gsub("[^0-9\\.]", "", eur.data[i, 5:35]))
}

for (i in unique(eur.data$geo.time)) {
  selection <- (eur.data$wat_proc == "ABS_IND" | eur.data$wat_proc == "ABS_IND_CL" | eur.data$wat_proc == "ABS_ELC_CL") & eur.data$wat_src == "FRW" & eur.data$unit == "MIO_M3" & eur.data$geo.time == i
  eur.data.sel <- eur.data[selection, ]
  if (nrow(eur.data.sel) == 0) {
    next
  }

  eur.data.sel <- rbind(eur.data.sel, eur.data.sel[1, ])
  eur.data.sel$wat_proc[nrow(eur.data.sel)] <- "ABS_IND_TOT"

  if (nrow(eur.data.sel) > 2) {
    data.sel <- as.matrix(eur.data.sel[1:(nrow(eur.data.sel) - 1), 5:ncol(eur.data.sel)])
    data.sel <- apply(data.sel, c(1, 2), as.numeric)
    eur.data.sel[nrow(eur.data.sel), 5:ncol(eur.data.sel)] <- apply(X = data.sel, MARGIN = c(2), FUN = sum, na.rm = T)
    eur.data.sel[eur.data.sel == 0] <- NA
  }

  eur.data <- rbind(eur.data, eur.data.sel[nrow(eur.data.sel), ])
}
selection <- eur.data$wat_proc == "ABS_IND_TOT" & eur.data$wat_src == "FRW" & eur.data$unit == "MIO_M3"
eur.data <- eur.data[selection, ]

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

eur.data$with <- as.numeric(eur.data$with) * 1e6
eur.data <- eur.data[, c("countrycode", "year", "with")]

# aquastat
aqu.data <- read.table(file = aqu.file, stringsAsFactors = F, header = T, sep = ";")
aqu.data <- aqu.data[, c("Area.Id", "Year", "Variable.Name", "Value")]

selection <- aqu.data$Variable.Name == "Industrial water withdrawal"
aqu.data <- aqu.data[selection, ]
aqu.data <- aqu.data[, c("Area.Id", "Year", "Value")]
aqu.data <- aqu.data[complete.cases(aqu.data), ]

aqu.data$Value <- aqu.data$Value * 1e9
colnames(aqu.data) <- c("countrycode", "year", "with")

# World Water Database
wwd.data <- read.table(file = wwd.file, stringsAsFactors = F, header = T, sep = ",")
wwd.data$industrial <- wwd.data$total_freshwater_km3_yr * 1e9 * (wwd.data$industrial / 100)
wwd.data <- wwd.data[, c("Country", "Year", "industrial")]
colnames(wwd.data) <- c("countrycode", "year", "with")
wwd.data <- wwd.data[complete.cases(wwd.data), ]

# Calculate
eur.data <- merge(merge.data, eur.data, by.x = "geo.time", by.y = "countrycode")
aqu.data <- merge(merge.data, aqu.data, by.x = "Area.Id", by.y = "countrycode")
wwd.data <- merge(merge.data, wwd.data, by.x = "Country", by.y = "countrycode")
out.data <- rbind(eur.data[, c("Country_number", "year", "with")], aqu.data[, c("Country_number", "year", "with")], wwd.data[, c("Country_number", "year", "with")])

out.data$Country_number <- as.numeric(out.data$Country_number)
out.data$year <- as.numeric(out.data$year)
out.data$with <- as.numeric(out.data$with)
out.data <- aggregate(formula = with ~ Country_number + year, data = out.data, FUN = median)
colnames(out.data) <- c("Country_number", "Year", "With")

# Save
dir.create(dirname(ind.out))
write.csv(x = out.data, file = ind.out, row.names = F)
