library(openxlsx)
library(zoo)
rm(list = ls())

# Input
iso.file <- "../../../Data/Primary/ISO3166/ISO3166_dev.csv"
gva.file1 <- "../../../Data/Primary/WorldBank/GVA_Data.csv"
gva.out <- "../../../Data/Transformed/GVA/GVA_country.csv"

# Load
iso.data <- read.csv(iso.file, stringsAsFactors = F, na.strings = "NA")
iso.data <- iso.data[complete.cases(iso.data$Country_code3), ]
iso.data <- iso.data[, c("Country_code3", "Country_number")]
gva.data <- read.csv(file = gva.file1, stringsAsFactors = F, header = T)
gva.data <- gva.data[1:217, ]

# Setup
colnames(gva.data) <- c(
  "countryname", "countrycode", "varname", "varcode",
  paste0("GvaPGdp.", 1960:2018)
)
gva.data <- reshape(
  data = gva.data,
  varying = c(paste0("GvaPGdp.", 1960:2018)),
  direction = "long",
  timevar = "year",
  sep = ".",
  idvar = "countrycode"
)
gva.data <- gva.data[, c("countrycode", "year", "GvaPGdp")]
gva.data[gva.data == ".."] <- NA
gva.data <- gva.data[complete.cases(gva.data), ]

# Calculate
out.data <- gva.data

out.data$year <- as.numeric(out.data$year)
out.data$GvaPGdp <- as.numeric(out.data$GvaPGdp)
out.data <- aggregate(formula = GvaPGdp ~ countrycode + year, data = out.data, FUN = mean, na.rm = T)
colnames(out.data) <- c("Country_code3", "Year", "GvaPGdp")

out.data <- merge(out.data, iso.data, by = "Country_code3")
out.data <- out.data[, c("Country_number", "Year", "GvaPGdp")]

# Save
dir.create(dirname(gva.out))
write.csv(x = out.data, file = gva.out, row.names = F)
