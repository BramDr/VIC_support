library(ggplot2)
rm(list = ls())

# Input
iso.file <- "../../../../Data/Primary/ISO3166/ISO3166_dev.csv"
pop.file <- "../../../../Data/Transformed/Population/population_country.csv"
with.file <- "../../../../Data/Transformed/Domestic/domesticWithdrawal_country.csv"
gdp.file <- "../../../../Data/Transformed/GDP/GDP_country.csv"
fit.out <- "Saves/domestic_country_fitting.csv"
val.out <- "Saves/domestic_country_validation.csv"

# Load
iso <- read.csv(file = iso.file, stringsAsFactors = F)
gdp <- read.csv(gdp.file, stringsAsFactors = F)
pop <- read.csv(pop.file, stringsAsFactors = F)
with <- read.csv(with.file, stringsAsFactors = F)

# Setup
set.seed(23021992)
data <- merge(gdp, with, by = c("Country_number", "Year"))
data <- merge(data, pop, by = c("Country_number", "Year"))
data <- merge(data, iso, by = c("Country_number"))

data <- data[data$WithPc > 1, ]
data <- data[data$GdpPc > 1, ]
data$lWithPc <- log(data$WithPc, 10)
data$lGdpPc <- log(data$GdpPc, 10)

data$row <- 1:nrow(data)

# Calculate
sel <- c()
isel <- c()
for (cn in unique(data$Subregion_number)) {
  sd <- data[data$Subregion_number == cn, ]

  if (nrow(sd) == 1) {
    sel <- c(sel, sd$row)
    isel <- c(isel, sd$row)
  } else {
    ss <- sample(x = 1:nrow(sd), size = ceiling(nrow(sd) / 2))
    iss <- 1:nrow(sd)
    iss <- iss[!iss %in% ss]

    sel <- c(sel, sd$row[ss])
    isel <- c(isel, sd$row[iss])
  }
}
sum(sel %in% isel)

data.fit <- data[sel, ]
data.validation <- data[isel, ]

# Save
dir.create(dirname(fit.out))
write.csv(x = data.fit, file = fit.out, row.names = F)
dir.create(dirname(val.out))
write.csv(x = data.validation, file = val.out, row.names = F)
