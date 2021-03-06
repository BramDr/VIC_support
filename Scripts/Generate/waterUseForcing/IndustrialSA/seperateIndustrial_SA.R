library(ggplot2)
rm(list = ls())

# Input
iso.file <- "../../../../Data/Primary/ISO3166/ISO3166_dev.csv"
pop.file <- "../../../../Data/Transformed/Population/population_country.csv"
with.file <- "../../../../Data/Transformed/Industrial/industrialWithdrawal_country.csv"
gva.file <- "../../../../Data/Transformed/GVA/GVA_country.csv"
gdp.file <- "../../../../Data/Transformed/GDP/GDP_country.csv"
fit.out <- "Saves/industrial_country_fitting.csv"
total.out <- "Saves/industrial_country_fitting_total.csv"

# load
iso <- read.csv(file = iso.file, stringsAsFactors = F)
gdp <- read.csv(gdp.file, stringsAsFactors = F)
gva <- read.csv(gva.file, stringsAsFactors = F)
pop <- read.csv(pop.file, stringsAsFactors = F)
with <- read.csv(with.file, stringsAsFactors = F)

gva <- merge(gva, gdp, by = c("Country_number", "Year"))
gva$Gva <- gva$Gdp * (gva$GvaPGdp / 100)
gva <- gva[, c("Country_number", "Year", "Gva")]

# Setup
set.seed(23021992)
itirations <- 100
data <- merge(gva, with, by = c("Country_number", "Year"))
data <- merge(data, pop, by = c("Country_number", "Year"))
data <- merge(data, iso, by = c("Country_number"))

data <- data[data$With > 1, ]
data <- data[data$Gva > 1, ]
data$lWith <- log(data$With, 10)
data$lGva <- log(data$Gva, 10)

data$row <- 1:nrow(data)

# Calculate & save
for (itiration in 1:itirations) {
  sel <- c()
  isel <- c()
  for (cn in unique(data$Country_number)) {
    sd <- data[data$Country_number == cn, ]

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

  fit.out.new <- gsub(x = fit.out, pattern = ".csv", replacement = paste0("_", itiration, ".csv"))

  dir.create(dirname(fit.out.new))
  write.csv(x = data.fit, file = fit.out.new, row.names = F)
}

# Save
dir.create(dirname(total.out))
write.csv(x = data, file = total.out, row.names = F)
