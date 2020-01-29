library(ggplot2)
rm(list = ls())

# Input
iso.file <- "../../../../Data/Primary/ISO3166/ISO3166_dev.csv"
fit.file <- "Saves/industrial_country_fitting.csv"
fit.out <- "Saves/industrialCoef_country.csv"

# Load
iso <- read.csv(file = iso.file, stringsAsFactors = F)

# Setup
itirations <- 100

# Calculate & save
for (itiration in 1:itirations) {
  fit.file.new <- gsub(x = fit.file, pattern = ".csv", replacement = paste0("_", itiration, ".csv"))
  data.fit <- read.csv(fit.file.new, stringsAsFactors = F)

  ## Country-year fit
  data.agg.cy <- data.fit
  data.agg.cy$cyIntense <- data.agg.cy$With / data.agg.cy$Gva
  data.agg.cy <- aggregate(formula = cbind(cyIntense) ~ Country_number + Year, data = data.agg.cy, FUN = median)
  data.agg.cy <- data.agg.cy[, c("Country_number", "Year", "cyIntense")]

  ## Country fit
  data.agg.c <- data.fit
  data.agg.c$cIntense <- data.agg.c$With / data.agg.c$Gva
  data.agg.c$cBaseYear <- data.agg.c$Year
  data.agg.c <- aggregate(formula = cbind(cIntense, cBaseYear) ~ Country_number, data = data.agg.c, FUN = median)
  data.agg.c <- data.agg.c[, c("Country_number", "cIntense", "cBaseYear")]

  ## Subregion fit
  data.agg.sr <- data.fit
  data.agg.sr$srIntense <- data.agg.sr$With / data.agg.sr$Gva
  data.agg.sr$srBaseYear <- data.agg.sr$Year
  data.agg.sr <- aggregate(formula = cbind(srIntense, srBaseYear) ~ Subregion_number, data = data.agg.sr, FUN = median)
  data.agg.sr <- data.agg.sr[, c("Subregion_number", "srIntense", "srBaseYear")]

  ## Region fit
  data.agg.r <- data.fit
  data.agg.r$rIntense <- data.agg.r$With / data.agg.r$Gva
  data.agg.r$rBaseYear <- data.agg.r$Year
  data.agg.r <- aggregate(formula = cbind(rIntense, rBaseYear) ~ Region_number, data = data.agg.r, FUN = median)
  data.agg.r <- data.agg.r[, c("Region_number", "rIntense", "rBaseYear")]

  coef <- iso[, c("Country_number", "Subregion_number", "Region_number", "OECD")]
  coef$Intense <- NA
  coef$BaseYear <- NA
  c.subregion <- c()
  c.region <- c()
  c.global <- c()
  for (i in 1:nrow(coef)) {
    if (coef$Country_number[i] %in% data.agg.c$Country_number) {
      j <- which(data.agg.c$Country_number == coef$Country_number[i])
      coef$Intense[i] <- data.agg.c$cIntense[j]
      coef$BaseYear[i] <- data.agg.c$cBaseYear[j]
    } else if (coef$Subregion_number[i] %in% data.agg.sr$Subregion_number) {
      j <- which(data.agg.sr$Subregion_number == coef$Subregion_number[i])
      coef$Intense[i] <- data.agg.sr$srIntense[j]
      coef$BaseYear[i] <- data.agg.sr$srBaseYear[j]
      c.subregion <- c(c.subregion, coef$Country_number[i])
    } else if (coef$Region_number[i] %in% data.agg.r$Region_number) {
      j <- which(data.agg.r$Region_number == coef$Region_number[i])
      coef$Intense[i] <- data.agg.r$rIntense[j]
      coef$BaseYear[i] <- data.agg.r$rBaseYear[j]
      c.region <- c(c.region, coef$Country_number[i])
    } else {
      j <- 1
      coef$Intense[i] <- mean(data.agg.cy$cyIntense, na.rm = T)
      coef$BaseYear[i] <- mean(data.agg.cy$Year, na.rm = T)
      c.global <- c(c.global, coef$Country_number[i])
    }
  }

  ## Save
  fit.out.new <- gsub(x = fit.out, pattern = ".csv", replacement = paste0("_", itiration, ".csv"))

  dir.create(dirname(fit.out.new))
  write.csv(x = coef, file = fit.out.new, row.names = F)
}
