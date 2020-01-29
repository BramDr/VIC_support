library(openxlsx)
library(zoo)
rm(list = ls())

# Input
iso.file <- "../../../Data/Primary/ISO3166/ISO3166_dev.csv"
pop.file <- "../../../Data/Transformed/Population/population_country.csv"
gdp.file <- "../../../Data/Transformed/GDP/GDP_country.csv"
gdp.out <- "../../../Data/Transformed/GDP/GDPInterpolated_country.csv"
years <- 1979:2016

# Load
iso <- read.csv(iso.file, stringsAsFactors = F, na.strings = "NA")
iso <- iso[complete.cases(iso$Country_code3), ]
pop <- read.csv(pop.file, stringsAsFactors = F)
gdp <- read.csv(gdp.file, stringsAsFactors = F)

# Setup
int.data <- data.frame(
  Country_number = rep(unique(iso$Country_number), each = length(years)),
  Year = rep(years, length(unique(iso$Country_number)))
)

int.data <- merge(int.data, gdp, by = c("Country_number", "Year"), all.x = T)
int.data <- merge(int.data, pop, by = c("Country_number", "Year"), all.x = T)
int.data <- merge(int.data, iso, by = c("Country_number"))

# Calculate
subregion.pc <- data.frame(Subregion_number = numeric(), Year = numeric(), Average = numeric())
region.pc <- data.frame(Region_number = numeric(), Year = numeric(), Average = numeric())

for (region in unique(int.data$Subregion_number)) {
  subdata <- int.data[int.data$Subregion_number == region, ]
  if (nrow(subdata) == 0) {
    next
  }

  for (year in years) {
    subdata2 <- subdata[subdata$Year == year, ]
    if (nrow(subdata2) == 0) {
      next
    }

    subregion.pc[nrow(subregion.pc) + 1, ] <- c(region, year, mean(subdata2$GdpPc, na.rm = T))
  }
}

for (region in unique(int.data$Region_number)) {
  subdata <- int.data[int.data$Region_number == region, ]
  if (nrow(subdata) == 0) {
    next
  }

  for (year in years) {
    subdata2 <- subdata[subdata$Year == year, ]
    if (nrow(subdata2) == 0) {
      next
    }

    region.pc[nrow(region.pc) + 1, ] <- c(region, year, mean(subdata2$GdpPc, na.rm = T))
  }
}

# Extrapolation if population is present
sel <- is.na(int.data$Gdp) & !is.na(int.data$Pop)
sel2 <- match(int.data$Subregion_number[sel], subregion.pc$Subregion_number)
int.data$GdpPc[sel] <- subregion.pc$Average[sel2]
int.data$Gdp[sel] <- int.data$GdpPc[sel] * int.data$Pop[sel]

sel <- is.na(int.data$Gdp) & !is.na(int.data$Pop)
sel2 <- match(int.data$Region_number[sel], region.pc$Region_number)
int.data$GdpPc[sel] <- region.pc$Average[sel2]
int.data$Gdp[sel] <- int.data$GdpPc[sel] * int.data$Pop[sel]

# Test if all countries are done, if not they do not receive any GDP
test <- aggregate(formula = Gdp ~ Country_number, data = int.data, FUN = mean, na.rm = T, na.action = na.pass)
print(sum(is.na(test$Gdp)))

# Now all counties are assigned, interpolate in time
for (code in unique(int.data$Country_number)) {
  code.data <- int.data[int.data$Country_number == code, ]
  code.data <- code.data[order(code.data$Year), ]

  if (sum(is.na(code.data$GdpPc)) == 0 && sum(is.na(code.data$Gdp)) == 0) {
    next
  }

  # enter in between years
  code.data.old <- code.data

  fill.values.GdpPc <- is.na(code.data$GdpPc)
  fill.values.Gdp <- is.na(code.data$Gdp)

  if (sum(fill.values.GdpPc) > 0) {
    gdp.cal <- rollapply(data = code.data$GdpPc, width = 5, FUN = mean, na.rm = T)
    gdp.cal[gdp.cal <= 0] <- NA
    code.data$GdpPc[fill.values.GdpPc] <- gdp.cal[fill.values.GdpPc]

    if (sum(is.na(code.data.old$GdpPc)) != sum(is.na(code.data$GdpPc))) {
      # plot(code.data$GdpPc, main = "rollapply-Pc", ylim = c(0, max(code.data$GdpPc, na.rm = T)))
      # points(code.data.old$GdpPc, col = "orange")
    }
  }
  if (sum(fill.values.Gdp) > 0) {
    gdp.cal <- rollapply(data = code.data$Gdp, width = 5, FUN = mean, na.rm = T)
    gdp.cal[gdp.cal <= 0] <- NA
    code.data$Gdp[fill.values.Gdp] <- gdp.cal[fill.values.Gdp]

    if (sum(is.na(code.data.old$Gdp)) != sum(is.na(code.data$Gdp))) {
      # plot(code.data$Gdp, main = "rollapply-Tot", ylim = c(0, max(code.data$Gdp, na.rm = T)))
      # points(code.data.old$Gdp, col = "orange")
    }
  }

  # enter in before/after years
  code.data.old <- code.data

  fill.values.GdpPc <- is.na(code.data$GdpPc)
  fill.values.Gdp <- is.na(code.data$Gdp)

  if (sum(fill.values.GdpPc) > 0) {
    gdp.idx <- which(!is.na(code.data$GdpPc))
    gdp.idx <- gdp.idx[1:min(5, length(gdp.idx))]
    gdp.cal <- mean(code.data$GdpPc[gdp.idx])

    fill.idx <- which(is.na(code.data$GdpPc))
    fill.idx <- fill.idx[fill.idx < min(gdp.idx)]

    code.data$GdpPc[fill.idx] <- gdp.cal

    gdp.idx <- which(!is.na(code.data$GdpPc))
    gdp.idx <- gdp.idx[max((length(gdp.idx) - 5), 1):length(gdp.idx)]
    gdp.cal <- mean(code.data$GdpPc[gdp.idx])

    fill.idx <- which(is.na(code.data$GdpPc))
    fill.idx <- fill.idx[fill.idx > max(gdp.idx)]

    code.data$GdpPc[fill.idx] <- gdp.cal

    if (sum(is.na(code.data.old$GdpPc)) != sum(is.na(code.data$GdpPc))) {
      # plot(code.data$GdpPc, main = "before/after-Pc", ylim = c(0, max(code.data$GdpPc, na.rm = T)))
      # points(code.data.old$GdpPc, col = "orange")
    }
  }

  if (sum(fill.values.Gdp) > 0) {
    gdp.idx <- which(!is.na(code.data$Gdp))
    gdp.idx <- gdp.idx[1:min(5, length(gdp.idx))]
    gdp.cal <- mean(code.data$Gdp[gdp.idx])

    fill.idx <- which(is.na(code.data$Gdp))
    fill.idx <- fill.idx[fill.idx < min(gdp.idx)]

    code.data$Gdp[fill.idx] <- gdp.cal

    gdp.idx <- which(!is.na(code.data$Gdp))
    gdp.idx <- gdp.idx[max((length(gdp.idx) - 5), 1):length(gdp.idx)]
    gdp.cal <- mean(code.data$Gdp[gdp.idx])

    fill.idx <- which(is.na(code.data$Gdp))
    fill.idx <- fill.idx[fill.idx > max(gdp.idx)]

    code.data$Gdp[fill.idx] <- gdp.cal

    if (sum(is.na(code.data.old$Gdp)) != sum(is.na(code.data$Gdp))) {
      # plot(code.data$Gdp, main = "before/after-Tot", ylim = c(0, max(code.data$Gdp, na.rm = T)))
      # points(code.data.old$Gdp, col = "orange")
    }
  }

  sel <- as.numeric(rownames(code.data))
  int.data[sel, ] <- code.data
}

int.data$Gdp[is.na(int.data$Gdp)] <- 0
int.data$GdpPc[is.na(int.data$GdpPc)] <- 0
int.data <- int.data[, c("Country_number", "Year", "Gdp", "GdpPc")]

# Save
dir.create(dirname(gdp.out))
write.csv(x = int.data, file = gdp.out, row.names = F)
