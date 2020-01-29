rm(list = ls())

# Input
iso.file <- "../../../Data/Primary/ISO3166/ISO3166_dev.csv"
aqu.file <- "../../../Data/Primary/AQUASTAT/aquastat2.csv"
wwd.file <- "../../../Data/Primary/WWDR/WWDR-15_waterUse.csv"
eur.file <- "../../../Data/Primary/EUROSTAT/countryCodes.csv"
merge.out <- "../../../Data/Transformed/Merging/mergeData.csv"

# Load
iso.data <- read.csv(iso.file, stringsAsFactors = F, na.strings = "NA")
iso.data <- iso.data[complete.cases(iso.data$Country_code3), ]
iso.data <- iso.data[, c("Country_code3", "Country_number", "Country")]
aqu.data <- read.table(file = aqu.file, header = TRUE, sep = ";", stringsAsFactors = F)
aqu.data <- aqu.data[, c("Area", "Area.Id")]
aqu.data <- unique(aqu.data)
wwd.data <- read.table(file = wwd.file, header = TRUE, sep = ",", stringsAsFactors = F)
wwd.data <- wwd.data[, c("Region", "Country")]
eur.data <- read.csv(eur.file, header = TRUE, sep = ";", stringsAsFactors = F)
eur.data <- eur.data[, c("Code.", "English.")]
eur.data <- unique(eur.data)

# Calculate
mergedata <- data.frame(
  Country_number = iso.data$Country_number,
  Country_code3 = iso.data$Country_code3
)
mergedata$Area <- NA
mergedata$Area.Id <- NA
mergedata$Country <- NA
mergedata$geo.time <- NA

for (i in 1:nrow(iso.data)) {
  name <- as.character(iso.data$Country[i])

  for (j in 1:nrow(aqu.data)) {
    area <- aqu.data$Area[j]
    area.id <- aqu.data$Area.Id[j]

    area2 <- area
    if (area == "Occupied Palestinian Territory") {
      area2 <- "Palestine"
    } else if (area == "Congo, Democratic Republic") {
      area2 <- "Democratic Republic of the Congo"
    } else if (area == "Congo, Republic of") {
      area2 <- "Congo"
    } else if (area == "Brunei") {
      area2 <- "Brunei Darussalam"
    } else if (area == "Cabo Verde") {
      area2 <- "Cape Verde"
    } else if (area == "Côte d'Ivoire") {
      area2 <- "Cote d'Ivoire"
    } else if (area == "Eswatini") {
      area2 <- "Swaziland"
    } else if (area == "Holy See") {
      area2 <- "Holy See (Vatican City)"
    } else if (area == "Bolivia (Plurinational State of)") {
      area2 <- "Bolivia"
    } else if (area == "United States of America") {
      area2 <- "United States"
    } else if (area == "Venezuela (Bolivarian Republic of)") {
      area2 <- "Venezuela"
    } else if (area == "Democratic People's Republic of Korea") {
      area2 <- "Korea, Democratic People's Republic of"
    } else if (area == "Micronesia (Federated States of)") {
      area2 <- "Micronesia, Federated States of"
    } else if (area == "Libya") {
      area2 <- "Libyan Arab Jamahiriya"
    } else if (area == "Myanmar") {
      area2 <- "Burma"
    } else if (area == "Republic of Korea") {
      area2 <- "Korea, Republic of"
    } else if (area == "Czechia") {
      area2 <- "Czech Republic"
    } else if (area == "South Sudan") {
      # not found
    } else if (area == "Russian Federation") {
      area2 <- "Russia"
    }

    if (area2 == name) {
      mergedata$Area.Id[i] <- area.id
      mergedata$Area[i] <- area
    }
  }
  for (j in 1:nrow(wwd.data)) {
    country <- as.character(wwd.data$Country[j])

    country2 <- country
    if (country == "Côte d’Ivoire") {
      country2 <- "Cote d'Ivoire"
    } else if (country == "Congo, Democratic Republic") {
      country2 <- "Democratic Republic of the Congo"
    } else if (country == "Laos") {
      country2 <- "Lao People's Democratic Republic"
    } else if (country == "Korea Democratic People’s") {
      country2 <- "Korea, Democratic People's Republic of"
    } else if (country == "Congo, Republic of") {
      country2 <- "Congo"
    } else if (country == "Sudan and South Sudan") {
      country2 <- "Sudan"
    } else if (country == "Tanzania, Republic of") {
      country2 <- "United Republic of Tanzania"
    } else if (country == "United States of America") {
      country2 <- "United States"
    } else if (country == "Korea Republic") {
      country2 <- "Korea, Republic of"
    } else if (country == "Palestine Territory, Occupied") {
      country2 <- "Palestine"
    } else if (country == "Libya") {
      country2 <- "Libyan Arab Jamahiriya"
    } else if (country == "St. Lucia") {
      country2 <- "Saint Lucia"
    } else if (country == "St. Vincent and the Grenadines") {
      country2 <- "Saint Vincent and the Grenadines"
    } else if (country == "Brunei") {
      country2 <- "Brunei Darussalam"
    } else if (country == "Iran") {
      country2 <- "Iran (Islamic Republic of)"
    } else if (country == "Myanmar") {
      country2 <- "Burma"
    } else if (country == "Vietnam") {
      country2 <- "Viet Nam"
    } else if (country == "Syria") {
      country2 <- "Syrian Arab Republic"
    } else if (country == "Moldova") {
      country2 <- "Republic of Moldova"
    } else if (country == "Macedonia") {
      country2 <- "The former Yugoslav Republic of Macedonia"
    } else if (country == "Russian Federation") {
      country2 <- "Russia"
    }

    if (country2 == name) {
      mergedata$Country[i] <- country
    }
  }
  for (j in 1:nrow(eur.data)) {
    country <- trimws(as.character(eur.data$English.[j]))
    country.id <- as.character(eur.data$Code.[j])

    country2 <- country
    if (country == "Czechia") {
      country2 <- "Czech Republic"
    }

    if (country2 == name) {
      mergedata$geo.time[i] <- country.id
    }
  }
}

test <- aqu.data[!aqu.data$Area.Id %in% mergedata$Area.Id, ]
test2 <- wwd.data[!wwd.data$Country %in% mergedata$Country, ]
test3 <- eur.data[!eur.data$Code. %in% mergedata$geo.time, ]

# Save
dir.create(dirname(merge.out))
write.csv(x = mergedata[, c("Country_number", "Country_code3", "Area.Id", "Area", "Country", "geo.time")], file = merge.out, row.names = F)
