library(ncdf4)
rm(list = ls())

# Input
force.dir <- "../../../Data/WOFOST/Forcing/global/"
out.file <- "../../../Data/WOFOST/Configuration/global/SA/force_config_global.txt"

# Load
force.files <- list.files(force.dir, pattern = "WFDEI.nc", full.names = T, recursive = T)
tasMin.files <- grep(x = force.files, pattern = "tasMin_", value = T)
tasMax.files <- grep(x = force.files, pattern = "tasMax_", value = T)
swdown.files <- grep(x = force.files, pattern = "swdown_", value = T)
pr.files <- grep(x = force.files, pattern = "pr_", value = T)
wind.files <- grep(x = force.files, pattern = "wind_", value = T)
vp.files <- grep(x = force.files, pattern = "vp_", value = T)

# Setup
force.years <- data.frame(start = numeric(), end = numeric())
for (file in pr.files) {
  nc <- nc_open(file)
  dates <- as.Date(nc$dim$time$vals / 24, origin = paste0("1979-01-01"))
  nc_close(nc)
  dates <- as.Date(paste0(format.Date(dates, "%Y-"), format.Date(dates, "%m-"), format.Date(dates, "%d")))

  force.years[nrow(force.years) + 1, ] <- c(min(as.numeric(format.Date(dates, "%Y"))), max(as.numeric(format.Date(dates, "%Y"))))
}

# Calculate
text <- c()
for (i in 1:nrow(force.years)) {
  line <- paste0(getwd(), "/", " ", force.years$start[i], " ", force.years$end[i])
  text <- c(text, line)
  line <- paste0(tasMin.files[i], " TMIN ", " tas ", " C", collapse = "")
  text <- c(text, line)
  line <- paste0(tasMax.files[i], " TMAX ", " tas ", " C", collapse = "")
  text <- c(text, line)
  line <- paste0(swdown.files[i], " RADIATION ", " swdown ", " Wm-2", collapse = "")
  text <- c(text, line)
  line <- paste0(pr.files[i], " RAIN ", " pr ", " mmday-1", collapse = "")
  text <- c(text, line)
  line <- paste0(wind.files[i], " WINDSPEED ", " wind ", " ms-1", collapse = "")
  text <- c(text, line)
  line <- paste0(vp.files[i], " VAPOUR ", " vp ", " kPa", collapse = "")
  text <- c(text, line)
  text <- c(text, " ")
}
header <- paste0("
** WOFOST FORCING CONFIGURATION for use with WOFOST-C Version 7.0, October 2019
** For use in VIC-WOFOST
")
text <- c(header, text)

# Save
dir.create(dirname(out.file))
writeLines(text = text, con = out.file)
