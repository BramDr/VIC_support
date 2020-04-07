library(fields)
library(ncdf4)
library(zoo)
rm(list = ls())

# Input
in.efr <- "../../../Data/Transformed/VIC/fluxes_NAT.1979-01.nc"
out.dir <- "../../../Data/VIC/Forcing/global/"
days.per.month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
years <- 1979:2016

# Load
nc <- nc_open(in.efr)
dates <- as.Date(nc$dim$time$vals, "0000-12-30")
dates.months <- as.numeric(format.Date(dates, "%m"))
z <- which(dates >= as.Date("1980-01-01") & dates <= as.Date("2010-01-01"))

efr.dis <- ncvar_get(nc, "OUT_DISCHARGE", start = c(1, 1, min(z)), count = c(-1, -1, length(z)))
efr.base <- ncvar_get(nc, "OUT_BASEFLOW", start = c(1, 1, min(z)), count = c(-1, -1, length(z)))
dates <- dates[z]
dates.months <- dates.months[z]
nc_close(nc)
print("Loaded")

# Setup
linterp <- function(monthly.data, daily.months) {
  daily.data <- rep(NA, length(daily.months))

  daily.rle <- rle(daily.months)
  for (i in 1:length(daily.months)) {
    month <- daily.months[i]
    month.len <- daily.rle$lengths[daily.rle$values == month]

    if (month == 1) {
      days.past <- 0
    } else {
      days.past <- cumsum(daily.rle$lengths[daily.rle$values <= month - 1])[month - 1]
    }
    days.done <- i - days.past

    if (month == 1) {
      prev.value <- monthly.data[12]
    } else {
      prev.value <- monthly.data[month - 1]
    }
    next.value <- monthly.data[month]

    daily.data[i] <- prev.value + (next.value - prev.value) * (days.done / month.len)
  }

  daily.data <- c(daily.data[(length(daily.data) - 15):length(daily.data)], daily.data[1:(length(daily.data) - 16)])
  return(daily.data)
}

res <- 0.5
lons <- seq(
  from = -180 + res / 2,
  to = 180 - res / 2,
  by = res
)
lats <- seq(
  from = -90 + res / 2,
  to = 90 - res / 2,
  by = res
)

lon.dim <- ncdim_def(
  name = "lon",
  units = "degrees_east",
  vals = lons,
  longname = "longitude of cell centre"
)
lat.dim <- ncdim_def(
  name = "lat",
  units = "degrees_north",
  vals = lats,
  longname = "latitude of cell centre"
)

# Calculate and save
efr.dis2 <- array(NA, dim = c(dim(efr.dis)[1:2], 12))
efr.base2 <- array(NA, dim = c(dim(efr.base)[1:2], 12))
for (x in 1:dim(efr.dis2)[1]) {
  print(x)
  for (y in 1:dim(efr.dis2)[2]) {
    if (is.na(efr.dis[x, y, 1])) {
      next
    }

    dis <- efr.dis[x, y, ]
    base <- efr.base[x, y, ]

    dis.agg <- aggregate(x = dis, by = list(dates.months), FUN = mean)[, 2]
    base.agg <- aggregate(x = base, by = list(dates.months), FUN = mean)[, 2]

    efr <- dis.agg
    sel <- dis.agg <= 0.4 * mean(dis.agg)
    efr[sel] <- 0.6 * dis.agg[sel]
    sel <- dis.agg > 0.8 * mean(dis.agg)
    efr[sel] <- 0.3 * dis.agg[sel]
    sel <- dis.agg > 0.4 * mean(dis.agg) & dis.agg <= 0.8 * mean(dis.agg)
    efr[sel] <- 0.45 * dis.agg[sel]

    efr.dis2[x, y, ] <- efr

    efr <- 0.9 * base.agg / days.per.month

    efr.base2[x, y, ] <- base.agg
  }
}
rm(efr.dis, efr.base)
print("Calculated")

year <- 1980
times <- seq(
  from = as.Date(paste0(year, "-01-01")),
  to = as.Date(paste0(year, "-12-31")),
  by = "day",
  origin = "1900-01-01"
)
times.month <- as.numeric(format.Date(times, "%m"))

efr.dis <- array(NA, dim = c(dim(efr.dis2)[1:2], length(times)))
efr.base <- array(NA, dim = c(dim(efr.base2)[1:2], length(times)))
for (x in 1:dim(efr.dis2)[1]) {
  print(x)
  for (y in 1:dim(efr.dis2)[2]) {
    if (is.na(efr.dis2[x, y, 1])) {
      next
    }

    efr.dis[x, y, ] <- linterp(efr.dis2[x, y, ], times.month)
    efr.base[x, y, ] <- linterp(efr.base2[x, y, ], times.month)
  }
}
# rm(efr.dis2, efr.base2)
print("Tranformed")


for (z in 1:length(years)) {
  year <- years[z]
  print(paste0("Working on year ", year))

  times <- seq(
    from = as.Date(paste0(year, "-01-01")),
    to = as.Date(paste0(year, "-12-31")),
    by = "day",
    origin = "1900-01-01"
  )
  times.month <- as.numeric(format.Date(times, "%m"))

  time.dim <- ncdim_def(
    name = "time",
    units = "days since 1970-01-01",
    vals = as.numeric(times),
    unlim = T,
    calendar = "standard"
  )

  out.name <- paste0("efrDischarge_daily_", year, ".nc")
  out.sdir <- paste0("/efrDischarge_daily/")
  out.file <- paste0(out.dir, out.sdir, out.name)

  var <- ncvar_def(
    name = "discharge",
    units = "m3 s-1",
    dim = list(lon.dim, lat.dim, time.dim),
    missval = -1,
    longname = "Discharge requirements for environmental flow",
    prec = "double",
    compression = 9
  )

  dir.create(dirname(out.file), showWarnings = F)
  nc <- nc_create(out.file, list(var))
  ncvar_put(nc, var$name, efr.dis[, , 1:length(times)])
  nc_close(nc)

  out.name <- paste0("efrBaseflow_daily_", year, ".nc")
  out.sdir <- paste0("/efrBaseflow_daily/")
  out.file <- paste0(out.dir, out.sdir, out.name)

  var <- ncvar_def(
    name = "baseflow",
    units = "mm",
    dim = list(lon.dim, lat.dim, time.dim),
    missval = -1,
    longname = "Baseflow requirements for environmental flow",
    prec = "double",
    compression = 9
  )

  dir.create(dirname(out.file), showWarnings = F)
  nc <- nc_create(out.file, list(var))
  ncvar_put(nc, var$name, efr.base[, , 1:length(times)])
  nc_close(nc)
}
