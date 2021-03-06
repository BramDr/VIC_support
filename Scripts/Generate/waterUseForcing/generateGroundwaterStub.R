library(fields)
library(ncdf4)
rm(list = ls())

# Input
in.files <- c(
  "../../../Data/Transformed/Manufacturing/manufacturingGroundwaterFraction_30min_global.RDS",
  "../../../Data/Transformed/Domestic/domesticGroundwaterFraction_30min_global.RDS",
  "../../../Data/Transformed/Energy/energyGroundwaterFraction_30min_global.RDS",
  "../../../Data/Transformed/Livestock/livestockGroundwaterFraction_30min_global.RDS"
)
out.dir <- "../../../Data/VIC/Forcing/global/"
years <- 1979:2016

# Setup
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
for (in.file in in.files) {
  print(paste0("Working on file ", basename(in.file)))

  sector <- ""
  sec <- ""
  type <- ""
  ty <- ""
  units <- "fraction"
  adjust <- 1
  if (length(grep(x = in.file, "manufacturing")) > 0) {
    sector <- "manufacturing"
    sec <- "man"
  }
  if (length(grep(x = in.file, "domestic")) > 0) {
    sector <- "domestic"
    sec <- "dom"
  }
  if (length(grep(x = in.file, "energy")) > 0) {
    sector <- "energy"
    sec <- "ene"
  }
  if (length(grep(x = in.file, "livestock")) > 0) {
    sector <- "livestock"
    sec <- "liv"
  }
  if (length(grep(x = in.file, "Groundwater")) > 0) {
    type <- "groundwater_fraction"
    ty <- "Ground"
  }
  if (length(grep(x = in.file, "Consumption")) > 0) {
    type <- "consumption_fraction"
    ty <- "Cons"
  }

  data <- readRDS(in.file)
  data <- data * 0

  for (z in 1:length(years)) {
    year <- years[z]

    out.name <- paste0(sec, ty, "_yearly_stub_", year, ".nc")
    out.sdir <- paste0("/", sec, ty, "_yearly_stub/")
    out.file <- paste0(out.dir, out.sdir, out.name)

    times <- as.Date(paste0(year, "-01-01"))

    time.dim <- ncdim_def(
      name = "time",
      units = "days since 1970-01-01",
      vals = as.numeric(times),
      unlim = T,
      calendar = "standard"
    )

    var <- ncvar_def(
      name = type,
      units = units,
      dim = list(lon.dim, lat.dim, time.dim),
      missval = -1,
      longname = paste0(sector, " water ", type),
      prec = "double",
      compression = 9
    )

    dir.create(dirname(out.file))
    nc <- nc_create(out.file, list(var))

    ncvar_put(nc, var$name, data)
    nc_close(nc)
  }
}
