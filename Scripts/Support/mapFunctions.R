
getNearestZero <- function(map, x, y) {
  return(0)
}

getNearestValue <- function(map, x, y, value) {
  return(value)
}

getNearestCount <- function(map, x, y, itirs = 1000) {
  for (dis in 1:itirs) {
    x.min <- max(x - dis, 1)
    y.min <- max(y - dis, 1)
    x.max <- min(x + dis, dim(map)[1])
    y.max <- min(y + dis, dim(map)[2])

    val <- map[x.min:x.max, y.min:y.max]
    val <- table(val)

    if (!length(val) != 0) {
      idx <- order(val, decreasing = T)[1]
      val <- as.numeric(names(val)[idx])

      return(val)
    }
  }

  warning("Nearest out of iterations")
  return(NA)
}

getNearestMax <- function(map, x, y, itirs = 1000) {
  for (dis in 1:itirs) {
    x.min <- max(x - dis, 1)
    y.min <- max(y - dis, 1)
    x.max <- min(x + dis, dim(map)[1])
    y.max <- min(y + dis, dim(map)[2])

    val <- map[x.min:x.max, y.min:y.max]
    val <- max(val, na.rm = T)

    if (!is.infinite(val)) {
      return(val)
    }
  }

  warning("Nearest out of iterations")
  return(NA)
}

getNearestMean <- function(map, x, y, itirs = 1000) {
  for (dis in 1:itirs) {
    x.min <- max(x - dis, 1)
    y.min <- max(y - dis, 1)
    x.max <- min(x + dis, dim(map)[1])
    y.max <- min(y + dis, dim(map)[2])

    val <- map[x.min:x.max, y.min:y.max]
    val <- mean(val, na.rm = T)

    if (!is.na(val)) {
      return(val)
    }
  }

  warning("Nearest out of iterations")
  return(NA)
}

fillMap <- function(map, na.map, nearest.function, ...) {
  if (max(dim(map)[1:2] != dim(na.map))) {
    print(paste0("map: ", paste0(dim(map)[1:2], collapse(", "))))
    print(paste0("na.map: ", paste0(dim(na.map), collapse(", "))))
    stop("map and na.map dimensions do not match")
  }

  for (x in 1:dim(na.map)[1]) {
    for (y in 1:dim(na.map)[2]) {
      if (na.map[x, y]) {
        if (length(dim(map)) == 2) {
          map[x, y] <- NA
        }
        else if (length(dim(map)) == 3) {
          map[x, y, ] <- NA
        }
        else if (length(dim(map)) == 4) {
          map[x, y, , ] <- NA
        }
        else if (length(dim(map)) == 5) {
          map[x, y, , , ] <- NA
        }
        else {
          stop("Map dimensions not implemented")
        }
        next
      }

      if (length(dim(map)) == 2) {
        if (is.na(map[x, y])) {
          map[x, y] <- nearest.function(map, x, y, ...)
        }
      }
      else if (length(dim(map)) == 3) {
        for (z in 1:dim(map)[3]) {
          if (is.na(map[x, y, z])) {
            map[x, y, z] <- nearest.function(map[, , z], x, y, ...)
          }
        }
      }
      else if (length(dim(map)) == 4) {
        for (z in 1:dim(map)[3]) {
          for (k in 1:dim(map)[4]) {
            if (is.na(map[x, y, z, k])) {
              map[x, y, z, k] <- nearest.function(map[, , z, k], x, y, ...)
            }
          }
        }
      }
      else if (length(dim(map)) == 5) {
        for (z in 1:dim(map)[3]) {
          for (k in 1:dim(map)[4]) {
            for (l in 1:dim(map)[5]) {
              if (is.na(map[x, y, z, k, l])) {
                map[x, y, z, k, l] <- nearest.function(map[, , z, k, l], x, y, ...)
              }
            }
          }
        }
      }
      else {
        stop("Map dimensions not implemented")
      }
    }
  }

  return(map)
}
