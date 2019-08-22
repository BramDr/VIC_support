library(ggplot2)
rm(list = ls())

# Input
iso.file = "Input/ISO3166_dev.csv"
fit.file = "Input/country_domestic_global_fitting_MC"
fit.out = "Output/country_domestic_global_MC"

# Load
iso = read.csv(file = iso.file, stringsAsFactors = F)

# Setup
itirations = 1000

# Calculate
for(itiration in 1:itirations) {
  data.fit = read.csv(paste0(fit.file, "_", itiration, ".csv"), stringsAsFactors = F)

  data.fit$Int = 1
  data.fit$Int[data.fit$Development_number == 3 & data.fit$Year > 1980] = 0.995
  data.fit$Int[data.fit$Development_number == 2 & data.fit$Year > 1980] = 0.99
  data.fit$Int[data.fit$Development_number == 1 & data.fit$Year > 1980] = 0.98
  
  # Global fit
  data.fit$lPar = log(7.5 * 365 * 1e-3, 10)
  fit.global = nls(formula = lWithPc ~ log(10^(lPar + (u - lPar) / (1 + exp(-f * (lGdpPc - o)))) * Int ^ (Year - 1980), 10),
            data = data.fit,
            start = list(u = max(data.fit$lWithPc),
                         o = 3,
                         f = 3),
            control = list(maxiter = 500))
  summary(fit.global)
  
  data.fit$uPar = coefficients(fit.global)[["u"]]
  data.fit$gfPar = coefficients(fit.global)[["f"]]
  data.fit$goPar = coefficients(fit.global)[["o"]]
  data.fit$glWithPc = predict(object = fit.global, newdata = data.fit)
  data.fit$gWithPc = 10^data.fit$glWithPc
  
  # Subregion
  data.fit$slWithPc = NA
  data.fit$sWithPc = NA
  data.fit$sfPar = NA
  data.fit$soPar = NA
  for(region in unique(data.fit$Subregion)){
    region.data = data.fit[data.fit$Subregion == region,]
  
    if(exists("fit")){
      rm(fit)
    }
    try(fit <- nls(formula = lWithPc ~ log(10^(lPar + (uPar - lPar) / (1 + exp(-f * (lGdpPc - o)))) * Int ^ (Year - 1980), 10),
                     data = region.data,
                     start = list(o = coefficients(fit.global)[["o"]],
                                  f = coefficients(fit.global)[["f"]]),
                     control = list(maxiter = 500)), silent = TRUE)
    if(!exists("fit")){
      next
    }
  
    region.data$clWithPc = predict(object = fit, newdata = region.data)
    region.data$cWithPc = 10^region.data$clWithPc
  
    data.fit$slWithPc[data.fit$Subregion == region] = region.data$clWithPc
    data.fit$sWithPc[data.fit$Subregion == region] = region.data$cWithPc
    data.fit$sfPar[data.fit$Subregion == region] = coefficients(fit)[["f"]]
    data.fit$soPar[data.fit$Subregion == region] = coefficients(fit)[["o"]]
  }
  
  # Region
  data.fit$rlWithPc = NA
  data.fit$rWithPc = NA
  data.fit$rfPar = NA
  data.fit$roPar = NA
  for(region in unique(data.fit$Region)){
    region.data = data.fit[data.fit$Region == region,]
  
    if(exists("fit")){
      rm(fit)
    }
    try(fit <- nls(formula = lWithPc ~ log(10^(lPar + (uPar - lPar) / (1 + exp(-f * (lGdpPc - o)))) * Int ^ (Year - 1980), 10),
                   data = region.data,
                   start = list(o = coefficients(fit.global)[["o"]],
                                f = coefficients(fit.global)[["f"]]),
                   control = list(maxiter = 500)), silent = TRUE)
    if(!exists("fit")){
      next
    }
  
    region.data$clWithPc = predict(object = fit, newdata = region.data)
    region.data$cWithPc = 10^region.data$clWithPc
  
    data.fit$rlWithPc[data.fit$Region == region] = region.data$clWithPc
    data.fit$rWithPc[data.fit$Region == region] = region.data$cWithPc
    data.fit$rfPar[data.fit$Region == region] = coefficients(fit)[["f"]]
    data.fit$roPar[data.fit$Region == region] = coefficients(fit)[["o"]]
  }
  
  data.fit.s = aggregate(formula = cbind(uPar, lPar, goPar, gfPar, soPar, sfPar, roPar, rfPar) ~ Subregion_number, 
                           data = data.fit, FUN = mean, na.rm = T)
  data.fit.r = aggregate(formula = cbind(uPar, lPar, goPar, gfPar, soPar, sfPar, roPar, rfPar) ~ Region_number, 
                          data = data.fit, FUN = mean, na.rm = T)
  
  coef = iso[,c("Country_number", "Subregion_number", "Region_number", "Development_number")]
  coef$uPar = NA
  coef$lPar = NA
  coef$oPar = NA
  coef$fPar = NA
  sr.region = c()
  sr.global = c()
  for(i in 1:nrow(coef)){
    if(coef$Subregion_number[i] %in% data.fit.s$Subregion_number){
      j = which(data.fit.s$Subregion_number == coef$Subregion_number[i])
      coef$uPar[i] = data.fit.s$uPar[j]
      coef$lPar[i] = data.fit.s$lPar[j]
      coef$oPar[i] = data.fit.s$soPar[j]
      coef$fPar[i] = data.fit.s$sfPar[j]
    } else if(coef$Region_number[i] %in% data.fit.r$Region_number){
      j = which(data.fit.r$Region_number == coef$Region_number[i])
      coef$uPar[i] = data.fit.r$uPar[j]
      coef$lPar[i] = data.fit.r$lPar[j]
      coef$oPar[i] = data.fit.r$roPar[j]
      coef$fPar[i] = data.fit.r$rfPar[j]
      sr.region = c(sr.region, coef$Subregion_number[i])
    } else {
      coef$uPar[i] = mean(data.fit$uPar, na.rm = T)
      coef$lPar[i] = mean(data.fit$lPar, na.rm = T)
      coef$oPar[i] = mean(data.fit$goPar, na.rm = T)
      coef$fPar[i] = mean(data.fit$gfPar, na.rm = T)
      sr.global = c(sr.global, coef$Subregion_number[i])
    }
  }
  
  print(unique(sr.region))
  print(unique(sr.global))
  
  # Save
  write.csv(x = coef, file = paste0(fit.out, "_", itiration, ".csv"), row.names = F)
}
