
# TODOs
#  - sd_dPPT anomalies should be in units of transformed values
#  - check that my interpretatin of leads is correct
#  - deal with covariance matrices that are not positive (semi-)definite

set.seed(12347)

Niter <- 30 # number of iterations

do_anomalies <- TRUE # FALSE, do modifiers
do_fixed <- FALSE # TRUE, don't randomly sample from sigma, but at fixed modifiers

#------ NWS leads

# Based on https://www.cpc.ncep.noaa.gov/products/predictions/long_range/
# and https://www.cpc.ncep.noaa.gov/pacdir/NFORdir/HUGEdir2/hue.html
# "The valid target season can be found by adding lead months to the
# initial time listed in column 1 and 2, adjusting for year changes."
# My interpretation (TODO: is this correct?):
#   Lead 1 = current month + 0 + 1:3, e.g., May -> JJA
#   Lead 2 = current month + 1 + 1:3, e.g., May -> JAS
# ...
#   Lead 13 = current month + 12 + 1:3, e.g., May -> JJA == Lead 1
Nleads <- 12
leads <- seq_len(Nleads)
todays_month <- as.POSIXlt(Sys.Date())$mon + 1

lookup_months_by_lead <- sapply(
  X = leads,
  FUN = function(lead) {
    tmp <- todays_month + lead - 1
    rSW2utils::circ_seq(
      from = tmp,
      to = tmp + 2,
      int = 12,
      by = 1
    )
  }
)

lead_yearstart <- which(lookup_months_by_lead[1, ] == 1)


#------ Inputs

# daily meteorological variables
# here, grab rSOILWAT2 example values
meteo_daily <- as.data.frame(rSOILWAT2::dbW_weatherData_to_dataframe(
  weatherData = rSOILWAT2::weatherData
))


# NWS forecasted data
# 30-year climatological normal (1980-2010) and anomalies
# (here, later filled in with example data)
# TODO: anomaly_sd_dPPT should be in units of transformed values
forecast_nws <- data.frame(
  Lead = leads,
  clim_T_C = NA,
  clim_PPT_cm = NA,
  anomaly_mean_dT_C = NA,
  anomaly_mean_dPPT_cm = NA,
  anomaly_sd_dT_C = NA,
  anomaly_sd_dPPT_cm = NA
)


#------ Prepare meteorological values
# Calculate mean daily temperature
meteo_daily[, "Tmean_C"] <- apply(
  X = meteo_daily[, c("Tmax_C", "Tmin_C")],
  MARGIN = 1,
  FUN = mean
)

# Determine months
meteo_daily[, "Month"] <- as.POSIXlt(
  x = apply(
    X = meteo_daily[, c("Year", "DOY")],
    MARGIN = 1,
    FUN = paste,
    collapse = "-"
  ),
  format = "%Y-%j",
  tz = "UTC"
)$mon + 1


# Aggregate to monthly values
meteo_monthly <- data.frame(
  {
    tmp <- aggregate(
      x = meteo_daily[["Tmean_C"]],
      by = meteo_daily[c("Month", "Year")],
      FUN = mean
    )
    colnames(tmp)[3] <- "Tmean_C"
    tmp[, c(2, 1, 3)]
  },
  PPT_cm = aggregate(
    x = meteo_daily[["PPT_cm"]],
    by = meteo_daily[c("Month", "Year")],
    FUN = sum
  )[["x"]]
)


# Aggregate to moving left-aligned 3-month periods (NWS lead seasons)
# e.g., Month = 1 represents the average across months 1:3
#       Month = 2 represents the average across months 2:4
meteo_moving3monthly <- meteo_monthly

tmp <- nrow(meteo_moving3monthly)
meteo_moving3monthly[(tmp - 1):tmp, c("Tmean_C", "PPT_cm")] <- NA

meteo_moving3monthly[1:(tmp - 2), c("Tmean_C", "PPT_cm")] <- zoo::rollmean(
  x = meteo_monthly[c("Tmean_C", "PPT_cm")],
  k = 3,
  FUN = mean,
  partial = TRUE,
  align = "left"
)

# Convert moving left-aligned 3-month periods to NWS leads
meteo_leads <- data.frame(
  Lead = rSW2utils::circ_add(
    lead_yearstart - 1,
    meteo_moving3monthly[["Month"]] -1,
    int = 12,
    type = "ZeroPlus2Pi"
  ),
  meteo_moving3monthly[c("Tmean_C", "PPT_cm")]
)


#------ Example data fill-in
# here, grab 1980-2010 example meteo data as climate normals
# don't use this when nws climate normals are grabbed from their website
ids <- meteo_moving3monthly[["Year"]] %in% 1980:2010
tmp <- aggregate(
  x = meteo_leads[ids, c("Tmean_C", "PPT_cm")],
  by = list(meteo_leads[ids, "Lead"]),
  FUN = mean,
  na.rm = TRUE
)

forecast_nws[["clim_T_C"]] <- tmp[["Tmean_C"]]
forecast_nws[["clim_PPT_cm"]] <- tmp[["PPT_cm"]]

# Made up values
forecast_nws[["anomaly_mean_dT_C"]] <- 2
forecast_nws[["anomaly_sd_dT_C"]] <- 1
forecast_nws[["anomaly_mean_dPPT_cm"]] <- -1
forecast_nws[["anomaly_sd_dPPT_cm"]] <- 1.5

#------ End example data fill-in


#------ Calculate anomalies corresponding to NWS deviations
ids <- meteo_leads[["Lead"]]
meteo_anomalies_leads2 <- data.frame(
  Lead = ids,
  dT_C = meteo_leads[["Tmean_C"]] - forecast_nws[["clim_T_C"]][ids],
  dPPT_cm = meteo_leads[["PPT_cm"]] - forecast_nws[["clim_PPT_cm"]][ids]
)


#------ Calculate means and covariance matrix and lagged cross-variations by leads
tmp <- paste0(rep(c("dT_C", "dPPT_cm"), each = Nleads), "_L", leads)

sigma <- array(
  0,
  dim = rep(2 * Nleads, 2),
  dimnames = list(tmp, tmp)
)

# Variances from NWS forecasts
if (do_anomalies) {
  diag(sigma)[leads] <- forecast_nws[, "anomaly_sd_dT_C"] ^ 2
  diag(sigma)[Nleads + leads] <- forecast_nws[, "anomaly_sd_dPPT_cm"] ^ 2
} else {
  diag(sigma) <- 1
}

# Within-lead covariances among dT and dPPT from historical data
cov_anomalies_leads2 <- by(
  data =  meteo_anomalies_leads2[c("dT_C", "dPPT_cm")],
  INDICES = meteo_anomalies_leads2["Lead"],
  FUN = cov,
  use = "na.or.complete"
)

for (k in leads) {
  sigma[k, Nleads + k] <- sigma[Nleads + k, k] <- cov_anomalies_leads2[[k]][1, 2]
}


# 1-lead lagged covariances within dT and within dPPT from historical data
# "this lead is similar to the previous lead"
hist_cov_1lag <- array(
  NA,
  dim = c(Nleads, 2),
  dimnames = list(NULL, c("dT_C", "dPPT_cm"))
)

for (k in leads) {
  ids <- k == meteo_anomalies_leads[["Lead"]]
  k_lagged1 <- rSW2utils::circ_minus(k, 1, int = 12, type = "ZeroPlus2Pi")
  ids_lagged1 <- k_lagged1 == meteo_anomalies_leads[["Lead"]]
  
  var_lag1 <- var(
    meteo_anomalies_leads[ids, c("dT_C", "dPPT_cm")],
    meteo_anomalies_leads[ids_lagged1, c("dT_C", "dPPT_cm")],
    na.rm = TRUE
  )
  
  hist_cov_1lag[k, "dT_C"] <- var_lag1[1, 1]
  hist_cov_1lag[k, "dPPT_cm"] <- var_lag1[2, 2]
  
  sigma[k, k_lagged1] <- sigma[k_lagged1, k] <- var_lag1[1, 1]
  sigma[Nleads + k, Nleads + k_lagged1] <- var_lag1[2, 2]
  sigma[Nleads + k_lagged1, Nleads + k] <- var_lag1[2, 2]
}

# Check covariance matrix: plot as correlation matrix
image(
  t(sigma)[ncol(sigma):1, ],
  zlim = c(-1, 1) * rep(max(abs(sigma)), 2),
  col = hcl.colors(13, "Purple-Brown")
)
abline(h = 0.5)
abline(v = 0.5)

sigma2 <- as.matrix(
  Matrix::nearPD(
    x = sigma,
    keepDiag = TRUE
  )[["mat"]]
)

image(
  t(sigma2)[ncol(sigma2):1, ],
  zlim = c(-1, 1) * rep(max(abs(sigma2)), 2),
  col = hcl.colors(13, "Purple-Brown")
)
abline(h = 0.5)
abline(v = 0.5)


image(
  t(sigma2 - sigma)[ncol(sigma2):1, ],
  zlim = c(-1, 1) * rep(max(abs(sigma2 - sigma)), 2),
  col = hcl.colors(13, "Purple-Brown")
)
abline(h = 0.5)
abline(v = 0.5)



# Means as forecasted NWS means
if (do_anomalies) {
  mus2 <- c(
    forecast_nws[, "anomaly_mean_dT_C"], forecast_nws[, "anomaly_mean_dPPT_cm"]
  )
} else {
  mus <- rep(0, 2 * Nleads)
}

#------ Draw multivariate normal anomalies
gen_anomalies_leads <- array(
  if (!do_fixed) {
    retval <- tryCatch(
      expr = mvtnorm::rmvnorm(
        n = Niter,
        mean = mus2,
        sigma = sigma,
        method = "chol" # c("eigen", "svd", "chol")
      ),  
      
      warning = function(w) {
        # Make sigma positive definite
        sigma2 <- Matrix::nearPD(
          x = sigma,
          keepDiag = TRUE
        )
        
        if (!sigma2[["converged"]]) stop("not converged")
        
        mvtnorm::rmvnorm(
          n = Niter,
          mean = mus,
          sigma = as.matrix(sigma2[["mat"]]),
          method = "chol"
        )
      }
    )
    
  } else {
    # sample multivariate distribution at fixed "modifiers"
    mods <- seq(-2, 2, length.out = 5)
    
    # Cholesky decomposition
    R <- chol(sigma2, pivot = TRUE)
    R <- R[, order(attr(R, "pivot"))]
    
    retval <- matrix(
      rep(mods, ncol(sigma2)),
      nrow = length(mods),
      byrow = FALSE
    )
    retval <- sweep(retval %*% R, 2, mus, "+")
    colnames(retval) <- colnames(sigma2)
  }
  
  NA,
  dim = c(Niter, length(leads), 2),
  dimnames = list(NULL, NULL, c("dT_C", "dPPT_cm"))
)

if (do_anomalies) {
  gen_anomalies_leads[, , "dT_C"] <- retval[, leads]
  gen_anomalies_leads[, , "dPPT_cm"] <- retval[, Nleads + leads]
} else {
  gen_anomalies_leads[, , "dT_C"] <-
    retval[, leads] * forecast_nws[, "anomaly_sd_dT_C"]
  gen_anomalies_leads[, , "dPPT_cm"] <-
    retval[, Nleads + leads] * forecast_nws[, "anomaly_sd_dPPT_cm"]
}


#------ Checks
cols <- c(rep("orange", Nleads), rep("blue", Nleads))

# Check that means have no bias compared to forecasted NWS means
tmp_mean <- apply(
  X = gen_anomalies_leads,
  MARGIN = 2:3,
  FUN = mean
)

plot(
  tmp_mean,
  as.matrix(forecast_nws[, c("anomaly_mean_dT_C", "anomaly_mean_dPPT_cm")]),
  xlab = "Sim means",
  ylab = "Forecasted means",
  col = cols
)
abline(0, 1, col = "red")


# Check that SDs have no bias compared to forecasted NWS SDs
tmp_sd <- apply(
  X = gen_anomalies_leads,
  MARGIN = 2:3,
  FUN = sd
)

plot(
  tmp_sd,
  as.matrix(forecast_nws[, c("anomaly_sd_dT_C", "anomaly_sd_dPPT_cm")]),
  xlab = "Sim SDs",
  ylab = "Forecasted SDs",
  col = cols
)
abline(0, 1, col = "red")


# Check that covariances have no bias compared to historical covariances
tmp_cov <- apply(
  X = gen_anomalies_leads,
  MARGIN = 2,
  FUN = function(x) cov(x, use = "na.or.complete")[1, 2]
)

plot(
  tmp_cov,
  sapply(cov_anomalies_leads, function(x) x[1, 2]),
  xlab = "Sim covs",
  ylab = "Historical covs"
)
abline(0, 1, col = "red")



# Check that 1-lead lagged covariances have no bias compared to historical lags
tmp_cov_1lag <- array(
  NA,
  dim = c(Nleads, 2),
  dimnames = list(NULL, c("dT_C", "dPPT_cm"))
)

for (k in leads) {
  k_lagged1 <- rSW2utils::circ_minus(k, 1, int = 12, type = "ZeroPlus2Pi")
  
  var_lag1 <- var(
    gen_anomalies_leads[, k, c("dT_C", "dPPT_cm")],
    gen_anomalies_leads[, k_lagged1, c("dT_C", "dPPT_cm")],
    na.rm = TRUE
  )
  
  tmp_cov_1lag[k, "dT_C"] <- var_lag1[1, 1]
  tmp_cov_1lag[k, "dPPT_cm"] <- var_lag1[2, 2]
}

plot(
  tmp_cov_1lag,
  hist_cov_1lag,
  xlab = "Sim 1-lead lagged covs",
  ylab = "Historical 1-lead lagged covs",
  col = cols
)
abline(0, 1, col = "red")
