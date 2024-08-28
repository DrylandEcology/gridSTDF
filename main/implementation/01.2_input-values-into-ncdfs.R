# in the future - make this into a function
# in theory the nc_info should already be open

#attributes <- data.table::fread('projects/05-Setup-futureMonthly-netCDFs/nc_atts-all.csv')

# check for different depths and names
valueName <- c(attributes$dataset_column_name)[1:106]
#index <- valueName %in% names(AllVarData)

netCDFnames <- c(attributes$short_name)[1:106] 

varName <- c(attributes$var_name)[1:106]

valueName <- c(attributes$dataset_column_name)
valueName_short <- c(attributes$short_name)
tdim <- c(attributes$time_values_max)[1:106]


for(n in seq_along(netCDFnames)){
  
  print(netCDFnames[n])
  print(valueName[n])

  # where to start 
  # set count based on length of values data_dims_nc <- c(0, 739, 585, 0, nrow(time_bounds), 0)
  # the 'count' that will be fed into nc_put, which is a "vector of integers 
  # indicating the count of values to write along each dimension, order is x-y-t)
  co <- c(1, 1, tdim[n])
  st_n <- st
  #AES get values for last 180 days? 
  # get the date for 180 previous from today
  if (tdim[n] == 180) {
    vals <- AllVarData[1:183,valueName[n]] 
  } else if (tdim[n] == 549){
    vals <- as.vector(AllVarData[ ,valueName[n]]) 
  } else if (tdim[n] == 352) {
    vals <- as.vector(AllVarData[(nrow(AllVarData)-351):nrow(AllVarData),valueName[n]])
  } else if (tdim[n] == 31) { # for Oconnor variables
    if (valueName_short[n] == "oconnor-swp_mean") {
      vals <- c(Oconnor_Stats[TP == "Forecast",]$SWP_mean)
    }
    if (valueName_short[n] == "oconnor-swp_95CI_lower") {
      vals <- c(Oconnor_Stats[TP == "Forecast",]$SWP_mean) - c(Oconnor_Stats[TP == "Forecast",]$SWP_CI95)
    }
     if (valueName_short[n] == "oconnor-swp_95CI_upper") {
      vals <- c(Oconnor_Stats[TP == "Forecast",]$SWP_mean) + c(Oconnor_Stats[TP == "Forecast",]$SWP_CI95)
     }
    if (valueName_short[n] == "oconnor-stemp_mean") {
      vals <- c(Oconnor_Stats[TP == "Forecast",]$sTemp_mean)
    }
    if (valueName_short[n] == "oconnor-stemp_95CI_lower") {
      vals <- c(Oconnor_Stats[TP == "Forecast",]$sTemp_mean) - c(Oconnor_Stats[TP == "Forecast",]$sTemp_CI95)
    }
    if (valueName_short[n] == "oconnor-stemp_95CI_upper") {
      vals <- c(Oconnor_Stats[TP == "Forecast",]$sTemp_mean) + c(Oconnor_Stats[TP == "Forecast",]$sTemp_CI95)
    }
  } 
  if (valueName_short[n] == "shriver_historical") vals <- c(Shriver_Stats[TP == 'Historical', 'Prob'])[["Prob"]]
  if (valueName_short[n] == "shriver_prediction") {
    vals <- c(Shriver_Stats[TP != 'Historical', 'Prob'])$Prob
    # redefine tdim to have the number of time steps for this run 
    # the 'count' that will be fed into nc_put, which is a "vector of integers 
    # indicating the count of values to write along each dimension, order is x-y-z-t)
    co <- c(1, 1, 30, tdim[n]) 
    # ammend the 'st' vecotr, which tells the ncvar_put function where to start writing the data 
    st_n <- c(st, 1)
    }
  
  if (valueName_short[n] == "GISSM_historical") vals <- as.numeric(c(NA, Hist_GISSM$SeedlingSurvival_1stSeason))
  if (valueName_short[n] == "GISSM_prediction") {
    vals <-  Future_GISSM$Prob
    # redefine tdim to have the number of time steps for this run 
    # the 'count' that will be fed into nc_put, which is a "vector of integers 
    # indicating the count of values to write along each dimension, order is x-y-z-t)
    co <- c(1, 1, 30, tdim[n]) 
    # ammend the 'st' vecotr, which tells the ncvar_put function where to start writing the data 
    st_n <- c(st, 1)
}
  #write!
  RNetCDF::var.put.nc(ncfile = get(netCDFnames[n]),
                      variable = varName[n], 
                      data = vals,
                      start = st_n, 
                      count = co)
  RNetCDF::sync.nc(get(netCDFnames[n]))
  # if (isParallel) {
  #   RNetCDF::var.put.nc(ncfile = get(netCDFnames[n]),
  #                       variable = varName[n], 
  #                       data = vals,
  #                       start = st_n, 
  #                       count = co)
  #   RNetCDF::sync.nc(get(netCDFnames[n]))
  # } else {
  #   ncdf4::ncvar_put(get(netCDFnames[n]), varName[n], vals, 
  #                       start = st_n, count = co)
  #   ncdf4::nc_sync(get(netCDFnames[n])) 
  # }

}

# ecological variables ----------------------------------------------------------
#netCDFnames <- c(attributes$short_name)[97:100]
#varName <- c(attributes$var_name)[97:100]

