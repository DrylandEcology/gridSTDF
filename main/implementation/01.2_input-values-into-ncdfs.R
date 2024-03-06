# in the future - make this into a function
# in theory the nc_info should already be open

#attributes <- data.table::fread('projects/05-Setup-futureMonthly-netCDFs/nc_atts-all.csv')

# check for different depths and names
valueName <- c(attributes$dataset_column_name)[1:100]
#index <- valueName %in% names(AllVarData)

netCDFnames <- c(attributes$short_name)[1:100] 

varName <- c(attributes$var_name)[1:100]

valueName <- c(attributes$dataset_column_name)

tdim <- c(attributes$time_values_max)[1:100]


for(n in seq_along(netCDFnames)){
  
  #print(netCDFnames[n])
  #print(valueName[n])

  # set count based on length of values
  co <- c(1, 1, tdim[n])

  ##AES not really sure why ecological predictions are included here? 
  # format
  # Shriver_Hist <- c(Shriver_Stats[TP == 'Historical', 'Prob'])
  # Shriver_Fut <- c(Shriver_Stats[TP != 'Historical', 'Prob'])
  # GISSM_Hist <- c(NA, Hist_GISSM$SeedlingSurvival_1stSeason)
  # GISSM_Fut <- Future_GISSM$Prob
  
  #AES get values for last 180 days? 
  # get the date for 180 previous from today
  if (tdim[n] == 180) {
    vals <- AllVarData[1:183,valueName[n]] 
  } else if (tdim[n] == 549){
    vals <- as.vector(AllVarData[ ,valueName[n]]) 
  } else if (tdim[n] == 352) {
    vals <- as.vector(AllVarData[(nrow(AllVarData)-351):nrow(AllVarData),valueName[n]])
  } else {
    vals <- as.vector(AllVarData[ ,valueName[n]])
    vals <- vals[!is.na(vals)]
  }
  #vals <- c(vals, Shriver_Hist,Shriver_Fut, GISSM_Hist, GISSM_Fut) #add ecovars
  
  #write!
  if (isParallel) {
    pbdNCDF4::ncvar_put(get(netCDFnames[n]), varName[n], vals, 
                        start = st, count = co)
    pbdNCDF4::nc_sync(get(netCDFnames[n])) 
  } else {
    ncdf4::ncvar_put(get(netCDFnames[n]), varName[n], vals, 
                        start = st, count = co)
    ncdf4::nc_sync(get(netCDFnames[n])) 
  }

}

# ecological variables ----------------------------------------------------------
#netCDFnames <- c(attributes$short_name)[97:100]
#varName <- c(attributes$var_name)[97:100]

