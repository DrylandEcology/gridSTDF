# in the future - make this into a function
# in theory the nc_info should already be open

#attributes <- data.table::fread('projects/05-Setup-futureMonthly-netCDFs/nc_atts-all.csv')

# check for different depths and names
valueName <- c(attributes$dataset_column_name)[1:96]
index <- valueName %in% names(AllVarData)

netCDFnames <- c(attributes$short_name)[1:96][index]

varName <- c(attributes$var_name)[1:96][index]

valueName <- c(attributes$dataset_column_name)[index]

tdim <- c(attributes$time_values_max)[1:96][index]

for(n in seq_along(netCDFnames)){
  
  #print(netCDFnames[n])
  #print(valueName[n])

  # set count based on length of values
  co <- c(1, 1, tdim[n])

  # format
  vals <- as.vector(AllVarData[,valueName[n]])
  if(tdim[n] < 549) vals <- vals[!is.na(vals)]

  #write!
  pbdNCDF4::ncvar_put(get(netCDFnames[n]), varName[n], vals, 
          start = st, count = co)
  nc_sync(get(netCDFnames[n])) 
}

# ecological variables ----------------------------------------------------------
netCDFnames <- c(attributes$short_name)[97:100]
varName <- c(attributes$var_name)[97:100]

Shriver_Hist <- c(Shriver_Stats[TP == 'Historical', 'Prob'])
pbdNCDF4::ncvar_put(get(netCDFnames[n]), varName[n], Shriver_Hist, 
                    start = st, count = co)

nc_sync(get(netCDFnames[n])) 
#
Shriver_Fut <- c(Shriver_Stats[TP != 'Historical', 'Prob'])
pbdNCDF4::ncvar_put(get(netCDFnames[n]), varName[n], Shriver_Fut, 
                    start = st, count = co)

nc_sync(get(netCDFnames[n])) 
#
GISSM_Hist <- c(NA, Hist_GISSM$SeedlingSurvival_1stSeason)
pbdNCDF4::ncvar_put(get(netCDFnames[n]), varName[n], GISSM_Hist, 
                    start = st, count = co)

nc_sync(get(netCDFnames[n])) 
#
GISSM_Fut <- Future_GISSM$Prob
pbdNCDF4::ncvar_put(get(netCDFnames[n]), varName[n], GISSM_Fut, 
                    start = st, count = co)

nc_sync(get(netCDFnames[n])) 