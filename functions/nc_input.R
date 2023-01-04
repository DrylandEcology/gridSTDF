# in the future - make this into a fucntion
# in theory the nc_info should already be open

#attributes <- data.table::fread('projects/05-Setup-futureMonthly-netCDFs/nc_atts-all.csv')

# check for different depths and names
valueName <- c(attributes$dataset_column_name)[1:72]
index <- valueName %in% names(AllVarData)


netCDFnames <- c(attributes$short_name)[1:72][index]

varName <- c(attributes$var_name)[1:72][index]
  
valueName <- c(attributes$dataset_column_name)[1:72][index]

tdim <- c(attributes$time_values_max)[1:72][index]

for(n in seq_along(netCDFnames)){
  
  #print(netCDFnames[n])
  #print(valueName[n])

  # set count based on length of values
  co <- c(1, 1, tdim[n])

  # format
  vals <- as.vector(AllVarData[,valueName[n]])
  if(tdim[n] == 351) vals <- vals[!is.na(vals)]

  #write!
  pbdNCDF4::ncvar_put(get(netCDFnames[n]), varName[n], vals, 
          start = st, count = co)
  nc_sync(get(netCDFnames[n])) 
}

