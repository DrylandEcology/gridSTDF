# in the future - make this into a fucntion
# in theory the nc_info should already be open

#attributes <- data.table::fread('projects/05-Setup-futureMonthly-netCDFs/nc_atts-all.csv')

netCDFnames <- c(attributes$short_name)[1:72]

varName <- c(attributes$var_name)[1:72]
  
valueName <- c(attributes$dataset_column_name)[1:72]

tdim <- c(attributes$time_values_max)[1:72]

for(n in seq_along(netCDFnames)){

  # set count based on length of values
  co <- c(1, 1, tdim[n])

  # format
  vals <- as.vector(AllVarData[,valueName[n]])

  #write!
  ncvar_put(netCDFnames[n], varName[n], vals, 
            start = st, count = co)
  nc_sync(tmmx_nc) 
}


