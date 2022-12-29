# in the future - make this into a fucntion
# in theory the nc_info should already be open

#attributes <- data.table::fread('projects/05-Setup-futureMonthly-netCDFs/nc_atts-all.csv')

netCDFnames <- c(attributes$short_name)

varName <- c(attributes$var_name)
  
valueName <- c(attributes$dataset_column_name)

for(n in seq_along(netCDFnames)){
  
  # format
  #wdata_2022_tmax <- as.vector(wdata_2022@data[,2])
  
  #write!
  ncvar_put(netCDFnames[n], varName[n], valueName[n], 
            start = st, count = co)
  nc_sync(tmmx_nc) 
}
  #wdata_2022_tmax <- as.vector(wdata_2022@data[,2])


