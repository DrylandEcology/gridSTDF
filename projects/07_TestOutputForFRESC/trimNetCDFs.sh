#bash script to trim netCDFs to extent of bounding box for FRESC data 
cd /Users/astears/Documents/Dropbox_static/Work/NAU_USGS_postdoc/shortTermDroughtForescaster/gridSTDF/projects/07_TestOutputForFRESC/full_netCDFs

# for one file 
ncks -d lat,42.88937,43.16044 -d lon,-116.74249,-116.24193 ./full_netCDFs/GISSM_yr_gridSTDF_historical_032024.nc -O ./trimmed_netCDFs/GISSM_yr_gridSTDF_historical_032024.nc

# trim all files in the "full_netCDFs directory"
for f in *.nc; do ncks -d lat,42.88937,43.16044 -d lon,-116.74249,-116.24193 "$f" -o "../trimmed_netCDFs/${f%%}"; done