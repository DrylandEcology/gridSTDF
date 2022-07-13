# Package names
packages <- c("lubridate", "RSQLite", "DBI", "data.table", "raster", "pbdMPI", "RNetCDF", "sp", "udunits2", "rgdal", "rgeos", "sf", "blob", "circular", "mvtnorm", "abind", "stars")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))