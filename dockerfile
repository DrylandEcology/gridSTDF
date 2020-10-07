FROM rocker/r-ver:3.6.3
MAINTAINER Caitlin Andrews <candrews@usgs.com>

RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y libxml2-dev \
	zlib1g-dev \
  libgdal-dev \
  libproj-dev \
  gdal-bin \
  libssl-dev

RUN ["install2.r", "data.table", "lubridate", "raster",  "caTools", "rvest", "sp", "xml2", "zoo", "blob", "RSQLite", "circular", "rgdal", "ncdf4", "plyr", "splines"]

COPY . /usr/local/app/
WORKDIR /usr/local/app/

RUN R -e 'install.packages("/usr/local/app/STDF/Packages/Rcpp_1.0.5.tar.gz", repos = NULL, type = "source")'
RUN R -e 'install.packages("/usr/local/app/STDF/Packages/rSW2utils.tar.gz", repos = NULL, type = "source")'
RUN R CMD INSTALL /usr/local/app/STDF/Packages/rSOILWAT2
RUN R -e 'install.packages("/usr/local/app/STDF/Packages/rSW2funs.tar.gz", repos = NULL, type = "source")'

EXPOSE 80

CMD ["/usr/local/app/STDF/shorttermdroughtforcaster/STDF_plumber.R"]
