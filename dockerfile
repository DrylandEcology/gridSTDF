FROM rocker/r-ver:3.6.3
MAINTAINER Caitlin Andrews <candrews@usgs.com>

RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y libxml2-dev \
	zlib1g-dev



RUN ["install2.r", "caTools", "data.table", "ggplot2", "lubridate", "plumber", "plyr", "raster", "rvest", "sp", "xml2", "zoo"]
WORKDIR /payload/
CMD ["R"]
