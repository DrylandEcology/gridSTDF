FROM rstudio/plumber
MAINTAINER Caitlin Andrews <candrews@usgs.com>

RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y libxml2-dev \
	zlib1g-dev \
  libgdal-dev \
  libproj-dev \
  gdal-bin \
  libssl-dev

# Install cron
RUN apt-get install -y cron

RUN ["install2.r", "data.table", "lubridate", "raster",  "caTools", "rvest", "sp", "xml2", "zoo", "blob", "RSQLite", "circular", "rgdal", "ncdf4", "plyr"]

COPY . /usr/local/app/STDF
WORKDIR /usr/local/app/STDF
VOLUME /usr/local/app/STDF/WeatherData

RUN R -e 'install.packages("/usr/local/app/STDF/Packages/Rcpp_1.0.5.tar.gz", repos = NULL, type = "source")'
RUN R -e 'install.packages("/usr/local/app/STDF/Packages/rSW2utils.tar.gz", repos = NULL, type = "source")'
RUN R CMD INSTALL /usr/local/app/STDF/Packages/rSOILWAT2
RUN R -e 'install.packages("/usr/local/app/STDF/Packages/rSW2funs.tar.gz", repos = NULL, type = "source")'

EXPOSE 8080

RUN chmod +x "/usr/local/app/STDF/shorttermdroughtforecaster/STDF_plumber_docker.R"

ENTRYPOINT ["R", "-f", "/usr/local/app/STDF/shorttermdroughtforecaster/STDF_plumber_docker.R", "--slave"]
#CMD ["/usr/local/app/STDF/shorttermdroughtforecaster/STDF_plumber_docker.R"]

### Usage instructions ########################################################

# Build the images with docker ------
# > cd /usr/local/app/stdf
# > docker build --tag stdf .

# Run with dockerfile ---------------
# >           docker run -it \
# >               -p 8080:8080 \
# >               -v /usr/local/app/WeatherData:/usr/local/app/STDF/WeatherData \
# >               stdf
# then visit online at yourip:8080/stdf

# Build with docker compose -------------
# >           docker-compose up --scale stdf=4 -d

# to debug  ----------
# > docker run -it -v /usr/local/app/WeatherData:/usr/local/app/STDF/WeatherData --rm --entrypoint /bin/bash app_stdf

# ping 
# curl "http://10.12.7.58:8080/gatherDataAndExecuteSW?clay=15&sand=50&soils=2&lng=-111.58&lat=35.258" > data.json

# check logs with -------------------------
# > docker logs -f app_stdf_1
# > docker-composse logs -ft

# stop, implement changes, and restart -----------------------------------------
# > docker-compose down
# > docker build --tag stdf .
# > docker-compose up --scale stdf=4 -d