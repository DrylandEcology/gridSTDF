#!/bin/sh
# script for crontab -e
# wget -r -l1 -np "https://www.northwestknowledge.net/metdata/data" -P /caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/gridSTDF_Projects/gridSTDF/main/Data -A "tmmx_2023.nc"
# wget -r -l1 -np "https://www.northwestknowledge.net/metdata/data" -P /caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/gridSTDF_Projects/gridSTDF/main/Data -A "tmmn_2023.nc"
# wget -r -l1 -np "https://www.northwestknowledge.net/metdata/data" -P /caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/gridSTDF_Projects/gridSTDF/main/Data -A "pr_2023.nc"

#Rscript "/caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/gridSTDF_Projects/gridSTDF/main/getDataOffline.R"

wget -r -l1 -np "https://www.northwestknowledge.net/metdata/data" -P /Users/astears/Documents/Dropbox_static/Work/NAU_USGS_postdoc/shortTermDroughtForescaster/gridSTDF/main/Data -A "pr_2024.nc"
wget -r -l1 -np "https://www.northwestknowledge.net/metdata/data" -P /Users/astears/Documents/Dropbox_static/Work/NAU_USGS_postdoc/shortTermDroughtForescaster/gridSTDF/main/Data -A "tmmn_2024.nc"
wget -r -l1 -np "https://www.northwestknowledge.net/metdata/data" -P /Users/astears/Documents/Dropbox_static/Work/NAU_USGS_postdoc/shortTermDroughtForescaster/gridSTDF/main/Data -A "tmmx_2024.nc"


Rscript "/Users/astears/Documents/Dropbox_static/Work/NAU_USGS_postdoc/shortTermDroughtForescaster/gridSTDF/main/getDataOffline.R"
