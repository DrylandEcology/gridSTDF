#!/bin/sh
# script for crontab -e
wget -r -l1 -np "https://www.northwestknowledge.net/metdata/data" -P /caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/gridSTDF_Projects/gridSTDF/main/Data -A "tmmx_2022.nc"
wget -r -l1 -np "https://www.northwestknowledge.net/metdata/data" -P /caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/gridSTDF_Projects/gridSTDF/main/Data -A "tmmn_2022.nc"
wget -r -l1 -np "https://www.northwestknowledge.net/metdata/data" -P /caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/gridSTDF_Projects/gridSTDF/main/Data -A "pr_2022.nc"


#wget -r -l1 -np "https://www.northwestknowledge.net/metdata/data" -P /Users/candrews/Documents/Git/gridSTDF/main/Data -A "pr_2022.nc"
#wget -r -l1 -np "https://www.northwestknowledge.net/metdata/data" -P /Users/candrews/Documents/Git/gridSTDF/main/Data -A "tmmn_2022.nc"
#wget -r -l1 -np "https://www.northwestknowledge.net/metdata/data" -P /Users/candrews/Documents/Git/gridSTDF/main/Data -A "tmmx_2022.nc"


