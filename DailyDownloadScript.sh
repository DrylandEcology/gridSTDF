#!/bin/sh
# script for crontab -e
wget -r -l1 -np --no-check-certificate  "https://www.northwestknowledge.net/metdata/data" -P . -A "tmmn_2020.nc"
wget -r -l1 -np --no-check-certificate "https://www.northwestknowledge.net/metdata/data" -P . -A "tmmx_2020.nc"
wget -r -l1 -np --no-check-certificate "https://www.northwestknowledge.net/metdata/data" -P . -A "pr_2020.nc"