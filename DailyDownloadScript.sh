#!/bin/sh
# script for crontab -e
sudo wget -r -l1 -np "https://www.northwestknowledge.net/metdata/data" -P /usr/local/app/WeatherData -A "tmmx_2020.nc"
sudo wget -r -l1 -np "https://www.northwestknowledge.net/metdata/data" -P /usr/local/app/WeatherData -A "tmmn_2020.nc"
sudo wget -r -l1 -np "https://www.northwestknowledge.net/metdata/data" -P /usr/local/app/WeatherData -A "pr_2020.nc"
