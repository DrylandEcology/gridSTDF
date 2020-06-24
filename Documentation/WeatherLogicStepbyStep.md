
# Short-term Drought Forecaster Weather Logic

This document contains the step by step logic used to gather, downscale, and simulate short-term future weather for specific locations.

## Short-term Future Data Overview

The Climate Prediction Center (CPC) of the National Weather Service (NWS) provides “long-lead” outlooks for temperature and precipitation for 102 regions in the lower 48 of the United States.

![RegionMap](RegionMap.png)

These outlooks consist of the probability of whether a region will be hotter or cooler (temperature) and wetter or drier (precipitation) than their 30-year climatological normal (1980-2010) for 1 1-month and 13 3-month outlooks. These outlooks are updated on the 15th of each month and the 1-month outlook is for the nearest upcoming month while the 3-month outlooks consist of 13 total forecasts for the next year. The 3-month outlooks overlap one another, and each individual month is included in up to 3 outlooks. For example, on May 15th, the closest 1-month outlook would be for June, and the nearest 3-month outlook would be for June-July-August. There are subsequent outlooks for July-August-September, August-September-October, and so on for the next 13 month period.

![image](https://www.cpc.ncep.noaa.gov/products/predictions/multi_season/13_seasonal_outlooks/color/page2.gif)


NWS meteorologists makes these forecasts using a series of models and tools that evaluate historical patterns as well as current global atmospheric and oceanic patterns. Full discussion [here](https://www.cpc.ncep.noaa.gov/products/predictions/long_range/tools.php).

## Technical Guide

Our intent with the short-term forecaster is to translate the information from the NWS CPC into predictions that are fine-tuned for specific locations, instead of the broad outlooks provided for 102 regions. In addition, we translate these predictions to a finer temporal scale, so we are able to utilize them as the climate driver in in a daily driven, water-balance model, SOILWAT2. SOILWAT2 is a site-specific model, that takes inputs about daily weather, vegetation, and soils (multi-layer), and mechanistically predicts daily soil moisture, a metric used for evaluating likely success of plant germination and survival.

To do this, we gather site-specific historical weather information for a specific site (Step 1), and use historical patterns between historical temperature and precipiation data, alongside NWS forecasts (Step 2), to predict a range of future anomalies with multivariate sampling (Step 3). These predictd anomalies are bias corrected (Step 4) and integrated with historical data (Step 5) from the climatic normal period to simulate 900 potential weather and soil moisture futures (Step 6). 

### [Step 0 - Load packages and example data](ShortTermDroughtForecaster_Logic.ipynb#step0)
Load R packages and code necessary to walk through this step by step. The step by step is cumulative. You can walk through this guide by downloading the entire Repo locally and opening jupyter notebook.

### [Step 1 - Gather point specific (lat, long) historic weather data from gridMet](ShortTermDroughtForecaster_Logic.ipynb#step1)
Historical weather data for 1979 through current is downloaded from gridMet.

### [Step 2 - Gather and format short-term predictions from National Weather Service (NWS)](ShortTermDroughtForecaster_Logic.ipynb#step2)

Based on site-specific coordinates, the correct regional NWS dataset is found for the coordinates. Values in this dataset are converted to the necessary units.

### [Step 3 - Use multivariate sampling to generate a range of future anomalies for a specific location.](ShortTermDroughtForecaster_Logic.ipynb#step3)

Historical data is converted to the same time step (leads) and units as the future forecasts. Covariances between historical precipiation and temperature are calculated. Means and variances from the NWS forecasts, alongside these historical covariances, are used to generate future anomalies (n = 30) using multivariate sampling.
 
### [Step 4 - Bias correction of anomaly forecasts](ShortTermDroughtForecaster_Logic.ipynb#step4)

To account for the variation of random draws of anomalies using mulitvaraite sampling and non-normal precipitation distribution (when converted to the necessary units) we correct the anomaly forecasts to the NWS mean.\n",

### [Step 5 - Integrate future anomalies with historical data to create future weather dataset.](ShortTermDroughtForecaster_Logic.ipynb#step5)

Anomalies are converted from the lead scale to the monthly scale. These monthly anomaly values are then applied to the long-term historical climatic normal record (1981 - 2010). Each anomaly (n = 30) is applied to each year in the historical record (years = 30), resulting in 900 (n * years) potential futures.

### [Step 6 - Use weather data to simulate futures](ShortTermDroughtForecaster_Logic.ipynb#step6)

Future years are appended with the previous years' observed data to account for the cunlative effects of weather on moisture in the soil profile. This new weather dataset is given to SOILWAT2, and future soil moisture is simulated.
 
### [Step 7 - Results](ShortTermDroughtForecaster_Logic.ipynb#step7)