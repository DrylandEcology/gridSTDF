# Grid STDF Overview
## Project Goal
The goal of this project (and the code in this repository) is to generate monthly, site-specific forecasts of precipitation, temperature, soil moisture, and germination and establishment of common restoration species over the next year for each 4-km grid cell in the western, contiguous US. This process will be repeated monthly, and will serve as a resource (housed online as part of the Land Treatment Exploration Tool) to help identify both when and where abiotic conditions are more likely to favor positive restoration outcomes.  

## Repository Contents
/functions
- this folder contains files that create functions used within the main simulation runner 

/HPC_Files
- this folder contains shell scripts and other files to set up the modules and R packages necessary to run the simulations (or currently, the test scripts found in projects/08_ParallelTest) on the HPC

/main
- This folder contains the scripts and data to actually run the gridded drought forecaster. Running the script "./main/DailyDownloadScript.sh" is the first step in a simulation run, which downloads new climate data that has been recorded since the last run, and stores it in "./main/Data". Then, the main process is run using the file "./main/implementation/01_main-simulation-runner.R". 

/outputs 
- this folder is not present in the GitHub repository, but note that the main simulation will write its output to this folder

/projects 
- this folder contains a series of folders, each of which corresponds to a "project" related to developing the forecaster. Each "project" is either a one-time task that was necessary to get the forecaster set up, or is code related to figuring out a specific challenge. None of this code is required to run the simulation, but can be edited if certain components they create that are used in the simulation need to be edited.


## Methods Overview
- Access long-lead forecasts from the National Weather Service Climate Prediction Center. These forecasts predict, for each of 102 regions in CONUS, whether precipitation and temperature over the next 12 months (in three month chunks) are likely to be above or below the 30-year normal climatology.

- Make these forecasts site-specific, and implement 900 simulated futures for each site that are run with SOILWAT2. Get the historical weather for each grid cell in the western CONUS from gridMet, and calculate the climatology over the 30-year normal period. Apply the long-lead anomalies from the forecast region in which the grid cell falls to the local climatology to make the forecast site-specific. 

- Calculate the climatology for a site over the normal period (currently 1991-2020); monthly mean and SD for precipitation and temperature 

- Get the forecasted anomalies over the next 12 months from the NWS CPC forecasts

- CURRENTLY: For each month in the normal period (mean value for each actual month, not the mean monthly value across the normal period -- 30 values for each month, 360 total anomalies), determine the anomaly between the observed temp or precip and forecasted temp/precip for that same month. **I think that this is not correct, because it doesn’t account for the long-term upward trend in temperature over the normal period. The forecasts are relative to the *mean* value over the normal period, so when we compare the forecast to a single year in the normal period, anomalies in the earlier years are more likely to be positive, while anomalies in the later years are more likely to be negative. We should probably do something where we “remove” the trend in temperature before this process… but need to consult w/ someone who knows more

- We don’t get the co-variances of temp and precip from the forecasted data, so we use the above generated historical ‘anomalies’ to estimate the covariance of precip and temperature anomalies for each month. 

- For each lead, use multivariate sampling to draw 30 possible anomalies of temp and precip using the above covariance matrix 

- Then, perform a bias correction for temperature based on the mean (**Note** This may actually solve the problem of the trend in temperature??). We do this by taking the mean of each monthly anomaly across all 30 anomaly sets, then subtracting this mean from each anomaly value. 

- Then, iterate through each of the 30 future anomalies: 

    - Apply the anomaly across the entire historical record ( normal period + intervening time since it ended) 

    - Then, assemble the data for a single SOILWAT2 run: historical data from the previous year prior to the simulation data until the end of that year + data from the start of the current year until the current date + Forecasted data for the rest of the current year and the next year until one year from the current date (the forecasted data is historical data from one year in the normal period with the ‘current’ anomaly applied)

    - Run SOILWAT2 with this set of daily weather data, and retrieve the outputs we want

    - Repeat this process 30 times (each time the ‘current’ anomaly is applied to a given year in the normal period)

    - Use historical and forecasted soil moisture and meteorological information to both hindcast and forecast the germination and establishment of common restoration species in each gridcell. These hindcasts and forecasts are generated using previously established ecological models. We have currently implemented three models that predict germination and/or establishment of big sagebrush (Artemisia tridentata), and will implement models for more common restoration species as they become available.

- This process is repeated for each grid cell in the forecast area, and outputs are saved as both netCDFs and COGs (cloud-optimized geoTIFFs) to generate figures and maps of the simulation output. The entire simulation will be run in parallel on the USGS Hovenweep high performance computer. 

- Repeat this process every month when new NWS long-lead forecasts are released. The simulation will be scheduled to download data and run the workflow, so there will be no interactive coding required to initiate a new run each month. 

- House the simulation outputs online as the Short-Term Drought Forecaster component of the Land Treatment Exploration Tool. Users will select a location, and the website will query saved data from that month’s run to quickly generate figures of forecasted values. Users will also be able to view maps of abiotic and ecological forecasts that span the entire forecast area (the western CONUS). 

## Summary of current status
The current version of code for the gridded forecaster can be found in this public GitHub repository: (https://github.com/DrylandEcology/gridSTDF) 

The repository for the point-based version is in this GitLab repo (https://code.chs.usgs.gov/sbsc/shorttermdroughtforecaster)(is on VPN, unfortunately) (but I don’t think is the version that is deployed by the LTET--need to coordinate with Michelle Jeffiries at FRESC to access that, I think) 

This document has the step-by-step weather logic that is currently deployed in the point-based version, and which is still implemented in the gridded version (https://code.chs.usgs.gov/sbsc/shorttermdroughtforecaster/-/blob/master/Documentation/WeatherLogicStepbyStep.md) 

My current understanding is that we just need to get the simulations running for all of the western CONUS and figure out how to schedule this run every month. Then FRESC will figure out how to retrieve the information they need and do what they need to do to host the information on the LTET. 

## Running Locally
To run the current version of the gridded STDF locally (only functions for a subset of the spatial extent, obviously) (all file names assume you start in the home directory for the gridSTDF repository, as saved on GitHub)

1) run a bash script to get new weather data from online (for days that have passed since last run) 
The script is “/main/DailyDownloadScript.sh” and can be run on the command line with the code  “bash YourPathToGridSTEDFRepository/main/DailyDownloadScript.sh”

2) Run the R script “/main/implementation/01_main-simulation-runner.R.” 

There is an option to ‘toggle’ whether the simulation runs in parallel or in series, which is in line 41 of the script “main/implementation/01_main-simulation-runner.R”

This script calls two scripts internally: 

“/main/implementation/01.1_create-netcdfs.R”, which creates the netCDF output files; and 

“/main/implementation/01.2_input-values-into-ncdfs.R”, which populations the netCDFs with the data resulting from the simulations

We ultimately want the output of certain variables as COGs for visualisation, and script that does that for a test area is “/projects/07_TestOutputForFRESC/CreateCOGs.R”

This script will populate a folder in the /outputs/ folder that will contain netCDFs for each output

## Running on the HPC
I have gotten the gridded STDF to run on Hovenweep in serial, but have not yet been able to get it to run in parallel. Lopaka Lee (rclee@usgs.gov) was really helpful in getting the modules setup on the HPC that we need to use parallel netcdf, and I imagine he’d be helpful in the future for getting it up and running in parallel. 

I’ve been running the gridded STDF on the USGS Hovenweep

A version of the gitHub repo for the gridded STDF is in this location in Caldera: /caldera/hovenweep/projects/usgs/ecosystems/sbsc/DrylandEcologyTeam/gridSTDF/

When running on Hovenweep, the following modules should load all of the packages and software you need (load with the command “module load”:
 module load cray-R-netcdf proj/9.3.0 geos/3.12.1 netcdf/4.9.2

My most recent attempts to get a parallel process to run using a similar setup to the gridded STDF can be found in “/projects/08_ParallelTest/RNetCDF_test.R” (where the R script for the actual test is located), and in "/HPC_Files/", which contains an R script to load packages, shell scripts to run the test R script, and a folder to contain output files. 


## To-Do List (as of 6/30/2026)
- Changes to COGs following 9/16 meeting w/ Michelle 
- Change ‘anomaly’ COGs to just straight future predictions for model outputs 
- Want to make model-outputs more interpretable to the user to help them understand why a given pixel has a prediction different from a nearby predictor 
- For the GISSM model, look at Shchlaepfer 2021 to identify a few variables that the model output is sensitive to 
- make COGs for these variables identified above
- For Shriver model also (looks like it’s driven by elevation, but is largely snow and temp...) 
- Could make a COG for snow also? 
- O’Connor predictions for “moist” vs “very moist” are identical... check that I stored the outputs correctly  
- Change naming the file output to be consistent w/ netCDFs  
- Does the word “Anomaly” mean something very specific in meteorological terms? Should we be careful about how we use this term?  
- We should probably axe maps of VWC  
- Make netCDF files (one for each model output?) that have the relevant input variables for each model  
- Maybe show uncertainty (if we show it at all) only for model forecasts 
- Test writing COGs during later months (months that overlap w/ “fall” and “growing season” months)
- Determine whether we want to keep current implementation of ‘growing season’ and ‘fall’ in the COG generation
- Add a check to make sure that the downloaded meteorological and anomaly datasets are correct for the current month’s run
- Double-check the way that NAs are stored in the netCDFs (should really only be an issue for deep soil layers)
- Update the years included in the ‘historical’ ecological predictions. The gridded version (and point-based version also) is currently hard-coded to be 32 years, but should be adaptable to include the 30 years of the normal period + the intervening number of years prior to the current year 
- Make sure that the GISSM predictions saved as current year and next year are predicting the germination in the year *following* the year indicated in the date
- Clarify the general timeline/dates for predicted values (why is there a gap between the current date and the date when forecasted values start?) 
- The temp and precip “diff” netCDFs (the ones that show how the predicted median values differ from the values from the normal period) are not storing the values correctly in space--just storing one value for the entire area… need to fix, obviously
