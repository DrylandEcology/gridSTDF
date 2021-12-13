# Set up workflow for grabbing static inputs
# AND generate some static inputs

# # # # # Static inputs include:
# # # # # # historical weather (Still 1980 - 2010) - obtained ... ACTUALLY NEED TO UPDATE TO 1990-2021
# # # # # # Soils  - 
# # # # # Vegetation - relies on climate. Can be static
# # # # # Transpiration - reliest on veg and soils
# # # # # Bare soil evaporation coefficient - depends on soil
# # # # # Sky 
# # # # # Elevation (Caitlin)
# # # # # CO2 - Now set to true :)
# # # # # # # For the next year - use current C02, and in the past use the varying CO2

# ---------------------------------------------------------------------------- #
# historical climate --------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# 1 - Get SQL DB from Yeti
# 2 - Code that unzips and then formats this data
library(DBI)
library(RSQLite)
library(rSOILWAT2)

con <- DBI::dbDriver('SQLite')
weatherDB <- dbConnect(con, "Data/dbWeatherData_WesternUS_gridMET_historical.sqlite3")


sql <- "SELECT data FROM WeatherData WHERE Site_id = :x1 AND Scenario = 1"
res <- DBI::dbGetQuery(weatherDB, sql, params = list(x1 = 1))[1, 1]

if (is.na(res) || all(lengths(res) == 0)) {
  stop(paste("Weather data for site", shQuote(IDs[["site_id"]]), 
             "and scenario", shQuote(IDs[["scenario_id"]]), "does not exist in weather database."))
}

wd <- try(dbW_blob_to_weatherData(res))

if (inherits(wd, "try-error")) {
  stop(paste("Weather data for site", shQuote(IDs[["site_id"]]), 
             "and scenario", shQuote(IDs[["scenario_id"]]), "is corrupted."))
}
temp <- class(wd[[1]])
if (!(attr(temp, "package") == "rSOILWAT2")) {
  message(paste("WARNING: The class of the extracted weather data object is", 
                shQuote(temp), "from package", shQuote(attr(temp, 
                                                            "package")), "which is", "outdated. Please, upgrade weather database with function", 
                "'dbW_upgrade_to_rSOILWAT2'."))
}

years <- rSOILWAT2::get_years_from_weatherData(wd)
ids <- rSOILWAT2:::select_years(years, 1981, 2011)
wd <- wd[ids]

# ---------------------------------------------------------------------------- #
# veg ------------------------------------------------------------------------ #
# ---------------------------------------------------------------------------- #

rSOILWAT2::swProd_Composition(sw_in4) <- c(0.4, 0.6, 0, 0, 0)
estimate_PotNatVeg_composition

# ---------------------------------------------------------------------------- #
# transpiration -------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
SoilLayerWidth <- c(0, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200)


# ---------------------------------------------------------------------------- #
# Sky ------------------------------------------------------------------------ #
# ---------------------------------------------------------------------------- #
# Daniel will generate

# ---------------------------------------------------------------------------- #
# 

