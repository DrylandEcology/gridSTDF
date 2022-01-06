setSW <- function(Lat, Long) {
  print(paste('Setting SW parameters', Sys.time()))
  
  sw_in <- new("swInputData") # baseline data
  
  rSOILWAT2::swYears_StartYear(sw_in) <- 0
  rSOILWAT2::swYears_EndYear(sw_in) <- 2022
  rSOILWAT2::swYears_StartYear(sw_in) <- 1991
  
  # Specify geographic location of site
  rSOILWAT2::swSite_IntrinsicSiteParams(sw_in) <- c(
    Longitude = Long,
    Latitude = Lat,
    Altitude = 1000,
    Slope = 0,
    Aspect = NA
  )
  
  # # CO2 ------------------------------------------------------------------------
  # Name of the CO2 concentration scenario
  co2_nametag <- "RCP85"
  
  # Obtain yearly values from look-up table
  co2_data <- rSOILWAT2::lookup_annual_CO2a(
    start = rSOILWAT2::swYears_StartYear(sw_in),
    end = rSOILWAT2::swYears_EndYear(sw_in)+1,
    name_co2 = co2_nametag
  )
  
  # Assign CO2 values to rSOILWAT2 input object
  rSOILWAT2::swCarbon_Scenario(sw_in) <- co2_nametag
  rSOILWAT2::swCarbon_CO2ppm(sw_in) <- data.matrix(co2_data)
  
  ## Climate inputs for Penman's potential evapotranspiration -----------------
  
  rH <- rep(50, 12) # relative humidity [%]
  ws <- rep(2, 12) # wind speed [m/s]
  sc <- rep(30, 12) # sky cover [%]
  
  # Assign monthly climate normals to rSOILWAT2 input object
  rSOILWAT2::swCloud_Humidity(sw_in) <- rH
  rSOILWAT2::swCloud_WindSpeed(sw_in) <- ws
  rSOILWAT2::swCloud_SkyCover(sw_in) <- sc
  
  ## Soils  --------------------------------------------------------------------
  soils_fixed <- data.frame(
    depth = c(5, 10, 20, 50),
    bulkd = 1.3,
    gravel = 0.1,
    evco = NA,
    trco_grass = NA,
    trco_shrub = NA,
    trco_tree = NA,
    trco_forb = NA,
    sand = 0.65,
    clay = 0.05,
    impermeability = NA,
    soil_temp = NA
  )
  
  soil_new <- data.frame(rSOILWAT2::swSoils_Layers(sw_in)[0, ])
  soil_new[seq_len(nrow(soils_fixed)), ] <- soils_fixed
  soil_new[, "impermeability_frac"] <- 0
  
  ##  Potential bare-soil evaporation rates --------------------------------------
  if (requireNamespace("rSW2data")) {
    soil_new[, "EvapBareSoil_frac"] <- rSW2data::calc_BareSoilEvapCoefs(
      layers_depth = soil_new[, "depth_cm"],
      sand = soil_new[, "sand_frac"],
      clay = soil_new[, "clay_frac"]
    )[1, ]
  }
  
  ## 
  ### Soil temperature parameters and initial profile values ---------------------
  soil_new[, "soilTemp_c"] <- rSW2data::init_soiltemperature(
    layers_depth = soil_new[, "depth_cm"],
    # Estimated soil surface temperature on Jan 1
    # (potentially underneath snow pack)
    Tsoil_upper = max(-1, mean(clim[["meanMonthlyTempC"]][c(1, 12)])),
    # Constant soil temperature (Celsius) at the lower boundary (max depth)
    # approximated by mean annual air temperature
    Tsoil_const = mean(clim[["meanMonthlyTempC"]]),
    depth_Tsoil_const = 990
  )
  
  ## Vegetation inputs
  
  ### Fractional land cover (vegetation composition)
  veg_cover <- rSOILWAT2::estimate_PotNatVeg_composition(
    MAP_mm = 10 * clim[["MAP_cm"]],
    MAT_C = clim[["MAT_C"]],
    mean_monthly_ppt_mm = 10 * clim[["meanMonthlyPPTcm"]],
    mean_monthly_Temp_C = clim[["meanMonthlyTempC"]],
    dailyC4vars = clim[["dailyC4vars"]]
  )
  
  ids <- sapply(
    X = names(rSOILWAT2::swProd_Composition(sw_in)),
    FUN = function(x) {
      grep(
        pattern = substr(x, 1, 4),
        x = names(veg_cover[["Rel_Abundance_L1"]]),
        ignore.case = TRUE
      )
    }
  )
  
  # Assign fractional cover values to rSOILWAT2 input object
  rSOILWAT2::swProd_Composition(sw_in) <- veg_cover[["Rel_Abundance_L1"]][ids]
  
  ### Biomass amount and phenology (for shrubs and grasses)
  # Reference biomass values from Bradford et al. 2014 are used
  # Mean monthly reference temperature corresponding to default phenology values
  # for the median across 898 big sagebrush sites are used
  veg_biom <- rSOILWAT2::estimate_PotNatVeg_biomass(
    target_temp = clim[["meanMonthlyTempC"]],
    target_MAP_mm = 10 * clim[["MAP_cm"]],
    do_adjust_phenology = TRUE,
    do_adjust_biomass = TRUE,
    fgrass_c3c4ann = veg_cover[["Grasses"]]
  )
  
  # Assign monthly biomass values to rSOILWAT2 input object
  # Note: monthly biomass values of forbs, trees, etc. need to be estimated
  v1 <- c("Litter", "Biomass", "Perc.Live")
  v2 <- c("Litter", "Biomass", "Live_pct")
  rSOILWAT2::swProd_MonProd_grass(sw_in)[, v2] <- veg_biom[["grass"]][, v1]
  rSOILWAT2::swProd_MonProd_shrub(sw_in)[, v2] <- veg_biom[["shrub"]][, v1]
  
  
  ### Rooting profiles of vegetation types
  # Select rooting profile types
  # Set those to "FILL" where cover == 0 (because of transpiration regions)
  trco_type_by_veg <- list(
    grass_C3 = if (veg_cover[["Rel_Abundance_L0"]][["Grasses_C3"]] > 0) {
      "SchenkJackson2003_PCdry_grasses"
    } else {
      "FILL"
    },
    grass_C4 = if (veg_cover[["Rel_Abundance_L0"]][["Grasses_C4"]] > 0) {
      "SchenkJackson2003_PCdry_grasses"
    } else {
      "FILL"
    },
    grass_annuals = if (
      veg_cover[["Rel_Abundance_L0"]][["Grasses_Annuals"]] > 0
    ) {
      "Jacksonetal1996_crops"
    } else {
      "FILL"
    },
    shrub = if (veg_cover[["Rel_Abundance_L0"]][["Shrubs"]] > 0) {
      "SchenkJackson2003_PCdry_shrubs"
    } else {
      "FILL"
    },
    forb = if (veg_cover[["Rel_Abundance_L0"]][["Forbs"]] > 0) {
      "SchenkJackson2003_PCdry_forbs"
    } else {
      "FILL"
    },
    tree = if (veg_cover[["Rel_Abundance_L0"]][["Trees"]] > 0) {
      "Bradfordetal2014_LodgepolePine"
    } else {
      "FILL"
    }
  )
  
  veg_roots <- rSOILWAT2::estimate_PotNatVeg_roots(
    layers_depth = soil_new[, "depth_cm"],
    trco_type_by_veg = trco_type_by_veg,
    fgrass_c3c4ann = veg_cover[["Grasses"]]
  )
  
  v1 <- c("Grass", "Shrub", "Tree", "Forb")
  v2 <- paste0("transp", v1, "_frac")
  soil_new[, v2] <- veg_roots[, v1]
  
  rSOILWAT2::swSoils_Layers(sw_in) <- data.matrix(soil_new)
  
  
  # Prepare transpiration regions based on soil layers ---------------------------
  # Adjust to your specific soil depths,
  # e.g., if extracted from NRCS SDA without updating soil layers
  tr_example <- c(1, 1, 2, 3, 3)
  
  # Values corresponding to `CONUSSOIL_BSE_EVERY10cm` of "rSFSW2":
  tr_lyrs_10cm <- c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3)
  
  tr <- rSOILWAT2::prepare_TranspirationRegions(tr_lyrs = tr_lyrs_10cm)
  rSOILWAT2::swSite_TranspirationRegions(sw_in) <- data.matrix(tr)
  
  # Make necessary adjustments based on soil depth and rooting profiles
  rSOILWAT2::swSite_TranspirationRegions(sw_in) <-
    rSOILWAT2::adjust_TranspirationRegions(sw_in)
  
  return(sw_in)
}