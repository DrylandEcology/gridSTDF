setVeg <- function(sw_in, AllProdInfo, i) {

  OneSiteProdInfo <- AllProdInfo[i,]
  
  # ### Fractional land cover (vegetation composition)
  Rel_Abundance_L1 <- OneSiteProdInfo[,2:6]
  
  ids <- sapply(
    X = names(rSOILWAT2::swProd_Composition(sw_in)),
    FUN = function(x) {
      grep(
        pattern = substr(x, 1, 4),
        x = names(Rel_Abundance_L1),
        ignore.case = TRUE
      )
    }
   )
  rSOILWAT2::swProd_Composition(sw_in) <- as.numeric(Rel_Abundance_L1)[ids]
  
  # # Assign monthly biomass values to rSOILWAT2 input object
  # # Note: monthly biomass values of forbs, trees, etc. need to be estimated
  v2 <- c("Litter", "Biomass", "Live_pct")
   
  grassProdCols <- grep('Grass_Litter|Grass_Biomass|Grass_Fra', names(OneSiteProdInfo))
  grass <- as.numeric(matrix(OneSiteProdInfo[,min(grassProdCols):max(grassProdCols)], ncol = 3))
  rSOILWAT2::swProd_MonProd_grass(sw_in)[, v2] <- grass
   
  shrubProdCols <- grep('Shrub_Litter|Shrub_Biomass|Shrub_Fra', names(OneSiteProdInfo))
  shrub <- as.numeric(matrix(OneSiteProdInfo[,min(shrubProdCols):max(shrubProdCols)], ncol = 3))
  rSOILWAT2::swProd_MonProd_shrub(sw_in)[, v2] <- shrub
   
  # treeProdCols <- grep('Tree_Litter|Tree_Biomass|Tree_Fra', names(OneSiteProdInfo))
  # tree <- as.numeric(matrix(OneSiteProdInfo[,min(treeProdCols):max(treeProdCols)], ncol = 3))
  # rSOILWAT2::swProd_MonProd_tree(sw_in)[, v2] <- tree
  #  
  # forbProdCols <- grep('Forb_Litter|Forb_Biomass|Forb_Fra', names(OneSiteProdInfo))
  # forb <- as.numeric(matrix(OneSiteProdInfo[,min(forbProdCols):max(forbProdCols)], ncol = 3))
  # rSOILWAT2::swProd_MonProd_forb(sw_in)[, v2] <- forb

  return(sw_in)
  
}

setSW <- function(sw_in, Lat, Long, calc_SiteClimate) {
  #print(paste('Setting SW parameters', Sys.time()))
    
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

  ##Rooting profiles of vegetation types
  #Select rooting profile types
  #Set those to "FILL" where cover == 0 (because of transpiration regions)
  veg_cover <- sw_in@prod@Composition
  
  trco_type_by_veg <- list(
    grass_annuals = if (
      veg_cover[["Grasses"]] > 0
    ) {
      "Jacksonetal1996_crops"
    } else {
      "FILL"
    },
    shrub = if (veg_cover[["Shrubs"]] > 0) {
      "SchenkJackson2003_PCdry_shrubs"
    } else {
      "FILL"
    },
    forb = if (veg_cover[["Forbs"]] > 0) {
      "SchenkJackson2003_PCdry_forbs"
    } else {
      "FILL"
    },
    tree = if (veg_cover[["Trees"]] > 0) {
      "Bradfordetal2014_LodgepolePine"
    } else {
      "FILL"
    }
  )


  grass_annuals <- veg_cover[["Grasses"]]
  names(grass_annuals) <- "grass_annuals"
  
  veg_roots <- rSOILWAT2::estimate_PotNatVeg_roots(
    layers_depth = soil_new[, "depth_cm"],
    trco_type_by_veg = trco_type_by_veg,
    trco_adj_by_veg = list(grass_annuals = "positive",  shrub = "positive", 
                           forb = "positive", tree = "positive"),
    fgrass_c3c4ann = grass_annuals
  )

  v1 <- c("Grass", "Shrub", "Tree", "Forb")
  v2 <- paste0("transp", v1, "_frac")
  soil_new[, v2] <- veg_roots[, v1]

  rSOILWAT2::swSoils_Layers(sw_in) <- data.matrix(soil_new)

  #Prepare transpiration regions based on soil layers ---------------------------
  #Adjust to your specific soil depths,
  #e.g., if extracted from NRCS SDA without updating soil layers
  #tr_example <- c(1, 1, 2, 3, 3)

  #Values corresponding to `CONUSSOIL_BSE_EVERY10cm` of "rSFSW2":
  tr_lyrs_10cm <- c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3)

  tr <- rSOILWAT2::prepare_TranspirationRegions(tr_lyrs = tr_lyrs_10cm)
  rSOILWAT2::swSite_TranspirationRegions(sw_in) <- data.matrix(tr)

  #Make necessary adjustments based on soil depth and rooting profiles
  rSOILWAT2::swSite_TranspirationRegions(sw_in) <-
    rSOILWAT2::adjust_TranspirationRegions(sw_in)

  return(sw_in)
}

