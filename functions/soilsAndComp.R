
set_soils <- function(sw_in, soils, sand, clay){
  
  if(soils == 2){
    sw_in@soils@Layers[,9] <- sand/100
    sw_in@soils@Layers[,10] <- clay/100
  }
  
  return(sw_in)
  
}

set_comp <- function(sw_in, comp, trees, shrubs, grasses, forbs, bg){
  
  if(comp == 2){
    sw_in@prod@Composition[1] <- grasses
    sw_in@prod@Composition[2] <- shrubs
    sw_in@prod@Composition[3] <- trees
    sw_in@prod@Composition[4] <- forbs
    sw_in@prod@Composition[5] <- bg
  }
  if(comp == 1){
    # MAP_mm <- SiteClimate_Scenario$MAP_cm*10
    # MAT_C <- SiteClimate_Scenario$MAT_C
    # monthly.ppt <- SiteClimate_Scenario$meanMonthlyPPTcm*10
    # monthly.temp <- SiteClimate_Scenario$meanMonthlyTempC
    # dailyC4vars <- SiteClimate_Scenario$dailyC4vars
    # rSFSW2::estimate_PotNatVeg_composition(MAP_mm, MAT_C,
    #                                        mean_monthly_ppt_mm = monthly.ppt, dailyC4vars)
    # 
    # rSFSW2::estimate_PotNatVeg_biomass( tr_VegBiom = tr_VegetationComposition,
    #                                      do_adjBiom_by_temp = any(create_treatments == "AdjMonthlyBioMass_Temperature") && i_sw_input_treatments$AdjMonthlyBioMass_Temperature,
    #                                      do_adjBiom_by_ppt = any(create_treatments == "AdjMonthlyBioMass_Precipitation") & i_sw_input_treatments$AdjMonthlyBioMass_Precipitation,
    #                                      fgrass_c3c4ann = grasses.c3c4ann.fractions[[sc]],
    #                                      growing_limit_C = opt_sim[["growseason_Tlimit_C"]],
    #                                      isNorth = isNorth, MAP_mm = MAP_mm, mean_monthly_temp_C = monthly.temp)
    # 
    # rSOILWAT2::swProd_MonProd_grass(swRunScenariosData[[sc]])[, 1:3] <- temp$grass[, 1:3]
    # rSOILWAT2::swProd_MonProd_shrub(swRunScenariosData[[sc]])[, 1:3] <- temp$shrub[, 1:3]
    
  }
  
  return(sw_in)
  
}

#x <- set_soils(sw_in0, 2, .33, .26)
#x@soils
