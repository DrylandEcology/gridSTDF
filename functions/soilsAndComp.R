
set_soils <- function(sw_in, soils, sand, clay){
  
  if(soils == 2){
    sw_in@soils@Layers[,9] <- sand/100
    sw_in@soils@Layers[,10] <- clay/100
  }
  
  # always have 10 layers - 5, 10, 15, 20, 30, 40, 60, 80, 100, 150
  
  Soils <- sw_in@soils@Layers
  Soils <- rbind(Soils[1:2,], Soils[3,], Soils[3:8,], Soils[8,] )
  Soils[c(3,10),'depth_cm'] <- c(15, 150)
  sw_in@soils@Layers <- Soils
  
  return(sw_in)
  
}

set_comp <- function(sw_in, weath){
  
    clim1 <- calc_SiteClimate(weatherList = weath, do_C4vars = TRUE)
  
    comp <- rSOILWAT2::estimate_PotNatVeg_composition(
        MAP_mm = 10 * clim1[["MAP_cm"]], MAT_C = clim1[["MAT_C"]],
        mean_monthly_ppt_mm = 10 * clim1[["meanMonthlyPPTcm"]],
        mean_monthly_Temp_C = clim1[["meanMonthlyTempC"]]
      )
    
    swProd_Composition(sw_in) <- c(comp$Rel_Abundance_L1[4],
                                    comp$Rel_Abundance_L1[2],
                                    comp$Rel_Abundance_L1[1],
                                    comp$Rel_Abundance_L1[3],
                                    comp$Rel_Abundance_L1[5])
  return(sw_in)
  
}

#x <- set_soils(sw_in0, 2, .33, .26)
#x@soils
