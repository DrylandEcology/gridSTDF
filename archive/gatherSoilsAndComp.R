

set_soils <- function(sw_in, LatIdx, LonIdx){
  
  Soils <- sw_in@soils@Layers

  soilsDir <- 'main/Data/Soils'
  soilsInfo <- read.csv(file.path(soilsDir, 'netCDF_metadata_soils.csv'))
  soilsNCs <- soilsInfo[, 'filename_template_netCDF']
  
  depth <- RNetCDF::open.nc(file.path(soilsDir, 'slthick_fx_SOILWAT2_wUS-gm_gn.nc'))
  depthData <- var.get.nc(depth, variable = 'slthick', start = c(LatIdx, LonIdx, 1), 
                                  count = c(1, 1, 12))
  
  Soils <- do.call("rbind", replicate(length(depthData), Soils, 
                                      simplify = FALSE))
  Soils[,'depth_cm'] <- depthData * 100
  
  for(s in seq(soilsNCs)) {
    dat <- RNetCDF::open.nc(file.path(soilsDir, soilsNCs[s]))
    
    var <- soilsInfo[s, 'id_netCDF']
    name <- soilsInfo[s, 'sw_variable_name']
    idx <- which(dimnames(Soils)[[2]] %in% name)
    
    # get data
    siteData <- var.get.nc(dat, variable = var, 
                           start = c(LatIdx, LonIdx, 1), 
                           count = c(1, 1, 12))
    
    Soils[,name] <- siteData
  }

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
