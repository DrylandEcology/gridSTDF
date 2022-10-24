nc_clip <- function (ff, vars = NULL, lon_range = c(-180, 180), lat_range = c(-90, 
                                                                   90), vert_range = NULL, date_range = NULL, months = NULL, 
          years = NULL, out_file = NULL, cdo_output = FALSE, zip_file = FALSE, 
          overwrite = FALSE) 
{
  if (!is.null(out_file) & file.exists(out_file) & overwrite == 
      FALSE) 
    return("out_file already exists and overwrite = FALSE. Functioning exiting")
  ff_orig <- normalizePath(ff)
  holding_nc <- ff_orig
  if (!rcdo:::file_valid(ff)) {
    stop(stringr::str_glue("error: {ff} does not exist or is not netcdf"))
  }
  if (!is.null(vars)) {
    var_list <- stringr::str_flatten(nc_variables(ff), collapse = " ")
    for (vv in vars) {
      if (vv %in% nc_variables(ff) == FALSE) {
        stop(stringr::str_glue("variable {vv} does not appear to be in the file. Available variables are {var_list}"))
      }
    }
  }
  if (!is.numeric(lon_range) | !is.numeric(lat_range)) {
    stop("lon or lat ranges are not numeric")
  }
  if (length(lon_range) != 2 | length(lat_range) != 2) {
    stop("lon or lat range is not a 2 value numeric")
  }
  if (lon_range[1] > lon_range[2]) {
    stop("lon_range is not valid")
  }
  if (lat_range[1] > lat_range[2]) {
    stop("lat_range is not valid")
  }
  if (!is.null(vert_range)) {
    if (!is.numeric(vert_range)) {
      stop("error: vert_range is not numeric")
    }
    if (vert_range[2] < vert_range[1]) {
      stop("error: vert_range is not valid")
    }
  }
  if (!is.null(years)) {
    if (!is.numeric(years)) 
      stop("error: years is not numeric")
  }
  if (!is.integer(years) & !is.null(years)) 
    years <- as.integer(years)
  if (!is.null(months)) {
    if (!is.numeric(months)) 
      stop("error: months is not numeric")
    if (!is.integer(months)) 
      months <- as.integer(months)
    valid_months <- 1:12
    if ((sum(months %in% valid_months) == length(months)) == 
        FALSE) 
      stop("error: months supplied are not actual months")
  }
  if (!is.null(vars)) {
    if (!is.character(vars)) {
      stop("error: vars is not a character string")
    }
  }
  if (as.integer(system(stringr::str_c("cdo ngrids ", ff), 
                        ignore.stderr = (cdo_output == FALSE), intern = TRUE)) > 
      1) {
    warning("warning: there is more than one horizontal grid in the netcdf file. Please check the output!")
  }
  init_dir <- getwd()
  on.exit(setwd(init_dir))
  temp_dir <- rcdo:::random_temp()
  setwd(temp_dir)
  if (getwd() == init_dir) {
    stop("error: there was a problem changing the directory")
  }
  temp_dir <- stringr::str_c(temp_dir, "/")
  if (!rcdo:::cdo_compatible(ff_orig)) {
    file.copy(ff_orig, "temp.nc")
    holding_nc <- "temp.nc"
    add_missing_grid(holding_nc, vars)
  }
  if (!is.null(vars)) {
    var_select <- stringr::str_flatten(vars, ",")
    system(stringr::str_glue("cdo selname,{var_select} {holding_nc} dummy.nc"), 
           ignore.stderr = (cdo_output == FALSE))
    if (!file.exists("dummy.nc")) {
      stop("error: cdo cannot subselect vars chosen. Set cdo_output = TRUE and inspect output.")
    }
    if (holding_nc == ff_orig) {
      holding_nc <- "temp.nc"
    }
    file.rename("dummy.nc", holding_nc)
  }
  lat_box <- stringr::str_flatten(c(lon_range, lat_range), 
                                  collapse = ",")
  system(stringr::str_glue("cdo sellonlatbox,{lat_box} {holding_nc} dummy.nc"), 
         ignore.stderr = (cdo_output == FALSE))
  if (!file.exists("dummy.nc")) {
    stop("error: cdo cannot subselect the lonlat box. Set cdo_output = TRUE and inspect output.")
  }
  if (holding_nc == ff_orig) {
    holding_nc <- "temp.nc"
  }
  file.rename("dummy.nc", holding_nc)
  if (!is.null(vert_range)) {
    depths <- nc_depths(ff)$Depth
    depths <- depths[complete.cases(depths)]
    depths <- depths[depths <= vert_range[2] & depths >= 
                       vert_range[1]]
  }
  if (!is.null(date_range)) {
    min_date <- lubridate::dmy(date_range[1])
    max_date <- lubridate::dmy(date_range[2])
    if (is.na(min_date) | is.na(max_date)) {
      stop("error check date range supplied")
    }
    system(stringr::str_c("cdo seldate,{min_date},{max_date} {holding_nc} dummy.nc"), 
           ignore.stderr = (cdo_output == FALSE))
    if (!file.exists("dummy.nc")) {
      stop("error: cdo cannot subselect the dates. Set cdo_output = TRUE and inspect output.")
    }
    if (holding_nc == ff_orig) {
      holding_nc <- "temp.nc"
    }
    file.rename("dummy.nc", holding_nc)
  }
  if (!is.null(vert_range)) {
    if (length(depths) == 0) {
      stop("error: no depths within the depth range selected")
    }
    levels_selected <- stringr::str_flatten(depths, ",")
    system(stringr::str_glue("cdo sellevel,{levels_selected} {holding_nc} dummy.nc"), 
           ignore.stderr = (cdo_output == FALSE))
    if (!file.exists("dummy.nc")) {
      stop("error: cdo cannot subselect the vertical levels. Set cdo_output = TRUE and inspect output.")
    }
    if (holding_nc == ff_orig) {
      holding_nc <- "temp.nc"
    }
    file.rename("dummy.nc", holding_nc)
  }
  if (!is.null(months)) {
    file_months <- system(stringr::str_c("cdo showmon ", 
                                         holding_nc), intern = TRUE, ignore.stderr = (cdo_output == 
                                                                                        FALSE)) %>% stringr::str_split(" ") %>% .[[1]] %>% 
      as.integer()
    num_months <- 0
    for (mm in months) {
      if (mm %in% unique(file_months[complete.cases(file_months)])) {
        num_months <- num_months + 1
      }
    }
    if (num_months == 0) {
      stop("error: check months supplied")
    }
    month_choice <- stringr::str_flatten(months, ",")
    system(stringr::str_glue("cdo selmonth,{month_choice} {holding_nc} dummy.nc"), 
           ignore.stderr = (cdo_output == FALSE))
    if (!file.exists("dummy.nc")) {
      stop("error: cdo cannot subselect the months. Set cdo_output = TRUE and inspect output.")
    }
    if (holding_nc == ff_orig) {
      holding_nc <- "temp.nc"
    }
    file.rename("dummy.nc", holding_nc)
  }
  if (!is.null(years)) {
    file_years <- system(stringr::str_c("cdo showyear ", 
                                        holding_nc), intern = TRUE, ignore.stderr = (cdo_output == 
                                                                                       FALSE)) %>% stringr::str_split(" ") %>% .[[1]] %>% 
      as.integer()
    num_years <- 0
    for (yy in years) {
      if (yy %in% unique(file_years[complete.cases(file_years)])) {
        num_years <- num_years + 1
      }
    }
    if (num_years == 0) {
      stop("error: check years supplied")
    }
    year_choice <- stringr::str_flatten(years, ",")
    system(stringr::str_glue("cdo selyear,{year_choice} {holding_nc} dummy.nc"), 
           ignore.stderr = (cdo_output == FALSE))
    if (!file.exists("dummy.nc")) {
      stop("error: cdo cannot subselect the years. Set cdo_output = TRUE and inspect output.")
    }
    if (holding_nc == ff_orig) {
      holding_nc <- "temp.nc"
    }
    file.rename("dummy.nc", holding_nc)
  }
  if (is.null(out_file)) {
    if (is.null(vert_range)) 
      depths = 0
    nc_grid <- nc_read(holding_nc)
    if (length(depths) == 1) {
      nc_grid <- nc_grid %>% dplyr::mutate(Depth = depths[1])
    }
    setwd(temp_dir)
    if (length(dir(temp_dir)) < 6 & temp_dir != init_dir) {
      unlink(temp_dir, recursive = TRUE)
    }
    return(nc_grid)
  }
  if (zip_file) {
    nc_zip(holding_nc, overwrite = TRUE)
  }
  setwd(init_dir)
  file.copy(stringr::str_c(temp_dir, holding_nc), out_file, 
            overwrite = overwrite)
  setwd(temp_dir)
  if (length(dir(temp_dir)) < 6 & temp_dir != init_dir) {
    unlink(temp_dir, recursive = TRUE)
  }
}