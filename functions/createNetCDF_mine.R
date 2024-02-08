# the below function is an adaptation of the create_netCDF function from rSW2st
createNetCDF_mine <- function (filename, 
                               xyspace, 
                               data = NULL, 
                               data_str = c("xyzt","xyt", "xyz", "xy", "szt", "st", "sz", "s"), 
                               data_dims = get_data_dims(data_str,dim(data)), 
                               data_type = c("double", "float", "integer", "short", "byte", "char"), 
                               var_attributes = list(name = "", standard_name = "",cell_measures = ""), 
                               xy_attributes = list(name = c("lon", "lat"), 
                                                    standard_name = c("longitude", "latitude"), 
                                                    long_name = c("Longitude", "Latitude"), 
                                                    units = c("degrees_east", "degrees_north")),
                               crs_attributes = list(crs_wkt = sf::st_crs("OGC:CRS84")$Wkt,
                                                     grid_mapping_name = "latitude_longitude", 
                                                     longitude_of_prime_meridian = 0,
                                                     semi_major_axis = 6378137, 
                                                     inverse_flattening = 298.257223563),
                               check_crs = TRUE, 
                               time_values = NULL, 
                               type_timeaxis = c("timeseries","climatology"), 
                               time_attributes = list(units = "days since 1900-01-01",
                                                      calendar = "standard", unlim = FALSE), 
                               time_bounds = matrix(NA,  
                                                    nrow = length(time_values), 
                                                    ncol = 2),
                               vertical_values = NULL, 
                               vertical_attributes = list(units = "", 
                                                          positive = "down"),
                               vertical_bounds = matrix(NA, 
                                                        nrow = length(vertical_values),
                                                        ncol = 2), 
                               global_attributes = list(title = "Title"), 
                               overwrite = FALSE, 
                               nc_compression = FALSE, 
                               nc_shuffle = TRUE, 
                               nc_deflate = 5, 
                               nc_chunks = "by_zt", 
                               verbose = FALSE,
                               isParallel = FALSE) 
{
  # determine if required packages are loaded
  stopifnot(requireNamespace("ncdf4"), requireNamespace("RNetCDF"))
  # does it have compression??
  has_compression <- isTRUE(nc_compression)
  # the extent to which you want the netCDF to be compressed? 
  stopifnot(nc_deflate %in% c(NA, 1:9))
  nc_deflate <- if (has_compression) {
    nc_deflate
  } else {NA}
  # Other arguments to do with compression? default is "by_zt"
  has_chunks <- has_compression && !is.na(nc_chunks)
  has_predet_chunks <- is.character(nc_chunks)
  if (has_compression && has_predet_chunks) {
    stopifnot(nc_chunks %in% c("by_zt", "by_t"))
  }
  nc_shuffle <- has_compression && isTRUE(nc_shuffle) && data_type %in% 
    c("integer", "short")
  if (file.exists(filename)) {
    if (overwrite) {
      unlink(filename)
    }
    else {
      warning("File ", shQuote(basename(filename)), " exists and 'overwrite' is FALSE; returning early.")
      return(invisible(FALSE))
    }
  }
  has_data <- !missing(data) && !is.null(data)
  data_type <- match.arg(data_type)
  NAflag <- switch(data_type, char = NULL, byte = NULL, short = -128L, 
                   integer = -2147483647L, float = -3.4e+38, double = -1.7e+308)
  data_str <- match.arg(data_str)
  is_gridded <- startsWith(data_str, "xy")
  if (has_data) {
    data_dims_from_data <- rSW2st::get_data_dims(data_str, dim(data))
  }
  # get rid of this check, we'll always 
  # if (is.null(data_dims)) {
  #   data_dims <- data_dims_from_data
  # } else {
  #   data_dims <- lapply(data_dims, function(x) if (is.null(x) || 
  #                                                  !is.finite(x)) 
  #     NA
  #     else x)
  #   data_dims <- unlist(data_dims)
  #   if (has_data && !isTRUE(all.equal(data_dims_from_data, 
  #                                     data_dims[names(data_dims_from_data)]))) {
  #     stop("Disagreement in dimensions between `data_dims` and `data`.")
  #   }
  # }
  stopifnot(c("ns", "nx", "ny", "nt", "nz", "nv") %in% names(data_dims), 
            !anyNA(data_dims), data_dims[["ns"]] > 0 || data_dims[["nx"]] > 
              0 && data_dims[["ny"]] > 0)
  tmp <- switch(EXPR = data_str, xyzt = stopifnot(data_dims[["nt"]] > 0, 
                                                  data_dims[["nz"]] > 0, 
                                                  data_dims[["nv"]] == 0),
                xyt = stopifnot(data_dims[["nt"]] > 0, 
                                data_dims[["nz"]] == 0, 
                                data_dims[["nv"]] == 0), 
                xyz = stopifnot(data_dims[["nt"]] == 0, 
                                data_dims[["nz"]] > 0, 
                                data_dims[["nv"]] == 0), 
                xy = stopifnot(data_dims[["nt"]] ==  0, 
                               data_dims[["nz"]] == 0, 
                               data_dims[["nv"]] >= 0))
  if (is.null(xyspace) || missing(xyspace)) {
    stop("Must provide `xyspace` as argument.")
  }
  if (any(lengths(xy_attributes) != 2)) {
    stop("All `xy_attributes` must be of lenght two (xy-space).")
  }
  tmp_xy_atts <- c("name", "standard_name", "long_name", "units")
  if (!all(tmp_xy_atts %in% names(xy_attributes))) {
    stop("`xy_attributes` must include ", toString(shQuote(tmp_xy_atts)))
  }
  if ("axis" %in% names(xy_attributes)) {
    if (any(c("X", "Y") != toupper(xy_attributes[["axis"]]))) {
      stop("`xy_attributes`: if `axis` is included, then its value must be ", 
           "`X` and `Y`.")
    }
    xy_attributes[["axis"]] <- NULL
  }
  if ("crs_wkt" %in% names(crs_attributes)) {
    crs_wkt <- crs_attributes[["crs_wkt"]]
    crs_attributes[["crs_wkt"]] <- NULL
    crs_wkt <- try(sf::st_crs(crs_wkt), silent = TRUE)
    if (!inherits(crs_wkt, "crs") || crs_wkt == sf::NA_crs_) {
      stop("`crs_attributes[[\"crs_wkt\"]]` does not represent a valid CRS.")
    }
  }
  else {
    stop("Need `crs_attributes[[\"crs_wkt\"]]`")
  }
  if (!("grid_mapping_name" %in% names(crs_attributes))) {
    stop("Need `crs_attributes[[\"grid_mapping_name\"]]`")
  }
  ns_att_crs <- names(crs_attributes)
  crs_xyspace <- try(suppressWarnings(sf::st_crs(xyspace)), 
                     silent = TRUE)
  if (!inherits(crs_xyspace, "crs") || crs_xyspace == sf::NA_crs_) {
    crs_xyspace <- crs_wkt
    if (verbose) {
      message("No `crs` could be extracted from `xyspace`; assuming that it is ", 
              "`crs_attributes[[\"crs_wkt\"]]`.")
    }
  }
  if (crs_xyspace != crs_wkt) {
    msg <- paste0("The CRS given in `crs_attributes[[\"crs_wkt\"]]` needs to ", 
                  "match the CRS of the `xyspace` object. Currently, ", 
                  "`crs_attributes[[\"crs_wkt\"]]` is ", shQuote(crs_wkt$Wkt), 
                  " and the CRS of `xyspace` is ", shQuote(crs_xyspace$Wkt))
    if (check_crs) 
      stop(msg)
    else if (verbose) 
      warning(msg)
  }
  # if the dataset is gridded, make an xy-grid
  if (is_gridded) {
    xy_grid <- try(rSW2st::get_xyspace(xyspace, crs = crs_xyspace), 
                   silent = TRUE)
    if (inherits(xy_grid, "try-error")) {
      stop("Argument `xyspace` could not be interpreted as grid.")
    }
    xvals <- xy_grid[[1L]]
    yvals <- xy_grid[[2L]]
    n_xvals <- length(xvals)
    n_yvals <- length(yvals)
    if (data_dims[["nx"]] > n_xvals || data_dims[["ny"]] > 
        n_yvals) {
      stop("For gridded data, `data_dims[\"nx\"]` and `data_dims[\"ny\"]` ", 
           "must be smaller or equal to the number of unique x or, respectively, ", 
           "y coordinate values in `xyspace` for gridded data.")
    }
    grid_res <- lapply(xy_grid, function(x) unique(diff(x)))
    check_res <- vapply(grid_res, function(x) diff(range(x)), 
                        FUN.VALUE = NA_real_)
    if (any(check_res > sqrt(.Machine[["double.eps"]]))) {
      stop("Coordinate intervals of `xyspace` are not constant ", 
           "as is required for a grid.")
    }
    grid_halfres <- c(grid_res[[1L]][[1L]], grid_res[[2L]][[1L]])/2
    x_bounds <- rbind(xvals - grid_halfres[[1L]], xvals + 
                        grid_halfres[[1L]])
    y_bounds <- rbind(yvals - grid_halfres[[2L]], yvals + 
                        grid_halfres[[2L]])
  } else {
    locs <- try(as_points(xyspace, to_class = "sf", crs = crs_wkt), 
                silent = TRUE)
    if (inherits(locs, "try-error")) {
      stop("Argument `xyspace` could not be interpreted as location.")
    }
    n_sites <- nrow(locs)
    tmp_coord <- sf::st_coordinates(locs)
    xvals <- tmp_coord[, 1]
    yvals <- tmp_coord[, 2]
    if (data_dims[["ns"]] > n_sites) {
      stop("`data_dims[\"ns\"]` must be smaller or equal to ", 
           "the number of sites in `xyspace` for discrete data sites or points.")
    }
  }
  
  type_timeaxis <- match.arg(type_timeaxis)
  n_time <- length(time_values)
  has_T_timeAxis <- if (data_dims[["nt"]] > 0) {
    "explicit"
  } else if (n_time > 0) {
    "implicit"
  } else {
    "none"
  }
  if (has_T_timeAxis %in% c("explicit", "implicit")) {
    if (has_T_timeAxis == "explicit" && data_dims[["nt"]] != 
        n_time) {
      stop("`data_dims[\"nt\"]` must match ", "the number of elements in `time_values`.")
    }
    if (has_T_timeAxis == "implicit" && n_time != 1) {
      stop("If `data_dims[\"nt\"]` is zero, ", "then `time_values` can only have one value.")
    }
    if (length(dim(time_bounds)) == 0) {
      if (n_time * 2 != length(time_bounds)) {
        stop("Start and end required for each `time_values` ", 
             "to define `time_bounds`")
      }
      time_bounds <- matrix(time_bounds, nrow = n_time, 
                            ncol = 2, byrow = TRUE)
    } else {
      if (!identical(dim(time_bounds), c(as.integer(n_time), 
                                         2L))) {
        stop("Start and end required for each `time_values` ", 
             "to define `time_bounds`")
      }
    }
    if (type_timeaxis == "timeseries") {
      varid_timebnds <- "time_bnds"
      att_timebnds <- "bounds"
    }
    else if (type_timeaxis == "climatology") {
      varid_timebnds <- "climatology_bounds"
      att_timebnds <- "climatology"
    }
    if ("units" %in% names(time_attributes)) {
      time_units <- time_attributes[["units"]]
      time_attributes[["units"]] <- NULL
    }
    else {
      stop("Need units attribute in time attribute list")
    }
    if ("calendar" %in% names(time_attributes)) {
      time_cal <- time_attributes[["calendar"]]
      time_attributes[["calendar"]] <- NULL
    }
    else {
      stop("Need calendar attribute in time attribute list")
    }
    if ("unlim" %in% names(time_attributes)) {
      time_unlim <- as.logical(time_attributes[["unlim"]])
      time_attributes[["unlim"]] <- NULL
    }
    else {
      stop("Need unlim attribute in time attribute list")
    }
    if ("axis" %in% names(time_attributes)) {
      if ("T" != toupper(time_attributes[["axis"]])) {
        stop("`time_attributes`: ", "if `axis` is included, then its value must be `T`")
      }
      time_attributes[["axis"]] <- NULL
    }
    if (att_timebnds %in% names(time_attributes)) {
      if (varid_timebnds != time_attributes[[att_timebnds]]) {
        stop("`time_attributes`: ", "if ", shQuote(att_timebnds), 
             " is included, then its value must be ", shQuote(varid_timebnds))
      }
      time_attributes[[att_timebnds]] <- NULL
    }
    not_att_timebnds <- switch(EXPR = type_timeaxis, timeseries = "climatology", 
                               climatology = "bounds")
    if (not_att_timebnds %in% names(time_attributes)) {
      warning("`time_attributes`: ", "the attribute ", 
              shQuote(not_att_timebnds), " is ignored ", "because time represents a ", 
              shQuote(type_timeaxis), "; instead, the automatically generated attribute ", 
              shQuote(att_timebnds), " encodes the bounds of the time axis.")
      time_attributes[[not_att_timebnds]] <- NULL
    }
    ns_att_time <- names(time_attributes)
  }
  n_vertical <- length(vertical_values)
  has_Z_verticalAxis <- if (data_dims[["nz"]] > 0) {
    "explicit"
  } else if (n_vertical > 0) {
    "implicit"
  } else {
    "none"
  }
  if (has_Z_verticalAxis %in% c("explicit", "implicit")) {
    if (has_Z_verticalAxis == "explicit" && data_dims[["nz"]] != 
        n_vertical) {
      stop("`data_dims[\"nz\"]` must match ", "the number of elements in `vertical_values`.")
    }
    if (has_Z_verticalAxis == "implicit" && n_vertical != 
        1) {
      stop("If `data_dims[\"nz\"]` is zero, ", "then `vertical_values` can only have one value.")
    }
    if (length(dim(vertical_bounds)) == 0) {
      if (n_vertical * 2 != length(vertical_bounds)) {
        stop("Start and end values required for each `vertical_values` ", 
             "to define `vertical_bounds`")
      }
      vertical_bounds <- matrix(vertical_bounds, nrow = n_vertical, 
                                ncol = 2, byrow = TRUE)
    }
    else {
      if (!identical(dim(vertical_bounds), c(as.integer(n_vertical), 
                                             2L))) {
        stop("Start and end values required for each `vertical_values` ", 
             "to define `vertical_bounds`")
      }
    }
    if ("units" %in% names(vertical_attributes)) {
      vert_units <- vertical_attributes[["units"]]
      vertical_attributes[["units"]] <- NULL
    }
    else {
      stop("Need `units` attribute in vertical attribute list")
    }
    if (!("positive" %in% names(vertical_attributes))) {
      stop("Need `positive` attribute in vertical attribute list")
    }
    if ("axis" %in% names(vertical_attributes)) {
      if ("Z" != toupper(vertical_attributes[["axis"]])) {
        stop("`vertical_attributes`: ", "if `axis` is included, then its value must be `Z`")
      }
      vertical_attributes[["axis"]] <- NULL
    }
    if ("bounds" %in% names(vertical_attributes)) {
      if ("vertical_bnds" != vertical_attributes[["bounds"]]) {
        stop("`vertical_attributes`: ", "if `bounds` is included, then its value must be `vertical_bnds`")
      }
      vertical_attributes[["bounds"]] <- NULL
    }
    ns_att_vert <- names(vertical_attributes)
  }
  n_vars <- max(1, data_dims[["nv"]])
  if (any(lengths(var_attributes) != n_vars)) {
    stop("All variable attributes need a value for each variable.")
  }
  if ("name" %in% names(var_attributes)) {
    var_names <- var_attributes[["name"]]
    var_attributes[["name"]] <- NULL
  } else {
    stop("Need a `name` variable attribute.")
  }
  if (!"long_name" %in% names(var_attributes)) {
    var_attributes[["long_name"]] <- var_names
  }
  if ("units" %in% names(var_attributes)) {
    var_units <- var_attributes[["units"]]
    var_attributes[["units"]] <- NULL
  } else {
    stop("Need unit attribute in variable attribute list")
  }
  if ("grid_mapping" %in% names(var_attributes)) {
    if (!startsWith(var_attributes[["grid_mapping"]], "crs")) {
      warning("Variable attribute for `grid_mapping` should be 'crs: ...', but is ", 
              shQuote(var_attributes[["grid_mapping"]]))
    }
  } else { 
    var_attributes[["grid_mapping"]] <- paste("crs:", xy_attributes[["name"]][[1L]], 
                                              xy_attributes[["name"]][[2L]])
    if (verbose) {
      message("Adding `grid_mapping = \"", var_attributes[["grid_mapping"]], 
              "\"`", " to variable attributes.")
    }
  }
  if (!is_gridded) {
    if (!("coordinates" %in% names(var_attributes))) {
      tmp <- paste(xy_attributes[["name"]][[2L]], xy_attributes[["name"]][[1L]])
      if (has_T_timeAxis != "none") 
        tmp <- paste("time", tmp)
      if (has_Z_verticalAxis != "none") 
        tmp <- paste(tmp, "vertical")
      var_attributes[["coordinates"]] <- tmp
      if (verbose) {
        message("Adding `coordinates = \"", tmp, "\"` to variable attributes.")
      }
    }
    if (!("featureType" %in% names(global_attributes))) {
      global_attributes[["featureType"]] <- "point"
      if (verbose) {
        message("Adding `featureType = \"point\"` to global attributes.")
      }
    }
  }
  if ("_FillValue" %in% names(var_attributes)) {
    if (verbose) {
      warning("`_FillValue` variable attribute is automatically generated.")
    }
    var_attributes[["_FillValue"]] <- NULL
  }
  if ("missing_value" %in% names(var_attributes)) {
    if (verbose) {
      warning("`missing_value` variable attribute is replaced by ", 
              "automatically generated `_FillValue`.")
    }
    var_attributes[["missing_value"]] <- NULL
  }
  ns_att_vars <- names(var_attributes)
  ## start making netcdf?? 
  # initiate netCDF file 
  # new 
  if(dir.exists(dirname(filename))) {
    dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  }  
  if(isParallel == TRUE) {
    ## figure this out next!! 
    # FILL THIS IN ONCE I FIGURE OUT WHAT THE HECK PDBMPI IS DOING??
    # RNetCDF::create.nc(filename = filename, clobber = TRUE, format = "netcdf4", 
    #                    mpi_comm = 
    # )
  }
  if (isParalle == FALSE) {
    nc <- RNetCDF::create.nc(filename = filename, clobber = TRUE, format = "netcdf4")
    # define band dimension
    RNetCDF::dim.def.nc(ncfile = nc, dimname = "bnds", dimlength = 2)
    # our netCDFs will always be gridded, right??
    # define x dimension
    RNetCDF::dim.def.nc(ncfile = nc, dimname = xy_attributes[["name"]][[1L]], dimlength = data_dims_from_data["nx"])
    # define y dimension 
    RNetCDF::dim.def.nc(ncfile = nc, dimname = xy_attributes[["name"]][[2L]], dimlength = data_dims_from_data["ny"])
    # define time axis
    RNetCDF::dim.def.nc(ncfile = nc, dimname = "time", unlim = time_unlim)
    
    ## need to populate the x and y and time dimensions still? they've been defined, but not populated? 
  }
  
  on.exit(ncdf4::nc_close(nc))
  #bnddim <- ncdf4::ncdim_def(name = "bnds", units = "", vals = seq_len(2L), create_dimvar = FALSE)
  if (is_gridded) {
    xdim <- ncdf4::ncdim_def(name = xy_attributes[["name"]][[1L]], 
                             longname = xy_attributes[["long_name"]][[1L]], units = xy_attributes[["units"]][[1L]], 
                             vals = xvals)
    ydim <- ncdf4::ncdim_def(name = xy_attributes[["name"]][[2L]], 
                             longname = xy_attributes[["long_name"]][[2L]], units = xy_attributes[["units"]][[2L]], 
                             vals = yvals)
    var_dims <- list(xdim, ydim)
    var_chunksizes <- if (has_chunks) 
      c(n_xvals, n_yvals)
    else NA
    var_start <- c(1, 1) }
  # } else {
  #   idim <- ncdf4::ncdim_def(name = "site", longname = "SOILWAT2 simulation sites", 
  #                            units = "1", vals = seq_len(n_sites))
  #   var_dims <- list(idim)
  #   var_chunksizes <- if (has_chunks) 
  #     n_sites
  #   else NA
  #   var_start <- 1
  # }
  # if (has_Z_verticalAxis != "none") {
  #   zdim <- ncdf4::ncdim_def(name = "vertical", units = vert_units, 
  #                            vals = vertical_values)
  #   var_dims <- c(var_dims, list(zdim))
  #   if (has_chunks && has_predet_chunks) {
  #     var_chunksizes <- c(var_chunksizes, if (nc_chunks == 
  #                                             "by_zt") 1 else n_vertical)
  #   }
  #   var_start <- c(var_start, 1)
  # }
  if (has_T_timeAxis != "none") {
    tdim <- ncdf4::ncdim_def(name = "time", units = time_units, 
                             calendar = time_cal, unlim = time_unlim, vals = time_values)
    var_dims <- c(var_dims, list(tdim))
    # if (has_chunks && has_predet_chunks) {
    #   var_chunksizes <- c(var_chunksizes, if (nc_chunks %in% 
    #                                           c("by_zt", "by_t")) 1 else n_time)
    # }
    var_start <- c(var_start, 1)
  }
  # if (has_chunks && !has_predet_chunks) {
  #   stopifnot(length(nc_chunks) == length(var_dims))
  #   var_chunksizes <- nc_chunks
  # }
  var_defs <- lapply(seq_len(n_vars), function(k) {
    ncdf4::ncvar_def(name = var_names[k], units = var_units[k], 
                     dim = var_dims, compression = nc_deflate, shuffle = nc_shuffle, 
                     chunksizes = if (has_chunks) 
                       var_chunksizes
                     else NA, missval = NAflag, prec = data_type)
  })
  # if (!is_gridded) {
  #   xvar <- ncdf4::ncvar_def(name = xy_attributes[["name"]][[1L]], 
  #                            longname = xy_attributes[["long_name"]][[1L]], units = xy_attributes[["units"]][[1L]], 
  #                            dim = list(idim), compression = nc_deflate, chunksizes = n_sites, 
  #                            missval = NAflag, prec = "double")
  #   yvar <- ncdf4::ncvar_def(name = xy_attributes[["name"]][[2L]], 
  #                            longname = xy_attributes[["long_name"]][[2L]], units = xy_attributes[["units"]][[2L]], 
  #                            dim = list(idim), compression = nc_deflate, chunksizes = n_sites, 
  #                            missval = NAflag, prec = "double")
  #   var_defs <- c(var_defs, list(yvar, xvar))
  # }
  crsdef <- ncdf4::ncvar_def(name = "crs", units = "", dim = list(), 
                             missval = NULL, prec = "integer")
  nc_dimvars <- if (is_gridded) {
    bnds_name <- paste0(xy_attributes[["name"]][1:2], "_bnds")
    if ("bounds" %in% names(xy_attributes)) {
      if (any(bnds_name != xy_attributes[["bounds"]])) {
        stop("`xy_attributes`: ", "if `bounds` is included, then its value must be ", 
             shQuote(bnds_name[[1L]]), " and ", shQuote(bnds_name[[2L]]))
      }
      xy_attributes[["bounds"]] <- NULL
    }
    list(ncdf4::ncvar_def(name = bnds_name[[1L]], units = "", 
                          dim = list(bnddim, xdim), missval = NULL, compression = nc_deflate, 
                          chunksizes = c(2L, n_xvals), prec = "double"), ncdf4::ncvar_def(name = bnds_name[[2L]], 
                                                                                          units = "", dim = list(bnddim, ydim), missval = NULL, 
                                                                                          compression = nc_deflate, chunksizes = c(2L, n_yvals), 
                                                                                          prec = "double"))
  } else {
    list()
  }
  if (has_Z_verticalAxis != "none") {
    vertbnddef <- ncdf4::ncvar_def(name = "vertical_bnds", 
                                   units = "", dim = list(bnddim, zdim), missval = NULL, 
                                   compression = nc_deflate, chunksizes = c(2L, n_vertical), 
                                   prec = "double")
    nc_dimvars <- c(nc_dimvars, list(vertbnddef))
  }
  if (has_T_timeAxis != "none") {
    tbnddef <- ncdf4::ncvar_def(name = varid_timebnds, units = "", 
                                dim = list(bnddim, tdim), missval = NULL, compression = nc_deflate, 
                                chunksizes = if (time_unlim) 
                                  c(2L, 1L)
                                else c(2L, n_time), prec = "double")
    nc_dimvars <- c(nc_dimvars, list(tbnddef))
  }
  # dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  # nc <- ncdf4::nc_create(filename = filename, vars = c(nc_dimvars, 
  #                                                      list(crsdef), var_defs), force_v4 = TRUE)
  # on.exit(ncdf4::nc_close(nc))
  if (is_gridded) {
    ncdf4::ncvar_put(nc, varid = bnds_name[[1L]], vals = x_bounds, 
                     start = c(1, 1), count = c(2L, n_xvals))
    ncdf4::ncvar_put(nc, varid = bnds_name[[2L]], vals = y_bounds, 
                     start = c(1, 1), count = c(2L, n_yvals))
  } else {
    ncdf4::ncvar_put(nc, varid = xy_attributes[["name"]][[1L]], 
                     vals = xvals, start = 1, count = n_sites)
    ncdf4::ncvar_put(nc, varid = xy_attributes[["name"]][[2L]], 
                     vals = yvals, start = 1, count = n_sites)
  }
  if (has_Z_verticalAxis != "none") {
    ncdf4::ncvar_put(nc, varid = "vertical_bnds", vals = t(vertical_bounds), 
                     start = c(1, 1), count = c(2L, n_vertical))
  }
  if (has_T_timeAxis != "none") {
    ncdf4::ncvar_put(nc, varid = varid_timebnds, vals = t(time_bounds), 
                     start = c(1, 1), count = c(2, n_time))
  }
  if ("standard_name" %in% names(xy_attributes)) {
    for (k in seq_len(2)) {
      ncdf4::ncatt_put(nc, varid = xy_attributes[["name"]][k], 
                       attname = "standard_name", attval = xy_attributes[["standard_name"]][k])
    }
  }
  if (is_gridded) {
    ncdf4::ncatt_put(nc, xy_attributes[["name"]][[1L]], "axis", 
                     "X")
    ncdf4::ncatt_put(nc, xy_attributes[["name"]][[1L]], "bounds", 
                     bnds_name[[1L]])
    ncdf4::ncatt_put(nc, xy_attributes[["name"]][[2L]], "axis", 
                     "Y")
    ncdf4::ncatt_put(nc, xy_attributes[["name"]][[2L]], "bounds", 
                     bnds_name[[2L]])
  }
  if (has_Z_verticalAxis != "none") {
    ncdf4::ncatt_put(nc, "vertical", "axis", "Z")
    ncdf4::ncatt_put(nc, "vertical", "bounds", "vertical_bnds")
    for (natt in ns_att_vert) {
      ncdf4::ncatt_put(nc, varid = "vertical", attname = natt, 
                       attval = vertical_attributes[[natt]])
    }
  }
  if (has_T_timeAxis != "none") {
    ncdf4::ncatt_put(nc, "time", "axis", "T")
    ncdf4::ncatt_put(nc, "time", att_timebnds, varid_timebnds)
    for (natt in ns_att_time) {
      ncdf4::ncatt_put(nc, varid = "time", attname = natt, 
                       attval = time_attributes[[natt]])
    }
  }
  for (k in seq_len(n_vars)) {
    for (natt in ns_att_vars) {
      ncdf4::ncatt_put(nc, varid = var_names[k], attname = natt, 
                       attval = var_attributes[[natt]][k])
    }
  }
  ncdf4::nc_close(nc)
  ncr <- RNetCDF::open.nc(filename, write = TRUE)
  on.exit(RNetCDF::close.nc(ncr))
  for (natt in ns_att_crs) {
    RNetCDF::att.put.nc(ncr, variable = "crs", name = natt, 
                        type = get_nc_type(crs_attributes[[natt]]), value = crs_attributes[[natt]])
  }
  RNetCDF::close.nc(ncr)
  nc <- ncdf4::nc_open(filename = filename, write = TRUE)
  on.exit(ncdf4::nc_close(nc))
  ncdf4::ncatt_put(nc, "crs", attname = "crs_wkt", attval = crs_wkt$Wkt)
  ncdf4::ncatt_put(nc, varid = 0, attname = "Conventions", 
                   attval = "CF-1.8")
  tmp_version_netcdf <- try(system2("nc-config", "--version", 
                                    stdout = TRUE, stderr = TRUE), silent = TRUE)
  ncdf4::ncatt_put(nc, varid = 0, attname = "created_by", attval = paste0(R.version[["version.string"]], 
                                                                          ", R package ", "ncdf4 v", getNamespaceVersion("ncdf4"), 
                                                                          if (!inherits(tmp_version_netcdf, "try-error")) {
                                                                            paste0(", and ", tmp_version_netcdf)
                                                                          }))
  ncdf4::ncatt_put(nc, varid = 0, attname = "creation_date", 
                   attval = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  if (!missing(global_attributes)) {
    ns_att_glob <- names(global_attributes)
    tmp <- c("Conventions", "created_by", "creation_date")
    if (has_T_timeAxis == "none") {
      tmp <- c(tmp, "time_label", "time_title")
    }
    has_replaced_gatts <- tmp[tmp %in% ns_att_glob]
    if (length(has_replaced_gatts) > 0) {
      warning("`global_attributes` contained values for ", 
              toString(shQuote(has_replaced_gatts)), "; they were replaced with an automatically generated value.")
      ns_att_glob <- setdiff(ns_att_glob, has_replaced_gatts)
    }
    for (natt in ns_att_glob) {
      ncdf4::ncatt_put(nc, varid = 0, attname = natt, attval = global_attributes[[natt]])
    }
  }
  if (has_T_timeAxis == "none") {
    ncdf4::ncatt_put(nc, varid = 0, attname = "time_label", 
                     attval = "None")
    ncdf4::ncatt_put(nc, varid = 0, attname = "time_title", 
                     attval = "No temporal dimensions ... fixed field")
  }
  if (has_data) {
    .populate_netCDF_nocheck(nc, data = data, var_names = var_names, 
                             data_str = data_str, is_gridded = is_gridded)
  }
  if (verbose) {
    message("The netCDF has ", nc$nvars, " variables and ", 
            nc$ndim, " dimensions")
  }
  invisible(TRUE)
}
