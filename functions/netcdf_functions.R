#' Interaction of \var{rSW2} objects with \var{netCDFs}
#'
#' @param data A numeric array or vector (optional). A vector is converted
#'   to a one-column matrix.
#'
#' @param grid An object that describes a gridded \var{xy-space}.
#'  Regular, rectangular grids are the only currently supported type.
#'  This can be \itemize{
#'    \item a \code{\link[raster:RasterLayer-class]{raster::RasterLayer}}
#'          object,
#'    \item a \code{stars::stars} object,
#'    \item a file name pointing to such a raster on disk;
#'    \item a list, such as the one produced by \code{\link{get_xyspace}}.
#'    \item an object with coordinate values for all \var{gridcell} centers
#'          that can be passed to \code{\link{as_points}};
#'  }
#'  The \var{crs} of the grid coordinate values must match the one of the
#'  data locations.
#'
#' @param locations An object from which \var{x} and \var{y} coordinate values
#'   can be extracted that describe the \var{xy} locations of each \code{data}
#'   row, e.g., a matrix or \var{data.frame} or a spatial points object
#'   inheriting from \var{sf} or \var{Spatial*}.
#' @param locations_crs An object which is a \var{crs} or
#'   from which one can be derived
#'   that describes the \var{crs} of \code{locations}.
#'
#' @param data_str A character string describing the dimensions of \code{data}
#'   where \var{"xy"} stands for \var{x} and \var{y} spatial dimensions
#'   if the spatial structure is gridded,
#'   while \var{"s"} stands for \var{site} if the spatial structure are
#'   discrete points;
#'   \var{z} stands for a vertical dimension; and \var{t} stands for a
#'   temporal dimension.
#'
#' @param xy_names A vector with two character strings. The names of the
#'   \var{x} and \var{y} spatial dimensions of a \var{netCDF} file.
#'
#' @name rSW2st_netCDF
#' @aliases netCDF netcdf
NULL


#' Create a \var{netCDF} file (with or without data)
#'
#' The user describes a data array and specifies
#' spatial, vertical, and time information,
#' and metadata to create a \var{netCDF} in \var{netcdf-4 format}
#' according to \var{CF-1.8} standards.
#'
#'
#' @inheritParams rSW2st_netCDF
#' @param filename A character string. The name of the \var{netCDF} file.
#' @param xyspace An object that describes the \var{xy-space} of the
#'   \var{netCDF} file.
#'   If \code{xyspace} does not contain a \var{crs},
#'   then it is assumed that the \var{crs} is \var{crs_attributes[["crs_wkt"]]}.
#'   If data are gridded, then passed to \code{\link{get_xyspace}};
#'   if non-gridded, then passed to \code{\link{as_points}}.
#' @param data_dims A list as returned by \code{\link{rSW2st::get_data_dims}}.
#'   If \code{NULL} and \code{data} is not missing, then calculated from
#'   \code{data}.
#' @param data_type A character string. The \var{netCDF} data type.
#' @param var_attributes A list of named character strings defining the
#'   \var{netCDF} variable(s).
#'   Elements \var{name} and \var{units} are required.
#' @param xy_attributes A list of named character strings defining the
#'   \var{netCDF} dimensions for the \var{xy-space}.
#'   Elements \var{name}, \var{standard_name}, \var{long_name},
#'   and \var{units} are required.
#' @param crs_attributes A list of named character strings defining the
#'   \var{netCDF} \var{crs} of the \var{xy-space}.
#'   Elements \var{crs_wkt} and \var{grid_mapping_name} are required.
#' @param check_crs A logical value. If \code{TRUE} then check that
#'   the \var{crs} provided via \code{crs_attributes} matches the ones
#'   from \code{locations} and \code{grid} if available.
#' @param time_values A numeric vector or \code{NULL}. The values along the
#'   time dimension (if present).
#'   In units as described by \code{time_attributes}.
#' @param type_timeaxis A character string. Describing if the time dimension
#'   represents a time series or a climatological time.
#' @param time_attributes A list of named character strings defining the
#'   \var{netCDF} time dimension.
#'   Elements \var{calendar}, \var{units}, and \var{unlim} are required.
#' @param time_bounds A numeric vector or two-dimensional matrix.
#'   The start and end of each time (or climatological) unit.
#' @param vertical_values A numeric vector or \code{NULL}. The values along the
#'   vertical dimension (if present).
#'   In units as described by \code{vertical_attributes}.
#' @param vertical_attributes A list of named character strings defining the
#'   \var{netCDF} vertical dimension, e.g., soil depth.
#'   Elements \var{units} and \var{positive} are required.
#' @param vertical_bounds A numeric vector or two-dimensional matrix.
#'   The upper/lower limits of each vertical unit.
#' @param global_attributes A list of named character strings defining the
#'   global attributes of the \var{netCDF}.
#' @param overwrite A logical value. If \code{TRUE}, file will be overwritten
#'   if it already exists.
#' @param nc_compression A logical value. If \code{TRUE}, then the \var{netCDF}
#'   is created using compression arguments
#'   \code{nc_shuffle}, \code{nc_deflate}, and \var{nc_chunks}. Compression
#'   is turned off by default.
#' @param nc_shuffle A logical value. If \code{TRUE}, then the shuffle filter
#'   is turned on which can improve compression.
#'   Used only if \code{nc_compression} is activated \code{TRUE}.
#' @param nc_deflate An integer between 1 and 9 (with increasing)
#'   compression or \code{NA} to turn off compression.
#'   Used only if \code{nc_compression} is activated \code{TRUE}.
#' @param nc_chunks A character string, \code{NA}, or an integer vector.
#'   See details. The default \var{"by_zt"} is to create chunks for the
#'   entire \var{xy-space} and for each vertical and each time step.
#'   Used only if \code{nc_compression} is activated \code{TRUE}.
#' @param verbose A logical value.
#'
#' @return This function is used for the side-effect of creating a \var{netCDF}
#'   file.
#'   Data values are written to the file if provided as argument \code{data}.
#'
#'
#' @section Details:
#' Values can be written to the file at a later time using function
#' \code{\link{populate_netCDF}}.
#'
#' The created \var{netCDF} is suitable for three data situations:
#' \enumerate{
#'    \item one variable and \var{xy-space}, time and vertical dimensions
#'    \item one variable and \var{xy-space} and time or vertical dimensions
#'    \item one or multiple variables and \var{xy-space} dimensions
#'          without time/vertical dimensions
#' }
#'
#'
#' @section Spatial setup:
#' Spatial information about the \var{xy-space} is derived from the arguments
#' \code{xyspace}, \code{xy_attributes}, \code{crs_attributes}, \code{data_str},
#' and \code{data_dims}.
#'
#' The \var{xy-space} is either gridded (determined by the
#' first two characters of \code{data_str} equal to \var{"xy"}),
#' or list of discrete points/sites
#' (determined by the first character of \code{data_str} equal to \var{"s"}).
#'
#' \itemize{
#'   \item The gridded situation creates \var{x} and \var{y} dimensions and
#'     associated variables in the \var{netCDF} file.
#'     The size of the \var{xy-space} must agree
#'     with the elements \var{"n_x"} and \var{"n_y"} of \code{data_dims} and,
#'     thus, with the two first dimensions of \code{data}, if available.
#'
#'   \item The discrete point/site situation creates a \var{site} dimension and
#'     associated variable as well as \var{x} and \var{y} variables
#'     for the spatial coordinate values of the sites; see
# nolint start
#'     \href{http://cfconventions.org/Data/cf-conventions/cf-conventions-1.8/cf-conventions.html#point-data}{CF point-data}.
# nolint end
#'     The code will add a \var{coordinates} attribute to the variable(s) and
#'     a \var{featureType = "point"} global attribute.
#'     The size of the \var{xy-space}, i.e., the number of sites, must agree
#'     with the element \var{"n_s"} of \code{data_dims} and,
#'     thus, with the first dimension of \code{data}, if available.
#' }
#'
#' The \var{crs} are checked by default (see argument \code{check_crs}) for
#' consistency among \code{crs_atttributes}, \code{locations}, and/or
#' \code{grid}. However, this check may fail when \code{locations} and/or
#' \code{grid} use a \var{PROJ.4} representation that doesn't compare well with
#' a \var{WKT2} representation provided by \code{crs_atttributes} even if they
#' are the same. Turn off these checks in such cases.
#'
#'
#' @section Spatial dimensions of data:
#' For the gridded situation, \code{data} array must be arranged
#' in "expanded" spatial format, i.e.,
#' the two dimensions of the \code{data} array span to the
#' \var{xy-space}.
#' The first dimension, i.e., \var{X}, matches \var{gridcells}
#' along \var{longitude} or a projected \var{x} coordinate and
#' the second dimension, i.e., \var{Y}, matches \var{gridcells}
#' along \var{latitude} or a projected \var{y} coordinate.
#' The \code{xy_attributes[["name"]][1:2]} defines the names of
#' the \var{x} and \var{y} dimensions/variables.
#'
#' However, "gridded" data objects are frequently organized by "collapsed"
#' \var{x} and \var{y} dimensions, e.g., to achieve a sparse representation.
#' Use the function \code{\link{convert_xyspace}} to expand sparse data arrays
#' before their use by function \code{\link{create_netCDF}}.
#'
#' For the discrete point/site situation, \code{data} array must be arranged
#' in "collapsed" spatial format, i.e.,
#' the first dimension (rows) of \code{data} corresponds to the number of
#' points/sites.
#'
#'
#' @section Non-spatial dimensions of data:
#' The first non-spatial dimension of a \code{data} array, if present,
#' corresponds to
#' \itemize{
#'   \item multiple variables, if \code{data_str} is \var{"xy"} or \var{"s"}
#'   \item time dimension, if \code{data_str} is \var{"xyt"} or \var{"st"}
#'   \item vertical dimension, if \code{data_str} is
#'         \var{"xyzt"}, \var{"xyz"}, \var{"szt"}, or \var{"sz"}
#' }
#'
#' The second non-spatial dimension of the \code{data} array, if present,
#' corresponds to the time dimension;
#' this situation arises only in the presence of
#' both a time and vertical dimension,
#' i.e., \code{data_str} is \var{"xyzt"} or \var{"szt"}.
#'
#'
#' @section Variables:
#' Use \var{CMIP6} standard variable names, units, etc., where available.
#' Standardized variable names can be searched in the
# nolint start
#' \href{https://github.com/PCMDI/cmip6-cmor-tables/tree/master/Tables}{CMIP6-cmor-tables}
# nolint end
#'
#' @section Chunking:
#' The argument \code{nc_chunks} offers two auto-determined chunking schemes:
#' \describe{
#'   \item{"by_zt"}{
#'     create chunks for the entire \var{xy-space} and
#'     for each vertical and each time step
#'   }
#'   \item{"by_t"}{
#'     create chunks for the entire \var{xy-space} and all vertical steps
#'     (if present) and for each time step
#'   }
#' }
#' Alternatively, the user can provide an integer vector with a length
#' equal to the number of the dimensions according to \code{data_dims}.
#'
#' @seealso \code{\link{populate_netCDF}}, \code{\link{read_netCDF}}
# nolint start
#' @references \href{https://cfconventions.org/Data/cf-conventions/cf-conventions-1.8/cf-conventions.html}{CF conventions}
# nolint end
#'
#'
#' @examples
#' # Prepare data for examples
#' tmp_nc <- create_example_netCDFs(tempdir(), c("xyt", "szt"), "timeseries")
#' data_xyt <- read_netCDF(tmp_nc[["xyt"]], "array", xy_names = c("x", "y"))
#' data_szt <- read_netCDF(tmp_nc[["szt"]], "array", xy_names = c("x", "y"))
#'
#' # Prepare attribute lists
#' nc_att_global <- list(
#'   title = "Example netCDF of package rSW2st",
#'   version = paste0("v", format(Sys.Date(), "%Y%m%d")),
#'   source_id = "SOILWAT2",
#'   further_info_url = "https://github.com/DrylandEcology/",
#'   source_type = "LAND",
#'   realm = "land",
#'   product = "model-output",
#'   grid = "native Alberts projection grid with NAD83 datum",
#'   grid_label = "gn",
#'   nominal_resolution = "1 m"
#' )
#'
#' nc_att_crs <- list(
#'   crs_wkt = sf::st_crs("EPSG:6350")$Wkt,
#'   grid_mapping_name = "albers_conical_equal_area",
#'   standard_parallel = c(29.5, 45.5),
#'   longitude_of_central_meridian = -96.0,
#'   latitude_of_projection_origin = 23.0,
#'   false_easting = 0.0,
#'   false_northing = 0.0,
#'   # GRS 1980 ellipsoid
#'   longitude_of_prime_meridian = 0,
#'   semi_major_axis = 6378137.0,
#'   inverse_flattening = 298.257222101
#' )
#'
#' nc_att_xy <- list(
#'   name = c("x", "y"),
#'   standard_name = c("projection_x_coordinate", "projection_y_coordinate"),
#'   long_name = c("x coordinate of projection", "y coordinate of projection"),
#'   units = c("m", "m")
#' )
#'
#' ## Write netCDF for gridded data
#' tmp_nc[["xyt2"]] <- sub(".nc", "2.nc", tmp_nc[["xyt"]])
#'
#' create_netCDF(
#'   filename = tmp_nc[["xyt2"]],
#'   xyspace = data_xyt[["xyspace"]],
#'   data = data_xyt[["data"]],
#'   data_str = "xyt",
#'   var_attributes = list(name = "sine", units = "1"),
#'   xy_attributes = nc_att_xy,
#'   crs_attributes = nc_att_crs,
#'   time_values = data_xyt[["time_values"]],
#'   type_timeaxis = "timeseries",
#'   global_attributes = nc_att_global
#' )
#'
#' data_xyt2 <- read_netCDF(tmp_nc[["xyt2"]], "array", xy_names = c("x", "y"))
#' all.equal(data_xyt2[["data"]], data_xyt[["data"]])
#'
#'
#' ## Write netCDF for discrete data
#' tmp_nc[["szt2"]] <- sub(".nc", "2.nc", tmp_nc[["szt"]])
#'
#' create_netCDF(
#'   filename = tmp_nc[["szt2"]],
#'   xyspace = as.data.frame(data_szt[["xyspace"]][1:2]),
#'   data = data_szt[["data"]],
#'   data_str = "szt",
#'   var_attributes = list(name = "sine", units = "1"),
#'   xy_attributes = nc_att_xy,
#'   crs_attributes = nc_att_crs,
#'   time_values = data_szt[["time_values"]],
#'   type_timeaxis = "timeseries",
#'   vertical_values = data_szt[["vertical_values"]],
#'   vertical_attributes = list(units = "m", positive = "down"),
#'   global_attributes = nc_att_global
#' )
#'
#'
#' data_szt2 <- read_netCDF(tmp_nc[["szt2"]], "array", xy_names = c("x", "y"))
#' all.equal(data_szt2[["data"]], data_szt[["data"]])
#'
#' # Cleanup
#' unlink(unlist(tmp_nc))
#'
#' @export
create_netCDF <- function(
    filename,
    xyspace,
    data = NULL,
    data_str = c("xyzt", "xyt", "xyz", "xy", "szt", "st", "sz", "s"),
    data_dims = rSW2st::get_data_dims(data_str, dim(data)),
    data_type = c("double", "float", "integer", "short", "byte", "char"),
    var_attributes = list(
      name = "",
      standard_name = "",
      long_name = "",
      units = "",
      grid_mapping = "crs",
      cell_methods = "",
      cell_measures = ""
    ),
    xy_attributes = list(
      name = c("lon", "lat"),
      standard_name = c("longitude", "latitude"),
      long_name = c("Longitude", "Latitude"),
      units = c("degrees_east", "degrees_north")
    ),
    crs_attributes = list(
      crs_wkt = sf::st_crs("OGC:CRS84")$Wkt,
      grid_mapping_name = "latitude_longitude",
      longitude_of_prime_meridian = 0.0,
      semi_major_axis = 6378137.0,
      inverse_flattening = 298.257223563
    ),
    check_crs = TRUE,
    time_values = NULL,
    type_timeaxis = c("timeseries", "climatology"),
    time_attributes = list(
      units = "days since 1900-01-01",
      calendar = "standard",
      unlim = FALSE
    ),
    time_bounds = matrix(NA, nrow = length(time_values), ncol = 2),
    vertical_values = NULL,
    vertical_attributes = list(
      units = "",
      positive = "down"
    ),
    vertical_bounds = matrix(NA, nrow = length(vertical_values), ncol = 2),
    global_attributes = list(title = "Title"),
    overwrite = FALSE,
    nc_compression = FALSE,
    nc_shuffle = TRUE,
    nc_deflate = 5,
    nc_chunks = "by_zt",
    isParallel = FALSE,
    verbose = FALSE
) {
  #------ 1) Checks/preparations -----------------------------------------------
  stopifnot(requireNamespace("pbdNCDF4"))
  
  has_compression <- isTRUE(nc_compression)
  stopifnot(nc_deflate %in% c(NA, 1:9))
  
  nc_deflate <- if (has_compression) nc_deflate else NA
  
  has_chunks <- has_compression && !is.na(nc_chunks)
  has_predet_chunks <- is.character(nc_chunks)
  if (has_compression && has_predet_chunks) {
    stopifnot(nc_chunks %in% c("by_zt", "by_t"))
  }
  
  nc_shuffle <-
    has_compression && isTRUE(nc_shuffle) &&
    data_type %in% c("integer", "short")
  
  
  #------ netCDF filename ------
  if (file.exists(filename)) {
    if (overwrite) {
      unlink(filename)
    } else {
      warning(
        "File ", shQuote(basename(filename)),
        " exists and 'overwrite' is FALSE; returning early."
      )
      return(invisible(FALSE))
    }
  }
  
  
  #------ data characterization ------
  has_data <- !missing(data) && !is.null(data)
  
  data_type <- match.arg(data_type)
  
  NAflag <- switch(
    data_type,
    char = NULL,
    byte = NULL,
    short = -128L,
    integer = -2147483647L,
    float = -3.4e+38,
    double = -1.7e+308
  )
  
  
  # Three data structure situations:
  #   i) one variable and vertical axis and time ("xyzt", "szt")
  #   ii) one variable and time OR vertical axis ("xyt", "xyz", "st", "sz")
  #   iii) one/multiple variables and no time/vertical axis ("xy", "s")
  data_str <- match.arg(data_str)
  is_gridded <- isTRUE(substr(data_str, 1, 2) == "xy")
  
  
  #--- Check data dimensions/structure
  if (has_data) {
    data_dims_from_data <- rSW2st::get_data_dims(data_str, dim(data))
  }
  
  
  if (is.null(data_dims)) {
    data_dims <- data_dims_from_data
    
  } else {
    # Convert NULLs and non-finite values to NA
    data_dims <- lapply(
      data_dims,
      function(x) if (is.null(x) || !is.finite(x)) NA else x
    )
    
    # Convert list to named vector (now that we took care of possible NULLs)
    data_dims <- unlist(data_dims)
    
    # Check that provided `data_dims` match dimensions of argument `data`
    if (has_data) {
      if (
        !isTRUE(all.equal(
          data_dims_from_data,
          data_dims[names(data_dims_from_data)]
        ))
      ) {
        stop("Disagreement in dimensions between `data_dims` and `data`.")
      }
    }
  }
  
  
  # Check argument data dimensions
  stopifnot(
    c("ns", "nx", "ny", "nt", "nz", "nv") %in% names(data_dims),
    !anyNA(data_dims),
    data_dims["ns"] > 0 || data_dims["nx"] > 0 && data_dims["ny"] > 0
  )
  
  # Check that data structure is possible given data dimensions
  tmp <- switch(
    EXPR = data_str,
    
    `xyzt` = stopifnot(
      data_dims["nt"] > 0,
      data_dims["nz"] > 0,
      data_dims["nv"] == 0
    ),
    
    `xyt` = stopifnot(
      data_dims["nt"] > 0,
      data_dims["nz"] == 0,
      data_dims["nv"] == 0
    ),
    
    `xyz` = stopifnot(
      data_dims["nt"] == 0,
      data_dims["nz"] > 0,
      data_dims["nv"] == 0
    ),
    
    `xy` = stopifnot(
      data_dims["nt"] == 0,
      data_dims["nz"] == 0,
      data_dims["nv"] >= 0
    )
  )
  
  
  
  #------ xy-space ------
  if (is.null(xyspace) || missing(xyspace)) {
    stop("Must provide `xyspace` as argument.")
  }
  
  
  #--- xy attributes
  if (any(lengths(xy_attributes) != 2)) {
    stop("All `xy_attributes` must be of lenght two (xy-space).")
  }
  
  tmp_xy_atts <- c("name", "standard_name", "long_name", "units")
  if (any(!(tmp_xy_atts %in% names(xy_attributes)))) {
    stop(
      "`xy_attributes` must include ",
      paste0(shQuote(tmp_xy_atts), collapse = ", ")
    )
  }
  
  if ("axis" %in% names(xy_attributes)) {
    if (any(c("X", "Y") != toupper(xy_attributes[["axis"]]))) {
      stop(
        "`xy_attributes`: if `axis` is included, then its value must be ",
        "`X` and `Y`."
      )
    }
    xy_attributes[["axis"]] <- NULL
  }
  
  
  #--- crs attributes setup & info
  if ("crs_wkt" %in% names(crs_attributes)) {
    crs_wkt <- crs_attributes[["crs_wkt"]]
    crs_attributes[["crs_wkt"]] <- NULL
    
    # check that CRS is valid
    crs_wkt <- try(sf::st_crs(crs_wkt), silent = TRUE)
    if (!inherits(crs_wkt, "crs") || crs_wkt == sf::NA_crs_) {
      stop("`crs_attributes[[\"crs_wkt\"]]` does not represent a valid CRS.")
    }
    
  } else {
    stop("Need `crs_attributes[[\"crs_wkt\"]]`")
  }
  
  if (!("grid_mapping_name" %in% names(crs_attributes))) {
    stop("Need `crs_attributes[[\"grid_mapping_name\"]]`")
  }
  
  ns_att_crs <- names(crs_attributes)
  
  
  # `xyspace` is allowed to be an object without a crs
  crs_xyspace <- try(suppressWarnings(sf::st_crs(xyspace)), silent = TRUE)
  if (!inherits(crs_xyspace, "crs") || crs_xyspace == sf::NA_crs_) {
    crs_xyspace <- crs_wkt
    
    if (verbose) {
      message(
        "No `crs` could be extracted from `xyspace`; assuming that it is ",
        "`crs_attributes[[\"crs_wkt\"]]`."
      )
    }
  }
  
  
  # check that CRS definition matches CRS of xyspace (grid or locations)
  if (crs_xyspace != crs_wkt) {
    msg <- paste0(
      "The CRS given in `crs_attributes[[\"crs_wkt\"]]` needs to ",
      "match the CRS of the `xyspace` object. Currently, ",
      "`crs_attributes[[\"crs_wkt\"]]` is ", shQuote(crs_wkt$Wkt),
      " and the CRS of `xyspace` is ", shQuote(crs_xyspace$Wkt)
    )
    
    if (check_crs) stop(msg) else if (verbose) warning(msg)
  }
  
  
  if (is_gridded) {
    # Note: xvals are organized from west to east, yvals from south to north
    xy_grid <- try(
      rSW2st::get_xyspace(xyspace, crs = crs_xyspace),
      silent = TRUE
    )
    
    if (inherits(xy_grid, "try-error")) {
      stop("Argument `xyspace` could not be interpreted as grid.")
    }
    
    # xy values
    xvals <- xy_grid[[1]]
    yvals <- xy_grid[[2]]
    n_xvals <- length(xvals)
    n_yvals <- length(yvals)
    
    if (data_dims["nx"] > n_xvals || data_dims["ny"] > n_yvals) {
      stop(
        "For gridded data, `data_dims[\"nx\"]` and `data_dims[\"ny\"]` ",
        "must be smaller or equal to the number of unique x or, respectively, ",
        "y coordinate values in `xyspace` for gridded data."
      )
    }
    
    # Grid resolution/bounds
    grid_res <- lapply(xy_grid, function(x) unique(diff(x)))
    check_res <- sapply(grid_res, function(x) diff(range(x)))
    
    if (any(check_res > sqrt(.Machine$double.eps))) {
      stop(
        "Coordinate intervals of `xyspace` are not constant ",
        "as is required for a grid."
      )
    }
    
    # Calculate x and y bounds
    grid_halfres <- c(grid_res[[1]][1], grid_res[[2]][1]) / 2
    x_bounds <- rbind(xvals - grid_halfres[1], xvals + grid_halfres[1])
    y_bounds <- rbind(yvals - grid_halfres[2], yvals + grid_halfres[2])
    
  } else {
    locs <- try(
      as_points(xyspace, to_class = "sf", crs = crs_wkt),
      silent = TRUE
    )
    
    if (inherits(locs, "try-error")) {
      stop("Argument `xyspace` could not be interpreted as location.")
    }
    
    n_sites <- nrow(locs)
    tmp_coord <- sf::st_coordinates(locs) # coordinates of point locations
    xvals <- tmp_coord[, 1]
    yvals <- tmp_coord[, 2]
    
    if (data_dims["ns"] > n_sites) {
      stop(
        "`data_dims[\"ns\"]` must be smaller or equal to ",
        "the number of sites in `xyspace` for discrete data sites or points."
      )
    }
  }
  
  
  
  #------ time axis ------
  type_timeaxis <- match.arg(type_timeaxis)
  
  n_time <- length(time_values)
  
  has_T_timeAxis <- if (data_dims["nt"] > 0) {
    "explicit"
  } else if (n_time > 0) {
    "implicit"
  } else {
    "none"
  }
  
  if (has_T_timeAxis %in% c("explicit", "implicit")) {
    #--- Check time values/dimension match
    if (has_T_timeAxis == "explicit" && data_dims["nt"] != n_time) {
      stop(
        "`data_dims[\"nt\"]` must match ",
        "the number of elements in `time_values`."
      )
    }
    
    if (has_T_timeAxis == "implicit" && n_time != 1) {
      stop(
        "If `data_dims[\"nt\"]` is zero, ",
        "then `time_values` can only have one value."
      )
    }
    
    #--- Check and conform time_bounds
    if (length(dim(time_bounds)) == 0) {
      if (n_time * 2 != length(time_bounds)) {
        stop(
          "Start and end required for each `time_values` ",
          "to define `time_bounds`"
        )
      }
      
      time_bounds <- matrix(time_bounds, nrow = n_time, ncol = 2, byrow = TRUE)
    } else {
      if (!identical(dim(time_bounds), c(as.integer(n_time), 2L))) {
        stop(
          "Start and end required for each `time_values` ",
          "to define `time_bounds`"
        )
      }
    }
    
    #--- Identify type of time axis
    if (type_timeaxis == "timeseries") {
      varid_timebnds <- "time_bnds"
      att_timebnds <- "bounds"
      
    } else if (type_timeaxis == "climatology") {
      # nolint start
      # http://cfconventions.org/Data/cf-conventions/cf-conventions-1.8/cf-conventions.html#climatological-statistics
      # nolint end
      varid_timebnds <- "climatology_bounds" # not climatology_bnds!
      att_timebnds <- "climatology"
    }
    
    #--- Check time attributes
    if ("units" %in% names(time_attributes)) {
      time_units <- time_attributes[["units"]]
      time_attributes[["units"]] <- NULL
    } else {
      stop("Need units attribute in time attribute list")
    }
    
    if ("calendar" %in% names(time_attributes)) {
      time_cal <- time_attributes[["calendar"]]
      time_attributes[["calendar"]] <- NULL
    } else {
      stop("Need calendar attribute in time attribute list")
    }
    
    if ("unlim" %in% names(time_attributes)) {
      time_unlim <- as.logical(time_attributes[["unlim"]])
      time_attributes[["unlim"]] <- NULL
    } else {
      stop("Need unlim attribute in time attribute list")
    }
    
    if ("axis" %in% names(time_attributes)) {
      if ("T" != toupper(time_attributes[["axis"]])) {
        stop(
          "`time_attributes`: ",
          "if `axis` is included, then its value must be `T`"
        )
      }
      time_attributes[["axis"]] <- NULL
    }
    
    if (att_timebnds %in% names(time_attributes)) {
      if (varid_timebnds != time_attributes[[att_timebnds]]) {
        stop(
          "`time_attributes`: ",
          "if ", shQuote(att_timebnds), " is included, then its value must be ",
          shQuote(varid_timebnds)
        )
      }
      time_attributes[[att_timebnds]] <- NULL
    }
    
    not_att_timebnds <- switch(
      EXPR = type_timeaxis,
      timeseries = "climatology",
      climatology = "bounds"
    )
    
    if (not_att_timebnds %in% names(time_attributes)) {
      warning(
        "`time_attributes`: ",
        "the attribute ", shQuote(not_att_timebnds), " is ignored ",
        "because time represents a ", shQuote(type_timeaxis),
        "; instead, the automatically generated attribute ",
        shQuote(att_timebnds),
        " encodes the bounds of the time axis."
      )
      time_attributes[[not_att_timebnds]] <- NULL
    }
    
    ns_att_time <- names(time_attributes)
  }
  
  
  #------ vertical axis ------
  n_vertical <- length(vertical_values)
  
  has_Z_verticalAxis <- if (data_dims["nz"] > 0) {
    "explicit"
  } else if (n_vertical > 0) {
    "implicit"
  } else {
    "none"
  }
  
  if (has_Z_verticalAxis %in% c("explicit", "implicit")) {
    
    #--- Check time values/dimension match
    if (has_Z_verticalAxis == "explicit" && data_dims["nz"] != n_vertical) {
      stop(
        "`data_dims[\"nz\"]` must match ",
        "the number of elements in `vertical_values`."
      )
    }
    
    if (has_Z_verticalAxis == "implicit" && n_vertical != 1) {
      stop(
        "If `data_dims[\"nz\"]` is zero, ",
        "then `vertical_values` can only have one value."
      )
    }
    
    #--- Check and conform vertical_bounds
    if (length(dim(vertical_bounds)) == 0) {
      if (n_vertical * 2 != length(vertical_bounds)) {
        stop(
          "Start and end values required for each `vertical_values` ",
          "to define `vertical_bounds`"
        )
      }
      
      vertical_bounds <- matrix(
        vertical_bounds,
        nrow = n_vertical,
        ncol = 2,
        byrow = TRUE
      )
      
    } else {
      if (!identical(dim(vertical_bounds), c(as.integer(n_vertical), 2L))) {
        stop(
          "Start and end values required for each `vertical_values` ",
          "to define `vertical_bounds`"
        )
      }
    }
    
    
    #--- Check vertical attributes
    if ("units" %in% names(vertical_attributes)) {
      vert_units <- vertical_attributes[["units"]]
      vertical_attributes[["units"]] <- NULL
    } else {
      stop("Need `units` attribute in vertical attribute list")
    }
    
    if (!("positive" %in% names(vertical_attributes))) {
      stop("Need `positive` attribute in vertical attribute list")
    }
    
    if ("axis" %in% names(vertical_attributes)) {
      if ("Z" != toupper(vertical_attributes[["axis"]])) {
        stop(
          "`vertical_attributes`: ",
          "if `axis` is included, then its value must be `Z`"
        )
      }
      vertical_attributes[["axis"]] <- NULL
    }
    
    if ("bounds" %in% names(vertical_attributes)) {
      if ("vertical_bnds" != vertical_attributes[["bounds"]]) {
        stop(
          "`vertical_attributes`: ",
          "if `bounds` is included, then its value must be `vertical_bnds`"
        )
      }
      vertical_attributes[["bounds"]] <- NULL
    }
    
    ns_att_vert <- names(vertical_attributes)
  }
  
  
  #------ Variables ------
  n_vars <- max(1, data_dims["nv"]) # at least one implicit variable
  
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
  
  if (!("grid_mapping" %in% names(var_attributes))) {
    # This function creates only one grid_mapping and
    # the grid_mapping variable name is hard-coded to be "crs"
    var_attributes[["grid_mapping"]] <- paste(
      "crs:",
      # here, guessing axis order to be 1, 2
      # (that would be correct for OGC:CRS84, but not for EPSG:4326)
      xy_attributes[["name"]][1],
      xy_attributes[["name"]][2]
    )
    
    if (verbose) {
      message(
        "Adding `grid_mapping = \"", var_attributes[["grid_mapping"]], "\"`",
        " to variable attributes."
      )
    }
    
  } else {
    if (isTRUE(!grepl("\\<crs", var_attributes[["grid_mapping"]]))) {
      warning(
        "Variable attribute for `grid_mapping` should be 'crs: ...', but is ",
        shQuote(var_attributes[["grid_mapping"]])
      )
    }
  }
  
  if (!is_gridded) {
    # nolint start
    # http://cfconventions.org/Data/cf-conventions/cf-conventions-1.8/cf-conventions.html#point-data
    # nolint end
    if (!("coordinates" %in% names(var_attributes))) {
      tmp <- paste(xy_attributes[["name"]][2], xy_attributes[["name"]][1])
      if (has_T_timeAxis != "none") tmp <- paste("time", tmp)
      if (has_Z_verticalAxis != "none") tmp <- paste(tmp, "vertical")
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
      warning(
        "`missing_value` variable attribute is replaced by ",
        "automatically generated `_FillValue`."
      )
    }
    var_attributes[["missing_value"]] <- NULL
  }
  
  ns_att_vars <- names(var_attributes)
  
  
  
  #------ 2) Define netCDF dimensions ------------------------------------------
  #--- bounds dimension (without dimensional variable)
  bnddim <- pbdNCDF4::ncdim_def(
    name = "bnds",
    units = "",
    vals = seq_len(2L),
    create_dimvar = FALSE
  )
  
  # x and y dimension
  if (is_gridded) {
    xdim <- pbdNCDF4::ncdim_def(
      name = xy_attributes[["name"]][1],
      longname = xy_attributes[["long_name"]][1],
      units = xy_attributes[["units"]][1],
      vals = xvals
    )
    ydim <- pbdNCDF4::ncdim_def(
      name = xy_attributes[["name"]][2],
      longname = xy_attributes[["long_name"]][2],
      units = xy_attributes[["units"]][2],
      vals = yvals
    )
    
    var_dims <- list(xdim, ydim)
    var_chunksizes <- if (has_chunks) c(n_xvals, n_yvals) else NA
    var_start <- c(1, 1)
    
  } else {
    idim <- pbdNCDF4::ncdim_def(
      name = "site",
      longname = "SOILWAT2 simulation sites",
      units = "1",
      vals = seq_len(n_sites)
    )
    
    var_dims <- list(idim)
    var_chunksizes <- if (has_chunks) n_sites else NA
    var_start <- 1
  }
  
  # vertical dimension
  if (has_Z_verticalAxis != "none") {
    zdim <- pbdNCDF4::ncdim_def(
      name = "vertical",
      units = vert_units,
      vals = vertical_values
    )
    
    var_dims <- c(var_dims, list(zdim))
    if (has_chunks && has_predet_chunks) {
      var_chunksizes <- c(
        var_chunksizes,
        if (nc_chunks == "by_zt") 1 else n_vertical
      )
    }
    var_start <- c(var_start, 1)
  }
  
  # time dimension
  if (has_T_timeAxis != "none") {
    tdim <- pbdNCDF4::ncdim_def(
      name = "time",
      units = time_units,
      calendar = time_cal,
      unlim = time_unlim,
      vals = time_values
    )
    
    var_dims <- c(var_dims, list(tdim))
    if (has_chunks && has_predet_chunks) {
      var_chunksizes <- c(
        var_chunksizes,
        if (nc_chunks %in% c("by_zt", "by_t")) 1 else n_time
      )
    }
    var_start <- c(var_start, 1)
  }
  
  
  
  
  #------ 3) Define netCDF variables -------------------------------------------
  if (has_chunks && !has_predet_chunks) {
    stopifnot(length(nc_chunks) == length(var_dims))
    var_chunksizes <- nc_chunks
  }
  
  
  #------ Define data variables ------
  var_defs <- lapply(
    seq_len(n_vars),
    function(k) {
      pbdNCDF4::ncvar_def(
        name = var_names[k],
        units = var_units[k],
        dim = var_dims,
        compression = nc_deflate,
        shuffle = nc_shuffle,
        chunksizes = if (has_chunks) var_chunksizes else NA,
        missval = NAflag,
        prec = data_type
      )
    }
  )
  
  
  #------ Define x and y as variables if not gridded ------
  if (!is_gridded) {
    xvar <- pbdNCDF4::ncvar_def(
      name = xy_attributes[["name"]][1],
      longname = xy_attributes[["long_name"]][1],
      units = xy_attributes[["units"]][1],
      dim = list(idim),
      compression = nc_deflate,
      chunksizes = n_sites,
      missval = NAflag,
      prec = "double"
    )
    
    yvar <- pbdNCDF4::ncvar_def(
      name = xy_attributes[["name"]][2],
      longname = xy_attributes[["long_name"]][2],
      units = xy_attributes[["units"]][2],
      dim = list(idim),
      compression = nc_deflate,
      chunksizes = n_sites,
      missval = NAflag,
      prec = "double"
    )
    
    var_defs <- c(var_defs, list(yvar, xvar))
  }
  
  
  #------ Define CRS ------
  crsdef <- pbdNCDF4::ncvar_def(
    name = "crs",
    units = "",
    dim = list(),
    missval = NULL,
    prec = "integer"
  )
  
  
  #------ Define dimension bounds ------
  nc_dimvars <- if (is_gridded) {
    bnds_name <- paste0(xy_attributes[["name"]][1:2], "_bnds")
    
    if ("bounds" %in% names(xy_attributes)) {
      if (any(bnds_name != xy_attributes[["bounds"]])) {
        stop(
          "`xy_attributes`: ",
          "if `bounds` is included, then its value must be ",
          shQuote(bnds_name[1]), " and ", shQuote(bnds_name[2])
        )
      }
      xy_attributes[["bounds"]] <- NULL
    }
    
    list(
      pbdNCDF4::ncvar_def(
        name = bnds_name[1],
        units = "",
        dim = list(bnddim, xdim),
        missval = NULL,
        compression = nc_deflate,
        chunksizes = c(2L, n_xvals),
        prec = "double"
      ),
      
      pbdNCDF4::ncvar_def(
        name = bnds_name[2],
        units = "",
        dim = list(bnddim, ydim),
        missval = NULL,
        compression = nc_deflate,
        chunksizes = c(2L, n_yvals),
        prec = "double"
      )
    )
    
  } else {
    list()
  }
  
  if (has_Z_verticalAxis != "none") {
    vertbnddef <- pbdNCDF4::ncvar_def(
      name = "vertical_bnds",
      units = "",
      dim = list(bnddim, zdim),
      missval = NULL,
      compression = nc_deflate,
      chunksizes = c(2L, n_vertical),
      prec = "double"
    )
    
    nc_dimvars <- c(nc_dimvars, list(vertbnddef))
  }
  
  if (has_T_timeAxis != "none") {
    tbnddef <- pbdNCDF4::ncvar_def(
      name = varid_timebnds,
      units = "",
      dim = list(bnddim, tdim),
      missval = NULL,
      compression = nc_deflate,
      chunksizes = if (time_unlim) c(2L, 1L) else c(2L, n_time),
      prec = "double"
    )
    
    nc_dimvars <- c(nc_dimvars, list(tbnddef))
  }
  
  
  
  #------ 4) Write dimensions and attributes to netCDF file --------------------
  
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  
  if(!isParallel) {
    nc <- pbdNCDF4::nc_create(
      filename = filename,
      vars = c(nc_dimvars, list(crsdef), var_defs),
      force_v4 = TRUE
    )
  } else {
    nc <- pbdNCDF4::nc_create_par(
      filename = filename,
      vars = c(nc_dimvars, list(crsdef), var_defs),
      force_v4 = TRUE
    )
    
    nc_var_par_access(nc, var_names[1])
    
  }

  

  
  #------ Write dimensional variable values ------
  if (is_gridded) {
    #--- Write xy-bounds
    pbdNCDF4::ncvar_put(
      nc,
      varid = bnds_name[1],
      vals = x_bounds,
      start = c(1, 1),
      count = c(2L, n_xvals)
    )
    
    pbdNCDF4::ncvar_put(
      nc,
      varid = bnds_name[2],
      vals = y_bounds,
      start = c(1, 1),
      count = c(2L, n_yvals)
    )
    
  } else {
    #--- Write xy-coordinates to associated variables
    pbdNCDF4::ncvar_put(
      nc,
      varid = xy_attributes[["name"]][1],
      vals = xvals,
      start = 1,
      count = n_sites
    )
    
    pbdNCDF4::ncvar_put(
      nc,
      varid = xy_attributes[["name"]][2],
      vals = yvals,
      start = 1,
      count = n_sites
    )
  }
  
  if (has_Z_verticalAxis != "none") {
    pbdNCDF4::ncvar_put(
      nc,
      varid = "vertical_bnds",
      vals = t(vertical_bounds),
      start = c(1, 1),
      count = c(2L, n_vertical)
    )
  }
  
  if (has_T_timeAxis != "none") {
    pbdNCDF4::ncvar_put(
      nc,
      varid = varid_timebnds,
      vals = t(time_bounds),
      start = c(1, 1),
      count = c(2, n_time)
    )
  }
  
  
  
  #------ Write attributes ------
  
  #--- add standard_name attribute of x/y variables
  if ("standard_name" %in% names(xy_attributes)) {
    for (k in seq_len(2)) {
      pbdNCDF4::ncatt_put(
        nc,
        varid = xy_attributes[["name"]][k],
        attname = "standard_name",
        attval = xy_attributes[["standard_name"]][k]
      )
    }
  }
  
  #--- add dimension attributes
  if (is_gridded) {
    pbdNCDF4::ncatt_put(nc, xy_attributes[["name"]][1], "axis", "X")
    pbdNCDF4::ncatt_put(nc, xy_attributes[["name"]][1], "bounds", bnds_name[1])
    pbdNCDF4::ncatt_put(nc, xy_attributes[["name"]][2], "axis", "Y")
    pbdNCDF4::ncatt_put(nc, xy_attributes[["name"]][2], "bounds", bnds_name[2])
  }
  
  if (has_Z_verticalAxis != "none") {
    pbdNCDF4::ncatt_put(nc, "vertical", "axis", "Z")
    pbdNCDF4::ncatt_put(nc, "vertical", "bounds", "vertical_bnds")
    
    for (natt in ns_att_vert) {
      pbdNCDF4::ncatt_put(
        nc,
        varid = "vertical",
        attname = natt,
        attval = vertical_attributes[[natt]]
      )
    }
  }
  
  if (has_T_timeAxis != "none") {
    pbdNCDF4::ncatt_put(nc, "time", "axis", "T")
    pbdNCDF4::ncatt_put(nc, "time", att_timebnds, varid_timebnds)
    
    for (natt in ns_att_time) {
      pbdNCDF4::ncatt_put(
        nc,
        varid = "time",
        attname = natt,
        attval = time_attributes[[natt]]
      )
    }
  }
  
  
  #--- add variable attributes
  for (k in seq_len(n_vars)) {
    for (natt in ns_att_vars) {
      pbdNCDF4::ncatt_put(
        nc,
        varid = var_names[k],
        attname = natt,
        attval = var_attributes[[natt]][k]
      )
    }
  }
  
  #--- add coordinate system attributes
  for (natt in ns_att_crs) {
    pbdNCDF4::ncatt_put(
      nc,
      varid = "crs",
      attname = natt,
      attval = crs_attributes[[natt]]
    )
  }
  
  pbdNCDF4::ncatt_put(nc, "crs", attname = "crs_wkt", attval = crs_wkt$Wkt)
  
  
  #--- add global attributes
  pbdNCDF4::ncatt_put(nc, varid = 0, attname = "Conventions", attval = "CF-1.8")
  
  tmp_version_netcdf <- try(
    system2("nc-config", "--version", stdout = TRUE, stderr = TRUE),
    silent = TRUE
  )
  
  pbdNCDF4::ncatt_put(
    nc,
    varid = 0,
    attname = "created_by",
    attval = paste0(
      R.version[["version.string"]],
      ", R package ",
      "pbdNCDF4 v", getNamespaceVersion("pbdNCDF4"),
      if (!inherits(tmp_version_netcdf, "try-error")) {
        paste0(", and ", tmp_version_netcdf)
      }
    )
  )
  
  pbdNCDF4::ncatt_put(
    nc,
    varid = 0,
    attname = "creation_date",
    attval = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  if (!missing(global_attributes)) {
    ns_att_glob <- names(global_attributes)
    
    tmp <- c("Conventions", "created_by", "creation_date")
    if (has_T_timeAxis == "none") {
      tmp <- c(tmp, "time_label", "time_title")
    }
    has_replaced_gatts <- tmp[tmp %in% ns_att_glob]
    if (length(has_replaced_gatts) > 0) {
      warning(
        "`global_attributes` contained values for ",
        paste0(shQuote(has_replaced_gatts), collapse = ", "),
        "; they were replaced with an automatically generated value."
      )
      ns_att_glob <- setdiff(ns_att_glob, has_replaced_gatts)
    }
    
    for (natt in ns_att_glob) {
      pbdNCDF4::ncatt_put(
        nc,
        varid = 0,
        attname = natt,
        attval = global_attributes[[natt]]
      )
    }
  }
  
  if (has_T_timeAxis == "none") {
    pbdNCDF4::ncatt_put(nc, varid = 0, attname = "time_label", attval = "None")
    pbdNCDF4::ncatt_put(
      nc,
      varid = 0,
      attname = "time_title",
      attval = "No temporal dimensions ... fixed field"
    )
  }
  
  
  
  
  #------ The end --------------------------------------------------------------
  if (verbose) {
    message(
      "The netCDF has ", nc$nvars, " variables and ", nc$ndim, " dimensions"
    )
  }
  
  return(nc)
  invisible(TRUE)
}




