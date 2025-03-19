library(tools)

library(data.table)
library(logger)
library(RNetCDF)

# Enable data.table's `[.data.table`() method
.datatable.aware <- TRUE

#' Valid netCDF types
#' @export
NC_TYPES <- c(
  "NC_BYTE", "NC_UBYTE", "NC_CHAR", "NC_SHORT", "NC_USHORT", "NC_INT", "NC_UINT", "NC_INT64",
  "NC_UINT64", "NC_FLOAT", "NC_DOUBLE", "NC_STRING"
)

#' Check that a list describes a netCDF attribute
#'
#' @description Ensures the list contains only 'name' (character), 'value' (atomic), and 'type' (in
#' [NC_TYPES])
#' @param x The list to check.
#' @seealso Use [nc_att()] to create a valid list describing a netCDF attribute
check_nc_att <- function(x) {
  stopifnot("nc_att must be a list" = is.list(x))
  stopifnot(
    "nc_att must have 'name', 'type', and 'value'" = setequal(names(x), c("name", "type", "value"))
  )
  stopifnot("nc_att 'name' must be character" = is.character(x$name))
  stopifnot("nc_att 'value' must be atomic" = is.atomic(x$value))
  stopifnot("nc_att 'type' must be in NC_TYPES" = x$type %in% NC_TYPES)
  invisible(x)
}

#' Fill gaps in a sequence
#'
#' @param x Numeric vector whose values differ by integer multiples of an increment e.g. c(1, 2, 4).
#' @param by The increment. If `NULL`, `by` is set to the smallest difference between values.
#' @param tolerance Tolerance to allow for floating point errors and imprecision. Values that differ
#' by < `tolerance` are deduplicated. Differences must be within `tolerance` of an integer multiple
#' of `by`.
#' @param decreasing Whether to sort the values in decreasing order.
#' @returns A increasing unique numeric vector whose values differ by the increment.
#' @export
#'
#' @examples
#' ## Fill a sequence that contains gaps
#' fill_seq(c(1, 2, 3, 5, 7)) # -> c(1, 2, 3, 4, 5, 6, 7)
#'
#' ## Specify the increment
#' fill_seq(c(1, 3, 5), by = 1) # -> c(1, 2, 3, 4, 5)
#'
#' ## Raises if differences between values are not integer multiples of increment
#' \dontrun{
#' fill_seq(c(1, 2, 3, 5.001))
#' }
#'
#' ## Allow slight variation in the differences between values
#' fill_seq(c(1, 2, 3, 5.001), tolerance = 1e-2) # -> c(1.000, 2.000, 3.000, 4.000, 5.001)
fill_seq <- function(x, by = NULL, tolerance = sqrt(.Machine$double.eps), decreasing = FALSE) {
  stopifnot(is.numeric(x))
  stopifnot(tolerance > 0)
  x <- unique(sort(x, decreasing = decreasing))
  stopifnot("length(x) < 3 after deduplicating" = length(x) > 2)

  # Remove values that differ by less than 'tolerance'
  diffs <- diff(x)
  mask <- abs(diffs) >= tolerance
  if (!any(mask)) {
    max_diff <- ifelse(decreasing, min(diffs), max(diffs))
    stop(
      "Max difference [", format(abs(max_diff), digits = 6), "] <= tolerance [",
      format(tolerance, digits = 3), "]. Decrease 'tolerance' to allow smaller increment."
    )
  }
  if (!all(mask)) {
    warning("Dropping ", sum(!mask), " values that differ by < ", format(tolerance, digits = 3))
  }
  x <- x[c(TRUE, mask)]
  if (length(x) < 3) {
    stop("length(x) < 3 after removing values that differ by < ", format(tolerance, digits = 3))
  }

  # Determine the increment
  diffs <- diff(x)
  if (missing(by)) {
    by <- ifelse(decreasing, max(diffs), min(diffs))
  } else {
    stopifnot(is.numeric(by))
    stopifnot(length(by) == 1)
    if (decreasing) {
      stopifnot("by must be < 0 for decreasing sequence" = by < 0)
    } else {
      stopifnot("by must be > 0 for ascending sequence" = by > 0)
    }
  }

  # Confirm all differences are integer multiples of increment
  mult_exact <- diffs / by
  mult <- round(mult_exact)
  mask <- abs(mult - mult_exact) >= tolerance
  if (any(mask)) {
    bad <- diffs[mask]
    if (length(bad) > 3) {
      bad <- toString(c(min(bad), "...", max(bad)))
    }
    stop(
      "Differences [", toString(bad), "] are not integer multiples of ", by
    )
  }

  # Fill gaps without replacing existing values
  fill_vals <- seq(
    from = ifelse(decreasing, max(x), min(x)), to = ifelse(decreasing, min(x), max(x)), by = by
  )
  idx <- cumsum(c(1, mult))
  x <- sort(c(fill_vals[-idx], x), decreasing = decreasing)

  return(x)
}

#' Iterate over a list of named lists, returning the 'name' element of each
#'
#' @param x A list of named lists.
#' @returns A character vector of names, or zero-length character vector if x is not a list
get_names <- function(x) {
  if (!is.list(x)) {
    return(character(0))
  }
  sapply(x, function(xi) xi$name)
}

#' Return the netCDF type corresponding to an object's class
#'
#' @param x An R object.
#' @returns The name of a netCDF type.
get_nctype <- function(x) {
  switch(class(x),
    character = "NC_CHAR",
    integer = "NC_INT", # 32-bit signed integer
    logical = "NC_BYTE",
    numeric = "NC_DOUBLE", # R numeric is 64-bit floating point
    stop("No known NC type for class ", class(x))
  )
}

#' Log a netCDF attribute and its value
#'
#' @description
#' Logs the name of a netCDF attribute and its value. The value is quoted if it is of type NC_CHAR.
#'
#' @param log_fun Logging function e.g. [logger::log_debug()].
#' @param varname Name of the NetCDF variable that the attribute describes.
#' @param attribute NetCDF attribute defined with [nc_att()].
log_attribute <- function(log_fun, varname, attribute) {
  att_logval <- attribute$value
  if (attribute$type == "NC_CHAR") {
    att_logval <- paste0("'", att_logval, "'")
  }
  log_fun("  ", varname, ":", attribute$name, " = ", att_logval)
}

#' Define a netCDF attribute
#'
#' @param name Name of the attribute.
#' @param value Value of the attribute.
#' @param type NetCDF type for `value`. If `NULL`, `type` is determined from `value`'s class. See
#' [NC_TYPES] for a list of valid types.
#'
#' @returns A list of the passed arguments.
#' @export
#'
#' @examples
#' nc_att("Test", value = "Test attribute", type = "NC_CHAR")
nc_att <- function(name, value, type = NULL) {
  stopifnot(is.character(name))
  stopifnot(is.atomic(value))

  if (missing(type)) {
    type <- get_nctype(value)
  }
  stopifnot(type %in% NC_TYPES)

  list(name = name, value = value, type = type)
}

#' Define a netCDF coordinate variable (dimension and variable)
#'
#' @param name Name of the variable. A dimension with this name will also be created.
#' @param values Values for the variable.
#' @param type NetCDF type for `values`. If `NULL`, `type` is determined from `value`'s class. See
#' [NC_TYPES] for a list of valid types.
#' @param long_name Optional descriptive name of the variable.
#' @param standard_name Optional standard name of the variable.
#' @param axis Optional axis (X, Y, Z, or T) represented by the variable.
#' @param units Optional units of the variable.
#' @param atts Optional list of attributes created with [nc_att()].
#' @param decreasing Whether the values should be sorted in decreasing order.
#' @param unlim Whether the dimension should be unlimited.
#' @param fill Whether to use [fill_seq()] to ensure values differ by a constant increment. This is
#' useful for coordinates of truncated regular grids (i.e. a regular grid where the coordinates of
#' some points are omitted because they have no data). When writing such data to a netCDF file, the
#' coordinates must contain a value for *all* points in the grid domain and the data variables must
#' have NA at coordinates with no data. Otherwise the data for the point preceding an omitted
#' coordinate will be interpreted as having an increased size along the omitted coordinate's
#' dimension (e.g. if one x-coordinate is omitted, the data for the previous x-coordinate will be
#' interpreted as having a width of two x-coordinates).
#' @param ... Arguments passed to [fill_seq()].
#'
#' @returns A list of the passed arguments
#' @export
#'
#' @seealso See https://cfconventions.org/ for recommended names, axis, and units.
#'
#' @examples
#' nc_coord("x", values = 1:5, type = "NC_INT")
#'
#' nc_coord("x", values = c(1, 2, 3, 5), fill = TRUE, atts = nc_att("long_name", "x coordinate"))
nc_coord <- function(
    name, values, type = NULL, long_name = NULL, standard_name = NULL, axis = NULL, units = NULL,
    atts = NULL, decreasing = FALSE, unlim = FALSE, fill = FALSE, ...) {
  stopifnot(is.character(name))
  stopifnot(is.numeric(values))

  # Make values unique, monotonic, and possibly ensure constant increment
  if (fill) {
    values <- fill_seq(values, ...)
  } else {
    values <- unique(sort(values, decreasing = decreasing))
  }

  # Get type if missing
  if (missing(type)) {
    type <- get_nctype(values)
  }
  stopifnot(type %in% NC_TYPES)

  # Make attributes for long_name, standard_name, axis, units (if any)
  arg_names <- names(as.list(match.call())[-1])
  arg_names <- intersect(arg_names, c("long_name", "standard_name", "axis", "units"))
  arg_atts <- NULL
  if (length(arg_names) > 0) {
    arg_atts <- lapply(arg_names, function(x) {
      nc_att(x, get(x))
    })
  }

  # Ensure atts is list of attribute definitions
  if (!is.null(atts) && !is.list(atts[[1]])) {
    atts <- list(atts)
  }
  for (att in atts) {
    check_nc_att(att)
  }

  # Ensure no duplicate attributes
  att_names <- get_names(atts)
  dups <- att_names %in% arg_names
  if (any(dups)) {
    stop(
      "Attributes [", toString(att_names[dups]),
      "] should be passed as individual arguments, not in 'atts'"
    )
  }
  counts <- table(att_names)
  if (any(counts > 1)) {
    stop("Duplicate attributes: [", toString(names(counts)[counts > 1]), "]")
  }
  atts <- c(atts, arg_atts)
  atts <- atts[order(get_names(atts))]

  coord <- list(name = name, values = values, type = type, atts = atts, unlim = unlim)
  Filter(Negate(is.null), coord)
}

#' Define a netCDF variable describing a coordinate reference system (CRS)
#'
#' @param name Name of the variable.
#' @param grid_mapping_name Value for the 'grid_mapping_name' attribute.
#' @param crs_wkt Optional well-known text representation of the CRS.
#' @param earth_radius Optional radius (m) of sphere used to approximate the earth.
#' @param inverse_flattening Optional inverse flattening of ellipsoid used to approximate the earth.
#' @param longitude_of_prime_meridian Optional long. of prime meridian with respect to Greenwich.
#' @param prime_meridian_name Optional name of prime meridian.
#' @param reference_ellipsoid_name Optional name of ellipsoid used to approximate the earth.
#' @param semi_major_axis Optional semi-major axis (m) of ellipsoid used to approximate the earth.
#' @param semi_minor_axis Optional semi-minor axis (m) of ellipsoid used to approximate the earth.
#' @param atts Optional list of grid mapping-specific attributes created with [nc_att()].
#'
#' @returns A list of the passed arguments.
#' @export
#'
#' @seealso
#' See https://cfconventions.org/Data/cf-conventions/cf-conventions-1.12/cf-conventions.html section
#' 5 "Coordinate Systems and Domain" for details on documenting coordinate systems.
#'
#' @examples
#' nc_crs("crs", grid_mapping_name = "sinusoidal", earth_radius = 6371007.181)
nc_crs <- function(
    name, grid_mapping_name, crs_wkt = NULL, earth_radius = NULL, inverse_flattening = NULL,
    longitude_of_prime_meridian = NULL, prime_meridian_name = NULL, reference_ellipsoid_name = NULL,
    semi_major_axis = NULL, semi_minor_axis = NULL, atts = NULL) {
  stopifnot(is.character(name))
  stopifnot(is.character(grid_mapping_name))

  # Convert most named args to attributes
  # Named args just serve as a reminder of the attributes that are valid for any grid mapping
  arg_names <- names(as.list(match.call())[-1])
  arg_names <- setdiff(arg_names, c("name", "atts"))
  arg_atts <- lapply(arg_names, function(x) {
    nc_att(x, get(x))
  })

  # Ensure atts is list of lists
  if (!missing(atts)) {
    if (!is.null(atts) && !is.list(atts[[1]])) {
      atts <- list(atts)
    }
    for (att in atts) {
      check_nc_att(att)
    }
  }

  # Ensure no duplicate attributes
  att_names <- get_names(atts)
  dups <- att_names %in% arg_names
  if (any(dups)) {
    stop(
      "CRS attributes [", toString(att_names[dups]),
      "] should be passed as individual arguments, not in 'atts'"
    )
  }
  counts <- table(att_names)
  if (any(counts > 1)) {
    stop("Duplicate attributes: [", toString(names(counts)[counts > 1]), "]")
  }
  atts <- c(atts, arg_atts)
  atts <- atts[order(get_names(atts))]

  list(name = name, atts = atts)
}

#' Define a netCDF variable
#'
#' @param name Name of the variable.
#' @param data A `data.frame` or `data.table` containing the variable's coordinates and values. The
#' column names must exactly match `dims` (the coordinates) and `name` (the values).
#' @param dims The names of the variable's dimensions. The last dimension varies fastest.
#' @param type NetCDF type for the values. If `NULL`, `type` is determined from the values' class.
#' If values are to be packed (see below), `type` must be the type of the *unpacked* (original)
#' values; the packed values will be written to the file as type `pack_type`. The resulting list
#' will have `type` set to `pack_type` to indicate that the data will be written as `pack_type`. See
#' [NC_TYPES] for a list of valid types.
#' @param pack_type,add_offset,scale_factor If defined, values will be packed to save space by first
#' subtracting `add_offset` then dividing by `scale_factor` and writing as type `pack_type`.
#' @param long_name Optional descriptive name of the variable.
#' @param standard_name Optional standard name of the variable.
#' @param units Optional units of the variable.
#' @param grid_mapping Optional name of a grid_mapping created with [nc_crs()]. For details, see
#' @param fill_value Optional value to use for missing data. If data are packed, `fill_value` must
#' be within range of For details, see
#' @param atts Optional list of attributes created with [nc_att()].
#' @param chunking Set to `NA` to allow the netCDF library to choose a storage layout, `FALSE` for
#' contiguous storage, or a vector named with the dimensions indicating the number of elements along
#' each dimension. Any `NA` elements in the vector will be replaced by the dimension's size.
#' @param deflate Compression level from 1 (min) to 9 (max) or `NA` (no compression).
#' @param shuffle Whether to enable byte shuffling, which may improve compression with `deflate`.
#'
#' @returns A list of the passed arguments.
#' @export
#'
#' @seealso See https://cfconventions.org/ for details on packing, grid mappings, and recommended
#' names and units. See https://docs.unidata.ucar.edu/nug/current/netcdf_perf_chunking.html for
#' details on chunking.
#'
#' @examples
#' ## Generate some data
#' data <- cbind(expand.grid(x = 1:3, y = 1:3), list(elev = 0:8 * 12.5))
#'
#' ## Define a variable
#' nc_var("elev", data = data, dims = c("y", "x"), atts = nc_att("long_name", "elevation"))
#'
#' ## Chunk data with each chunk containing 1 y-coordinate and all x-coordinates
#' nc_var("elev", data = data, dims = c("y", "x"), chunking = c(y = 1, x = NA))
#'
#' ## Pack to reduce file size: divide by 0.1 and write as 16-bit integers rather than 32-bit floats
#' nc_var("elev", data = data, dims = c("y", "x"), pack_type = "NC_SHORT", scale_factor = 0.1)
nc_var <- function(
    name, data, dims, type = NULL, pack_type = NULL, add_offset = NULL, scale_factor = NULL,
    long_name = NULL, standard_name = NULL, units = NULL, grid_mapping = NULL, fill_value = NULL,
    atts = NULL, chunking = NA, deflate = NA, shuffle = FALSE) {
  stopifnot(is.character(name))
  stopifnot(is.character(dims))

  # Ensure data is data.table with columns for all dims and variables
  stopifnot(is.data.frame(data))
  missing <- !dims %in% colnames(data)
  if (any(missing)) {
    stop(
      "Dimensions [", toString(dims[missing]),
      "] not in data columns [", toString(colnames(data)), "]"
    )
  }
  if (!name %in% colnames(data)) {
    stop("Variable '", name, "' not in data columns [", toString(colnames(data)), "]")
  }

  # Drop unneeded data columns and sort by reversed dimensions. RNetCDF needs the data dimensions to
  # be reversed because it writes data using R arrays, which have the first dimension varying
  # fastest, whereas netCDF files have the last dimension varying fastest.
  data <- data.table::as.data.table(data)
  data <- data[, c(dims, name), with = FALSE]
  data.table::setkeyv(data, rev(dims))

  # Get type if missing
  if (missing(type)) {
    type <- get_nctype(data[[name]])
  }
  stopifnot(type %in% NC_TYPES)

  # Parse chunking
  if (is.numeric(chunking)) {
    if (!setequal(names(chunking), dims)) {
      stop(
        "Chunking names [", toString(names(chunking)), "] must match dimensions [", toString(dims),
        "]"
      )
    }
  } else if (!is.logical(chunking)) {
    stop("Chunking must be NA, TRUE, FALSE, or numeric vector named with variable dimensions")
  }

  # Make attributes for long_name, standard_name, units, grid_mapping_name (if any)
  arg_atts <- NULL
  arg_names <- intersect(
    names(as.list(match.call())[-1]), c("long_name", "standard_name", "units", "grid_mapping")
  )
  if (length(arg_names) > 0) {
    arg_atts <- lapply(arg_names, function(x) nc_att(x, get(x)))
  }

  # Parse packing, if any
  if (!missing(pack_type) || !missing(scale_factor) || !missing(add_offset)) {
    # Ensure type and pack_type are appropriate
    valid <- list(
      "NC_FLOAT" = c("NC_BYTE", "NC_UBYTE", "NC_SHORT", "NC_USHORT"),
      "NC_DOUBLE" = c("NC_BYTE", "NC_UBYTE", "NC_SHORT", "NC_USHORT", "NC_INT", "NC_UINT")
    )
    if (!type %in% names(valid)) {
      stop(
        "Cannot pack '", type, "' data; type must be one of [", toString(names(valid)), "]"
      )
    }
    if (is.null(pack_type) || !pack_type %in% valid[[type]]) {
      stop(
        "pack_type must be one of [", toString(valid[[type]]), "]; got [", pack_type,
        "]"
      )
    }
    if (missing(scale_factor) && missing(add_offset)) {
      stop("Cannot pack data without scale_factor or offset")
    }

    # Define offset and scale. These must have type of *unpacked* (original) data
    if (!missing(add_offset)) {
      stopifnot(is.numeric(add_offset))
      arg_atts <- c(arg_atts, list(nc_att("add_offset", value = add_offset, type = type)))
    }
    if (!is.null(scale_factor)) {
      stopifnot(is.numeric(scale_factor))
      arg_atts <- c(arg_atts, list(nc_att("scale_factor", value = scale_factor, type = type)))
    }

    # Update the variable's type so that it will be written as pack_type
    type <- pack_type
  }

  # Define _FillValue attribute, if any
  # Do this after parsing packing b/c fill value must have type pack_type if data are packed
  if (!missing(fill_value)) {
    arg_atts <- c(arg_atts, list(nc_att("_FillValue", fill_value, type)))
  }

  # Ensure atts is list of lists
  if (!missing(atts)) {
    if (!is.null(atts) && !is.list(atts[[1]])) {
      atts <- list(atts)
    }
    for (att in atts) {
      check_nc_att(att)
    }
  }

  # Ensure no overlap between named args and attributes
  arg_names <- get_names(arg_atts)
  att_names <- get_names(atts)
  dups <- att_names %in% arg_names
  if (any(dups)) {
    stop(
      "Attributes [", toString(att_names[dups]),
      "] should be passed as individual arguments, not in 'atts'"
    )
  }
  counts <- table(att_names)
  if (any(counts > 1)) {
    stop("Duplicate attributes: [", toString(names(counts)[counts > 1]), "]")
  }

  # Sort attributes by name
  atts <- c(arg_atts, atts)
  atts <- atts[order(get_names(atts))]

  var <- list(
    name = name, data = data, dims = dims, type = type, chunking = chunking, deflate = deflate,
    shuffle = shuffle, atts = atts
  )
  Filter(Negate(is.null), var)
}

#' Create a netCDF file
#'
#' @param path Output path.
#' @param coords List of coordinate variables defined with [nc_coord()].
#' @param vars List of variables defined with [nc_var()].
#' @param crs Optional coordinate reference system defined with [nc_crs()].
#' @param atts Optional global attributes defined with [nc_att()].
#' @param force Whether to overwrite `path` if it exists.
#' @param quiet Whether to silence warnings about missing attributes suggested by CF conventions.
#' @param log_level Logging level for [logger] messages.
#' @param ... Arguments passed to [RNetCDF::var.put.nc()]
#'
#' @export
#'
#' @examples
#' ## Write a variable to a netCDF file
# coords = list(nc_coord("x", 1:3), nc_coord("y", 1:3))
# data <- cbind(expand.grid(x = 1:3, y = 1:3), list(elev = 0:8 * 12.5))
# elev <- nc_var("elev", data = data, dims = c("y", "x"), atts = nc_att("long_name", "elevation"))
# nc_write(file.path(tempdir(), "test.nc"), coords = list(x, y))
#' #
nc_write <- function(
    path, coords, vars, crs = NULL, atts = NULL, force = FALSE, quiet = FALSE,
    log_level = logger::WARN, ...) {
  logger::log_threshold(log_level)

  # Helper to ensure netCDF file is closed if an error occurs during writing
  write_safely <- function(nc, expr) {
    tryCatch(expr, finally = RNetCDF::close.nc(nc))
  }

  path <- file.path(normalizePath(dirname(path), mustWork = FALSE), basename(path))
  stopifnot(is.character(path))
  stopifnot(length(path) == 1)
  if (tools::file_ext(path) != "nc") {
    stop("File extension of ", path, " must be 'nc'; got '", tools::file_ext(path), "'")
  }
  if (file.exists(path) && !force) {
    stop("Path ", path, " exists; set force = TRUE to overwrite")
  }

  # Check coordinates
  stopifnot(is.list(coords))
  if (!is.list(coords[[1]])) {
    coords <- list(coords)
  }
  for (coord in coords) {
    name_counts <- table(names(coord))
    if (max(name_counts) > 1) {
      stop(
        "Coordinate '", coord["name"], "' has duplicate items: [",
        paste(names(name_counts), name_counts, sep = ": ", collapse = ", "), "]. Did you combine
        multiple coordinates with `c()` rather than `list()`?"
      )
    }

    if (!quiet) {
      # Warn if missing axis, long/standard name, or units attributes
      att_names <- get_names(coord$atts)
      if (!"axis" %in% att_names) {
        warning("Coordinate '", coord$name, "' has no 'axis' attribute")
      }
      if (!"long_name" %in% att_names && !"standard_name" %in% att_names) {
        warning("Coordinate '", coord$name, "' has no 'long_name' or 'standard_name' attribute")
      }
      if (!"units" %in% att_names) {
        warning("Coordinate '", coord$name, "' has no 'units' attribute")
      }
    }
  }
  names(coords) <- get_names(coords)

  # Check variables
  stopifnot(is.list(vars))
  if (!is.list(vars[[1]])) {
    vars <- list(vars)
  }
  for (v in vars) {
    name_counts <- table(names(v))
    if (max(name_counts) > 1) {
      stop(
        "Variable '", v["name"], "' has duplicate items: [",
        paste(names(name_counts), name_counts, sep = ": ", collapse = ", "), "]. Did you combine
        multiple variables with `c()` rather than `list()`?"
      )
    }

    # Ensure dimensions exist
    missing <- setdiff(v$dims, names(coords))
    if (length(missing) > 0) {
      stop("Dimensions [", toString(missing), "] for var [", v$name, "] not defined")
    }

    if (!quiet) {
      # Warn if missing long/standard name or units attributes
      att_names <- get_names(v$atts)
      if (!"long_name" %in% att_names && !"standard_name" %in% att_names) {
        warning("Variable '", v$name, "' has no 'long_name' or 'standard_name' attribute")
      }
      if (!"units" %in% att_names) {
        warning("Variable '", v$name, "' has no 'units' attribute")
      }

      # Warn if the variable is not linked to the CRS
      if (!is.null(crs)) {
        if (!"grid_mapping" %in% att_names) {
          warning("CRS is defined but variable '", v$name, "' has no 'grid_mapping' attribute")
        } else {
          v_grid_mapping <- v$atts[[which(att_names == "grid_mapping")]]$value
          if (v_grid_mapping != crs$name) {
            warning(
              "Variable '", v$name, "' has grid_mapping '", v_grid_mapping,
              "' which does not match CRS name '", crs$name, "'"
            )
          }
        }
      } else if ("grid_mapping" %in% att_names) {
        warning("Variable '", v$name, "' has 'grid_mapping' attribute but no CRS is defined")
      }
    }
  }
  names(vars) <- get_names(vars)

  # Check global attributes
  if (!is.null(atts)) {
    stopifnot(is.list(atts))
    if (!is.list(atts[[1]])) {
      atts <- list(atts)
    }
  }
  if (!quiet && !"title" %in% get_names(atts)) {
    warning("File has no global 'title' attribute")
  }

  # Create the output file
  logger::log_info("Creating ", path)
  nc <- RNetCDF::create.nc(
    path,
    clobber = force, prefill = FALSE, format = "netcdf4", diskless = FALSE
  )

  # Ensure the file is closed after any error
  write_safely(nc, {
    # Write any global attributes
    if (!is.null(atts)) {
      logger::log_info("Writing global attributes")
      for (att in atts) {
        log_attribute(logger::log_debug, "GLOBAL", att)
        RNetCDF::att.put.nc(
          nc,
          variable = "NC_GLOBAL", name = att$name, type = att$type, value = att$value
        )
      }
    }

    # Write any CRS
    if (!is.null(crs)) {
      logger::log_info("Writing CRS '", crs$name, "'")
      RNetCDF::var.def.nc(nc, crs$name, vartype = "NC_CHAR", dimensions = NA)
      for (att in crs$atts) {
        log_attribute(logger::log_debug, crs$name, att)
        RNetCDF::att.put.nc(
          nc,
          variable = crs$name, name = att$name, type = att$type, value = att$value
        )
      }
    }

    # Write coordinates
    for (coord in coords) {
      # Define and write
      logger::log_info("Writing coordinate '", coord$name, "'")
      RNetCDF::dim.def.nc(nc, coord$name, dimlength = length(coord$values), unlim = coord$unlim)
      RNetCDF::var.def.nc(nc, varname = coord$name, vartype = coord$type, dimensions = coord$name)
      for (att in coord$atts) {
        log_attribute(logger::log_debug, coord$name, att)
        RNetCDF::att.put.nc(
          nc,
          variable = coord$name, name = att$name, type = att$type, value = att$value
        )
      }
      RNetCDF::var.put.nc(nc, variable = coord$name, data = coord$values)
    }

    # Write variables
    for (v in vars) {
      # Parse chunking
      v$chunksizes <- NULL
      if (!is.null(names(v$chunking))) {
        v$chunksizes <- v$chunking[v$dims]
        v$chunking <- TRUE
        na_dims <- names(v$chunksizes)[is.na(v$chunksizes)]
        v$chunksizes[na_dims] <- sapply(coords[na_dims], function(x) {
          length(x$values)
        })
      }

      # Define the variable
      # NOTE: We reverse the dimension order when defining the variable because RNetCDF writes data
      # with the last dimension varying fastest whereas in R arrays the first dimension varies
      # fastest. Reversing the dimensions when defining the variable results in a variable whose
      # dimensions are listed in the same order as v$dims with the last dimension varying fastest.
      logger::log_debug(
        "Defining variable '", v$name, "' with dims [", toString(v$dims), "]"
      )
      nc_dims <- rev(v$dims)
      RNetCDF::var.def.nc(
        nc,
        varname = v$name,
        vartype = v$type,
        dimensions = nc_dims,
        chunking = v$chunking,
        chunksizes = v$chunksizes[nc_dims], # chunksizes must match (reversed) dimension order
        deflate = v$deflate,
        shuffle = v$shuffle,
        ...
      )

      # Write attributes
      for (att in v$atts) {
        logger::log_debug("  ", v$name, ":", att$name, " = ", att$value)
        RNetCDF::att.put.nc(
          nc,
          variable = v$name, name = att$name, type = att$type, value = att$value
        )
      }

      # Prepare a lookup table to select and sort the data
      # This ensures the data are sorted by the specified dimensions and adds rows with NA for any
      # coordinates that were not included in the data (e.g. if data are on a truncated grid)
      idx <- lapply(v$dims, function(x) coords[[x]]$values)
      names(idx) <- v$dims
      idx <- do.call(data.table::CJ, idx)

      # Write data
      # NOTE: counts must match the order of the variable's (reversed) dimensions
      logger::log_info(
        "Writing variable '", v$name, "' with dims [", toString(v$dims), "]"
      )
      RNetCDF::var.put.nc(
        nc,
        variable = v$name,
        data = unlist(v$data[idx, v$name, with = FALSE, on = nc_dims], use.names = FALSE),
        count = sapply(nc_dims, function(x) {
          length(coords[[x]]$values)
        }),
        pack = TRUE # only applies if variable has 'add_offset' or 'add_scale' attribute
      )

      # Sync changes to disk
      RNetCDF::sync.nc(nc)
    }
  })

  logger::log_info("Closing ", path)
}
