# Write to a netCDF file

# Writes 1D data
path <- tempfile(fileext = ".nc")
x <- nc_coord("x", 1:3, axis = "X", standard_name = "projection_x_coordinate", units = "m")
elev <- nc_var(
  "elev", data = data.frame(x = 1:3, elev = 1:3), dims = "x", long_name = "elevation", units = "m"
)
title_att <- nc_att("title", "1D elevation")
nc_write(path, coords = x, vars = elev, atts = title_att)
nc <- RNetCDF::open.nc(path)
expect_equal(RNetCDF::att.get.nc(nc, "NC_GLOBAL", "title"), "1D elevation")
expect_equal(RNetCDF::dim.inq.nc(nc, "x")$name, x$name)
expect_equal(RNetCDF::var.inq.nc(nc, "x")$type, "NC_INT")
expect_equal(RNetCDF::var.get.nc(nc, "x"), as.array(x$values))
expect_equal(RNetCDF::att.get.nc(nc, "x", "axis"), "X")
expect_equal(RNetCDF::att.get.nc(nc, "x", "standard_name"), "projection_x_coordinate")
expect_equal(RNetCDF::att.get.nc(nc, "x", "units"), "m")
expect_equal(RNetCDF::var.inq.nc(nc, "elev")$name, elev$name)
expect_equal(RNetCDF::var.inq.nc(nc, "elev")$type, "NC_INT")
expect_equal(RNetCDF::var.get.nc(nc, "elev"), as.array(elev$data$elev))
expect_equal(RNetCDF::att.get.nc(nc, "elev", "long_name"), "elevation")
expect_equal(RNetCDF::att.get.nc(nc, "elev", "units"), "m")
RNetCDF::close.nc(nc)
file.remove(path)

# Output path must have file extension .nc
expect_error(nc_write(path = "foo"), "File extension of .+ must be 'nc'")
expect_false(file.exists(path))

# Doesn't overwrite unless force == TRUE
file.create(path)
expect_equal(file.size(path), 0)
expect_error(nc_write(path = path), "Path .+ exists; set force = TRUE to overwrite")
nc_write(path, coords = x, vars = elev, atts = title_att, force = TRUE)
expect_true(file.size(path) > 0)
file.remove(path)

# Warns if no global 'title' attribute
expect_warning(nc_write(path, coords = x, vars = elev), "File has no global 'title' attribute")
file.remove(path)

# Warns if coordinates are missing recommended attributes
expect_warning(
  nc_write(
    path, coords = nc_coord("x", 1:3, standard_name = "projection_x_coordinate", units = "m"),
    vars = elev, atts = title_att
  ),
  "Coordinate 'x' has no 'axis' attribute"
)
file.remove(path)
expect_warning(
  nc_write(
    path, coords = nc_coord("x", 1:3, axis = "X", units = "m"), vars = elev, atts = title_att
  ),
  "Coordinate 'x' has no 'long_name' or 'standard_name' attribute"
)
file.remove(path)
expect_warning(
  nc_write(
    path, coords = nc_coord("x", 1:3, axis = "X", standard_name = "projection_x_coordinate"),
    vars = elev, atts = title_att
  ),
  "Coordinate 'x' has no 'units' attribute"
)
file.remove(path)

# Warns if variables are missing recommended attributes
expect_warning(
  nc_write(
    path, coords = x, atts = title_att,
    vars = nc_var("elev", data.frame(x = 1:3, elev = 1:3), dims = "x", units = "m")
  ),
  "Variable 'elev' has no 'long_name' or 'standard_name' attribute"
)
file.remove(path)
expect_warning(
  nc_write(
    path, coords = x, atts = title_att,
    vars = nc_var("elev", data.frame(x = 1:3, elev = 1:3), dims = "x", long_name = "elevation")
  ),
  "Variable 'elev' has no 'units' attribute"
)
file.remove(path)

# Warns if variable has grid_mapping but no CRS defined
elev <- nc_var(
  "elev", data = data.frame(x = 1:3, elev = 1:3), dims = "x", long_name = "elevation", units = "m",
  grid_mapping = "crs"
)
expect_warning(
  nc_write(path, coords = x, vars = elev, atts = title_att),
  "Variable 'elev' has 'grid_mapping' attribute but no CRS is defined"
)
file.remove(path)

# Writes CRS
crs <- nc_crs("crs", grid_mapping_name = "sinusoidal", earth_radius = 6371007.181)
nc_write(path, coords = x, vars = elev, crs = crs, atts = title_att)
nc <- RNetCDF::open.nc(path)
expect_equal(RNetCDF::var.inq.nc(nc, "crs")$name, crs$name)
expect_equal(RNetCDF::var.inq.nc(nc, "crs")$type, "NC_CHAR")
expect_equal(RNetCDF::att.get.nc(nc, "crs", "grid_mapping_name"), "sinusoidal")
expect_equal(RNetCDF::att.get.nc(nc, "crs", "earth_radius"), 6371007.181)
expect_equal(RNetCDF::att.get.nc(nc, "elev", "grid_mapping"), "crs")
RNetCDF::close.nc(nc)
file.remove(path)

# Warns if CRS is defined and variables have no grid_mapping attribute
elev <- nc_var(
  "elev", data = data.frame(x = 1:3, elev = 1:3), dims = "x", long_name = "elevation", units = "m"
)
expect_warning(
  nc_write(path, coords = x, vars = elev, crs = crs, atts = title_att),
  "CRS is defined but variable 'elev' has no 'grid_mapping' attribute"
)
file.remove(path)

# Warns if variable grid_mapping != CRS name
elev <- nc_var(
  "elev", data = data.frame(x = 1:3, elev = 1:3), dims = "x", long_name = "elevation", units = "m",
  grid_mapping = "sinusoidal"
)
expect_warning(
  nc_write(path, coords = x, vars = elev, crs = crs, atts = title_att),
  "Variable 'elev' has grid_mapping 'sinusoidal' which does not match CRS name 'crs'"
)
file.remove(path)

# quiet = TRUE silences warnings about missing attributes, etc.
elev <- nc_var("elev", data = data.frame(x = 1:3, elev = 1:3), dims = "x")
expect_silent(
  nc_write(path, coords = nc_coord("x", 1:3), vars = elev, quiet = TRUE)
)
file.remove(path)

# Writes 2D data
lon <- nc_coord("lon", 1:3, axis = "X", standard_name = "longitude", units = "degrees_east")
lat <- nc_coord("lat", 1:3, axis = "Y", standard_name = "latitude", units = "degrees_north")
title_att <- nc_att("title", "2D elevation")
elev_data <- data.frame(lat = rep(3:1, each = 3), lon = rep(1:3, times = 3), elev = 0:8 * 12.5)
elev <- nc_var(
  "elev", data = elev_data, dims = c("lat", "lon"), long_name = "elevation", units = "m"
)
nc_write(path, coords = list(lat, lon), vars = elev, atts = title_att)
nc <- RNetCDF::open.nc(path)
expect_equal(RNetCDF::dim.inq.nc(nc, "lon")$name, lon$name)
expect_equal(RNetCDF::var.get.nc(nc, "lon"), as.array(lon$values))
expect_equal(RNetCDF::att.get.nc(nc, "lon", "axis"), "X")
expect_equal(RNetCDF::att.get.nc(nc, "lon", "standard_name"), "longitude")
expect_equal(RNetCDF::att.get.nc(nc, "lon", "units"), "degrees_east")
expect_equal(RNetCDF::dim.inq.nc(nc, "lat")$name, lat$name)
expect_equal(RNetCDF::var.get.nc(nc, "lat"), as.array(lat$values))
expect_equal(RNetCDF::att.get.nc(nc, "lat", "axis"), "Y")
expect_equal(RNetCDF::att.get.nc(nc, "lat", "standard_name"), "latitude")
expect_equal(RNetCDF::att.get.nc(nc, "lat", "units"), "degrees_north")
expect_equal(RNetCDF::var.inq.nc(nc, "elev")$name, elev$name)
expect_equal(RNetCDF::var.inq.nc(nc, "elev")$type, "NC_DOUBLE")
expect_equal(RNetCDF::var.get.nc(nc, "elev"), matrix(elev$data$elev, nrow = 3, byrow = TRUE))
expect_equal(RNetCDF::att.get.nc(nc, "elev", "long_name"), "elevation")
expect_equal(RNetCDF::att.get.nc(nc, "elev", "units"), "m")
RNetCDF::close.nc(nc)
file.remove(path)

# Checks coordinates is list of lists
expect_error(
  nc_write(path, coords = c(x, y), "Coordinate .+ has duplicate items")
)
expect_false(file.exists(path))

# Writes with chunking
elev <- nc_var(
  "elev", data = elev_data, dims = c("lat", "lon"), long_name = "elevation", units = "m",
  chunking = TRUE
)
nc_write(path, coords = list(lat, lon), vars = elev, atts = title_att)
nc <- RNetCDF::open.nc(path)
expect_equal(RNetCDF::var.inq.nc(nc, "elev")$chunksizes, c(3, 3))
RNetCDF::close.nc(nc)
file.remove(path)

# Writes with custom chunk sizes
elev <- nc_var(
  "elev", data = elev_data, dims = c("lat", "lon"), long_name = "elevation", units = "m",
  chunking = c(lat = 1, lon = NA)
)
nc_write(path, coords = list(lat, lon), vars = elev, atts = title_att)
nc <- RNetCDF::open.nc(path)
expect_equal(RNetCDF::var.inq.nc(nc, "elev")$chunksizes, c(3, 1))
RNetCDF::close.nc(nc)
file.remove(path)

# Writes with compression
elev <- nc_var(
  "elev", data = elev_data, dims = c("lat", "lon"), long_name = "elevation", units = "m", deflate = 9
)
nc_write(path, coords = list(lat, lon), vars = elev, atts = title_att)
nc <- RNetCDF::open.nc(path)
expect_equal(RNetCDF::var.inq.nc(nc, "elev")$deflate, 9)
expect_equal(RNetCDF::var.inq.nc(nc, "elev")$shuffle, FALSE)
RNetCDF::close.nc(nc)
file.remove(path)

# Writes with compression and shuffling
elev <- nc_var(
  "elev", data = elev_data, dims = c("lat", "lon"), long_name = "elevation", units = "m", deflate = 9,
  shuffle = TRUE
)
nc_write(path, coords = list(lat, lon), vars = elev, atts = title_att)
nc <- RNetCDF::open.nc(path)
expect_equal(RNetCDF::var.inq.nc(nc, "elev")$deflate, 9)
expect_equal(RNetCDF::var.inq.nc(nc, "elev")$shuffle, TRUE)
RNetCDF::close.nc(nc)
file.remove(path)

# Writes packed data
elev <- nc_var(
  "elev", data = elev_data, dims = c("lat", "lon"), long_name = "elevation", units = "m",
  pack_type = "NC_INT", scale_factor = 0.1
)
nc_write(path, coords = list(lat, lon), vars = elev, atts = title_att)
nc <- RNetCDF::open.nc(path)
expect_equal(RNetCDF::att.inq.nc(nc, "elev", "scale_factor")$type, "NC_DOUBLE")
expect_equal(RNetCDF::att.get.nc(nc, "elev", "scale_factor"), 0.1)
expect_equal(RNetCDF::var.inq.nc(nc, "elev")$type, "NC_INT")
expect_equal(
  RNetCDF::var.get.nc(nc, "elev", unpack = FALSE),
  matrix(elev$data$elev, nrow = 3, byrow = TRUE) / 0.1
)
expect_equal(
  RNetCDF::var.get.nc(nc, "elev", unpack = TRUE),
  matrix(elev$data$elev, nrow = 3, byrow = TRUE)
)
RNetCDF::close.nc(nc)
file.remove(path)

# Writes _FillValue
elev <- nc_var(
  "elev", data = data.frame(lon = 1:3, elev = c(NA, 2, 3)), dims = "lon", long_name = "elevation",
  units = "m", fill_value = -255
)
nc_write(path, coords = lon, vars = elev, atts = title_att)
nc <- RNetCDF::open.nc(path)
expect_equal(RNetCDF::att.inq.nc(nc, "elev", "_FillValue")$type, "NC_DOUBLE")
expect_equal(RNetCDF::att.get.nc(nc, "elev", "_FillValue"), -255)
expect_equal(RNetCDF::var.get.nc(nc, "elev", na.mode = 3), array(c(-255, 2, 3)))
RNetCDF::close.nc(nc)
file.remove(path)

# _FillValue has same type as packed data
elev <- nc_var(
  "elev", data = data.frame(lon = 1:3, elev = c(NA, 2, 3)), dims = "lon", long_name = "elevation",
  units = "m", fill_value = -255, pack_type = "NC_INT", scale_factor = 0.1
)
nc_write(path, coords = lon, vars = elev, atts = title_att)
nc <- RNetCDF::open.nc(path)
expect_equal(RNetCDF::att.inq.nc(nc, "elev", "_FillValue")$type, "NC_INT")
expect_equal(RNetCDF::att.get.nc(nc, "elev", "_FillValue"), -255)
expect_equal(RNetCDF::var.get.nc(nc, "elev", na.mode = 3), array(c(-255, 20, 30)))
RNetCDF::close.nc(nc)
file.remove(path)

# Writes 3D data
x <- nc_coord("x", 1:3 * 1000, axis = "X", standard_name = "projection_x_coordinate", units = "m")
y <- nc_coord("y", 1:3 * 1000, axis = "Y", standard_name = "projection_y_coordinate", units = "m")
time <- nc_coord("time", 1:3, axis = "T", standard_name = "time", units = "days since 1970-01-01")
crs <- nc_crs("crs", grid_mapping_name = "sinusoidal", earth_radius = 6371007.181)
title_att <- nc_att("title", "Daily temperature")
t2m_data <- cbind(expand.grid(x = 1:3 * 1000, y = 3:1 * 1000, time = 1:3), t2m = c(1:9, 2:10, 3:11))
t2m <- nc_var(
  "t2m", data = t2m_data, dims = c("time", "y", "x"), long_name = "temperature", units = "degC",
  grid_mapping = "crs"
)
nc_write(path, coords = list(x, y, time), vars = t2m, crs = crs, atts = title_att)
nc <- RNetCDF::open.nc(path)
expect_equal(RNetCDF::dim.inq.nc(nc, "x")$name, x$name)
expect_equal(RNetCDF::var.get.nc(nc, "x"), as.array(x$values))
expect_equal(RNetCDF::att.get.nc(nc, "x", "axis"), "X")
expect_equal(RNetCDF::att.get.nc(nc, "x", "standard_name"), "projection_x_coordinate")
expect_equal(RNetCDF::att.get.nc(nc, "x", "units"), "m")
expect_equal(RNetCDF::dim.inq.nc(nc, "y")$name, y$name)
expect_equal(RNetCDF::var.get.nc(nc, "y"), as.array(y$values))
expect_equal(RNetCDF::att.get.nc(nc, "y", "axis"), "Y")
expect_equal(RNetCDF::att.get.nc(nc, "y", "standard_name"), "projection_y_coordinate")
expect_equal(RNetCDF::att.get.nc(nc, "y", "units"), "m")
expect_equal(RNetCDF::dim.inq.nc(nc, "time")$name, time$name)
expect_equal(RNetCDF::var.get.nc(nc, "time"), as.array(time$values))
expect_equal(RNetCDF::att.get.nc(nc, "time", "axis"), "T")
expect_equal(RNetCDF::att.get.nc(nc, "time", "standard_name"), "time")
expect_equal(RNetCDF::att.get.nc(nc, "time", "units"), "days since 1970-01-01")
expect_equal(RNetCDF::var.inq.nc(nc, "t2m")$name, t2m$name)
expect_equal(RNetCDF::var.inq.nc(nc, "t2m")$type, "NC_INT")
expect_equal(RNetCDF::var.get.nc(nc, "t2m"), array(t2m$data$t2m, dim = c(3, 3, 3)))
expect_equal(RNetCDF::att.get.nc(nc, "t2m", "long_name"), "temperature")
expect_equal(RNetCDF::att.get.nc(nc, "t2m", "units"), "degC")
expect_equal(RNetCDF::att.get.nc(nc, "t2m", "grid_mapping"), "crs")
RNetCDF::close.nc(nc)
file.remove(path)
