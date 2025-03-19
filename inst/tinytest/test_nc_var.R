# Returns a list describing a netCDF data variable

# Name and dims must be character
expect_error(nc_var(name = 1), "is\\.character\\(name\\) is not TRUE")
expect_error(nc_var("t2m", dims = 1), "is\\.character\\(dims\\) is not TRUE")

# Data must be data.frame with columns for variable name and dims
dims <- c("y", "x")
data <- data.frame(t2m = 1:4L, y = c(1L, 1L, 2L, 2L), x = c(1L, 2L, 1L, 2L))
expect_error(nc_var("foo", data, dims = dims), "Variable 'foo' not in data columns")
expect_error(nc_var("t2m", data[, -2], dims = dims), "Dimensions \\[y\\] not in data columns")

# Guesses data type if not provided
data_dt <- data.table::as.data.table(data[c(dims, "t2m")], key = rev(dims))
expect_equal(
  nc_var("t2m", data, dims = dims),
  list(
    name = "t2m", data = data_dt, dims = dims, type = "NC_INT", chunking = NA, deflate = NA,
    shuffle = FALSE
  )
)

# Can specify chunking
chunking <- c(y = 1, x = NA)
expect_equal(
  nc_var("t2m", data, dims = dims, chunking = chunking),
  list(
    name = "t2m", data = data_dt, dims = dims, type = "NC_INT", chunking = chunking,
    deflate = NA, shuffle = FALSE
  )
)
chunking <- TRUE
expect_equal(
  nc_var("t2m", data, dims = dims, chunking = chunking),
  list(
    name = "t2m", data = data_dt, dims = dims, type = "NC_INT", chunking = chunking,
    deflate = NA, shuffle = FALSE
  )
)

# Chunking must be NA, TRUE/FALSE, or numeric named with dimensions
expect_error(
  nc_var("t2m", data, dims = dims, chunking = list()),
  "Chunking must be NA, TRUE, FALSE, or numeric vector named with variable dimensions"
)

# Can specify recommended attributes
atts <- list(
  nc_att("long_name", "long name"),
  nc_att("standard_name", "standard name"),
  nc_att("units", "m")
)
expect_equal(
  nc_var(
    "t2m", data,
    dims = dims, long_name = "long name", standard_name = "standard name", units = "m"
  ),
  list(
    name = "t2m", data = data_dt, dims = dims, type = "NC_INT", chunking = NA, deflate = NA,
    shuffle = FALSE, atts = atts
  )
)

# Can specify grid_mapping
expect_equal(
  nc_var("t2m", data, dims = dims, grid_mapping = "crs"),
  list(
    name = "t2m", data = data_dt, dims = dims, type = "NC_INT", chunking = NA, deflate = NA,
    shuffle = FALSE, atts = list(nc_att("grid_mapping", "crs"))
  )
)

# Can specify packing
data$t2m <- as.numeric(data$t2m)
data_dt[, t2m := as.numeric(t2m)]
expect_equal(
  nc_var("t2m", data, dims = dims, pack_type = "NC_SHORT", scale_factor = 0.1),
  list(
    name = "t2m", data = data_dt, dims = dims, type = "NC_SHORT", chunking = NA, deflate = NA,
    shuffle = FALSE, atts = list(nc_att("scale_factor", 0.1))
  )
)

# Cannot pack data that is not NC_FLOAT or NC_DOUBLE
data$t2m <- as.integer(data$t2m)
data_dt[, t2m := as.integer(t2m)]
expect_error(
  nc_var("t2m", data, dims = dims, pack_type = "NC_SHORT", scale_factor = 0.1),
  "Cannot pack 'NC_INT' data; type must be one of \\[NC_FLOAT, NC_DOUBLE\\]"
)

# Can specify _FillValue
fill_val_att <- nc_att("_FillValue", -255L)
expect_equal(
  nc_var("t2m", data, dims = dims, fill_value = -255L),
  list(
    name = "t2m", data = data_dt, dims = dims, type = "NC_INT", chunking = NA, deflate = NA,
    shuffle = FALSE, atts = list(fill_val_att)
  )
)

# _FillValue has type of pack_type if packing
data$t2m <- as.numeric(data$t2m)
data_dt[, t2m := as.numeric(t2m)]
fill_val_att <- nc_att("_FillValue", -255L, "NC_SHORT")
expect_equal(
  nc_var("t2m", data, dims = dims, pack_type = "NC_SHORT", scale_factor = 0.1, fill_value = -255L),
  list(
    name = "t2m", data = data_dt, dims = dims, type = "NC_SHORT", chunking = NA, deflate = NA,
    shuffle = FALSE, atts = list(fill_val_att, nc_att("scale_factor", 0.1))
  )
)

# Can specify attributes
data$t2m <- as.integer(data$t2m)
data_dt[, t2m := as.integer(t2m)]
att <- nc_att("attribute", 1L)
att2 <- nc_att("attribute2", 2L)
expect_equal(
  nc_var("t2m", data, dims = dims, atts = list(att2, att)),
  list(
    name = "t2m", data = data_dt, dims = dims, type = "NC_INT", chunking = NA, deflate = NA,
    shuffle = FALSE, atts = list(att, att2)
  )
)

# Attributes cannot overlap with named arguments
bad_atts <- list(
  nc_att("units", "m"),
  nc_att("grid_mapping", "foo"),
  nc_att("_FillValue", -255)
)
expect_error(
  nc_var(
    "t2m", data, dims = dims, units = "m", grid_mapping = "foo", fill_value = -255, atts = bad_atts
  ),
  "Attributes \\[units, grid_mapping, _FillValue\\] should be passed as individual arguments"
)

# Attributes must be unique
expect_error(
  nc_var("t2m", data, dims = dims, units = "m", atts = list(att, att)),
  "Duplicate attributes: \\[attribute\\]"
)
