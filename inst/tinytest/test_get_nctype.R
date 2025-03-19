# Returns the netCDF type for an R object

# integer
expect_equal(nctools:::get_nctype(1L), "NC_INT")

# numeric
expect_equal(nctools:::get_nctype(1.0), "NC_DOUBLE")

# logical
expect_equal(nctools:::get_nctype(TRUE), "NC_BYTE")

# character
expect_equal(nctools:::get_nctype("foo"), "NC_CHAR")

# raises if unrecognized
expect_error(nctools:::get_nctype(NULL), "No known NC type for class")
