# Returns a list describing a netCDF attribute

# Name must be character
expect_error(nc_att(1, value = 1), "is\\.character\\(name\\) is not TRUE")

# Value must be atomic
expect_error(nc_att("foo", value = list(1), type = "NC_INT"), "is\\.atomic\\(value\\) is not TRUE")

# Guesses type if not provided
expect_equal(
  nc_att("attribute", 1L),
  list(name = "attribute", value = 1, type = "NC_INT")
)

# Can specify type
expect_equal(
  nc_att("attribute", 1L, type = "NC_FLOAT"),
  list(name = "attribute", value = 1, type = "NC_FLOAT")
)

# Type must be valid
expect_error(nc_att("foo", value = 1, type = "foo"), "type %in% NC_TYPES is not TRUE")
