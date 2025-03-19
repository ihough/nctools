# Checks that a list describes a valid netCDF attribute

# Returns attribute if it is valid
att <- nc_att("foo", value = 1L, type = "NC_INT")
expect_equal(nctools:::check_nc_att(att), att)

# Must be list
expect_error(nctools:::check_nc_att("foo"), "nc_att must be a list")

# Must have name, value, and type
expect_error(
  nctools:::check_nc_att(list(value = 1, type = "NC_INT")),
  "nc_att must have 'name', 'type', and 'value'"
)
expect_error(
  nctools:::check_nc_att(list(name = "foo", type = "NC_INT")),
  "nc_att must have 'name', 'type', and 'value'"
)
expect_error(
  nctools:::check_nc_att(list(name = "foo", value = 1)),
  "nc_att must have 'name', 'type', and 'value'"
)

# Name must be character
att <- list(name = 1, value = 1, type = "NC_INT")
expect_error(nctools:::check_nc_att(att), "nc_att 'name' must be character")

# Value must be atomic
att <- list(name = "foo", value = list(1), type = "NC_INT")
expect_error(nctools:::check_nc_att(att), "nc_att 'value' must be atomic")

# Type must be one of NC_TYPES
att <- list(name = "foo", value = 1, type = "foo")
expect_error(nctools:::check_nc_att(att), "nc_att 'type' must be in NC_TYPES")
