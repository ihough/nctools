# Returns a list describing a netCDF variable that specifies a coordinate reference system

# name and grid_mapping must be character
expect_error(nc_crs(1), "is\\.character\\(name\\) is not TRUE")
expect_error(
  nc_crs(name = "foo", grid_mapping_name = 1), "is\\.character\\(grid_mapping_name\\) is not TRUE"
)

# Works with just name and grid_mapping_name
grid_mapping_name <- nc_att("grid_mapping_name", "sinusoidal")
expect_equal(
  nc_crs("wgs84", grid_mapping_name = "sinusoidal"),
  list(name = "wgs84", atts = list(grid_mapping_name))
)

# Can specify crs_wkt
crs_wkt <- nc_att("crs_wkt", "Fake WKT")
expect_equal(
  nc_crs("wgs84", grid_mapping_name = "sinusoidal", crs_wkt = "Fake WKT"),
  list(name = "wgs84", atts = list(crs_wkt, grid_mapping_name))
)

# Can specify earth_radius
earth_radius <- nc_att("earth_radius", 6378137L)
expect_equal(
  nc_crs("wgs84", grid_mapping_name = "sinusoidal", earth_radius = 6378137L),
  list(name = "wgs84", atts = list(earth_radius, grid_mapping_name))
)

# Can specify inverse_flattening
inverse_flattening <- nc_att("inverse_flattening", 298.257223563)
expect_equal(
  nc_crs("wgs84", grid_mapping_name = "sinusoidal", inverse_flattening = 298.257223563),
  list(name = "wgs84", atts = list(grid_mapping_name, inverse_flattening))
)

# Can specify longitude_of_prime_meridian
longitude_of_prime_meridian <- nc_att("longitude_of_prime_meridian", 0L)
expect_equal(
  nc_crs("wgs84", grid_mapping_name = "sinusoidal", longitude_of_prime_meridian = 0L),
  list(name = "wgs84", atts = list(grid_mapping_name, longitude_of_prime_meridian))
)

# Can specify prime_meridian_name
prime_meridian_name <- nc_att("prime_meridian_name", "Greenwich")
expect_equal(
  nc_crs("wgs84", grid_mapping_name = "sinusoidal", prime_meridian_name = "Greenwich"),
  list(name = "wgs84", atts = list(grid_mapping_name, prime_meridian_name))
)

# Can specify reference_ellipsoid_name
reference_ellipsoid_name <- nc_att("reference_ellipsoid_name", "WGS 84")
expect_equal(
  nc_crs("wgs84", grid_mapping_name = "sinusoidal", reference_ellipsoid_name = "WGS 84"),
  list(name = "wgs84", atts = list(grid_mapping_name, reference_ellipsoid_name))
)

# Can specify semi_major_axis
semi_major_axis <- nc_att("semi_major_axis", 6378137L)
expect_equal(
  nc_crs("wgs84", grid_mapping_name = "sinusoidal", semi_major_axis = 6378137L),
  list(name = "wgs84", atts = list(grid_mapping_name, semi_major_axis))
)

# Can specify semi_minor_axis
semi_minor_axis <- nc_att("semi_minor_axis", 6356752.314245)
expect_equal(
  nc_crs("wgs84", grid_mapping_name = "sinusoidal", semi_minor_axis = 6356752.314245),
  list(name = "wgs84", atts = list(grid_mapping_name, semi_minor_axis))
)

# Can specify additional attributes
false_easting <- nc_att("false_easting", 0L)
expect_equal(
  nc_crs("wgs84", grid_mapping_name = "sinusoidal", atts = false_easting),
  list(name = "wgs84", atts = list(false_easting, grid_mapping_name))
)

# Verifies no attributes duplicate named arguments
att <- nc_att("grid_mapping_name", "foo")
expect_error(
  nc_crs("wgs84", grid_mapping_name = "sinusoidal", atts = att),
  "CRS attributes \\[grid_mapping_name\\] should be passed as individual arguments, not in 'atts'"
)

# Verifies attributes are unique
expect_error(
  nc_crs("wgs84", grid_mapping_name = "sinusoidal", atts = list(false_easting, false_easting)),
  "Duplicate attributes: \\[false_easting\\]"
)
