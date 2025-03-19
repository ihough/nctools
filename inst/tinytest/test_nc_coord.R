# Returns a list describing a netCDF coordinate variable (dimension + variable)

# Name must be character
expect_error(nc_coord(1, values = 1:5), "is\\.character\\(name\\) is not TRUE")

# Values must be numeric
expect_error(nc_coord("x", values = c("a", "b", "c")), "is\\.numeric\\(values\\) is not TRUE")

# Guesses type if not provided
expect_equal(
  nc_coord("x", 1:5),
  list(name = "x", values = 1:5, type = "NC_INT", unlim = FALSE)
)

# Can specify type
expect_equal(
  nc_coord("x", 1:5, type = "NC_FLOAT"),
  list(name = "x", values = 1:5, type = "NC_FLOAT", unlim = FALSE)
)

# Can specify attributes
att <- nc_att("attribute", 1)
expect_equal(
  nc_coord("x", 1:5, atts = att),
  list(name = "x", values = 1:5, type = "NC_INT", atts = list(att), unlim = FALSE)
)

# Can specify multiple attributes
att <- nc_att("attribute", 1)
att2 <- nc_att("attribute2", 2)
expect_equal(
  nc_coord("x", 1:5, atts = list(att, att2)),
  list(name = "x", values = 1:5, type = "NC_INT", atts = list(att, att2), unlim = FALSE)
)

# Can specify whether variable's dimension should be unlimited
expect_equal(
  nc_coord("x", 1:5, unlim = TRUE),
  list(name = "x", values = 1:5, type = "NC_INT", unlim = TRUE)
)

# Can fill gaps in the values sequence using `fill_seq()`
expect_equal(
  nc_coord("x", c(1L, 2L, 3L, 5L), fill = TRUE),
  list(name = "x", values = 1:5, type = "NC_INT", unlim = FALSE)
)

# Additional args are passed to `fill_seq()`
expect_equal(
  nc_coord("x", c(1L, 5L, 9L), fill = TRUE, by = 2L),
  list(name = "x", values = seq(1L, 9L, by = 2L), type = "NC_INT", unlim = FALSE)
)

# Can provide recommended attributes as named arguments
atts <- list(
  nc_att("axis", "X"),
  nc_att("long_name", "long name"),
  nc_att("standard_name", "standard name"),
  nc_att("units", "m")
)
expect_equal(
  nc_coord(
    "x", 1:5,
    long_name = "long name", standard_name = "standard name", axis = "X", units = "m"
  ),
  list(name = "x", values = 1:5, type = "NC_INT", atts = atts, unlim = FALSE)
)

# Doesn't allow attributes that duplicate named arguments
expect_error(
  nc_coord("x", 1:5, axis = "X", atts = nc_att("axis", "X")),
  "Attributes \\[axis\\] should be passed as individual arguments, not in 'atts'"
)

# Doesn't allow duplicate attributes
expect_error(
  nc_coord("x", 1:5, atts = list(att, att)), "Duplicate attributes: \\[attribute\\]"
)
