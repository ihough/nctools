# Works with increasing sequence
x <- c(1, 2, 4)
result <- fill_seq(x)
expect_equal(result, 1:4)

# Works with decreasing sequence
x <- c(1, 2, 4)
result <- fill_seq(x, decreasing = TRUE)
expect_equal(result, 4:1)

# Uses smallest difference as increment
x <- c(1, 3, 7)
result <- fill_seq(x)
expect_equal(result, c(1, 3, 5, 7))

# Allows specifying increment
x <- c(1, 3, 7)
result <- fill_seq(x, by = 1)
expect_equal(result, 1:7)

# Stops if increment has wrong sign
x <- c(1, 3, 7)
expect_error(fill_seq(x, by = -1), "by must be > 0 for ascending sequence")
expect_error(fill_seq(x, by = 1, decreasing = TRUE), "by must be < 0 for decreasing")

# Works with negative values
x <- c(-3, -2, 0, 3)
result <- fill_seq(x)
expect_equal(result, -3:3)

# Works with small differences
x <- c(1.000001, 1.000002, 1.000004)
result <- fill_seq(x)
expect_equal(result, c(1.000001, 1.000002, 1.000003, 1.000004))

# Works with very small values
x <- c(1, 2, 4) * 1e-25
result <- fill_seq(x, tolerance = 1e-25)
expect_equal(result, 1:4 * 1e-25)

# Deduplicates input and sorts increasing
x <- c(4, 2, 2, 1)
result <- fill_seq(x)
expect_equal(result, 1:4)

# Deduplicates values that differ by < 'tolerance'
x <- c(1, 2, 4, 4 + 1e-8)
expect_warning(result <- fill_seq(x), "Dropping 1 values that differ by <")
expect_equal(result, 1:4)

# Deduplication works with decreasing sequence
x <- c(1, 2, 4, 4 + 1e-8)
expect_warning(result <- fill_seq(x, decreasing = TRUE), "Dropping 1 values that differ by <")
expect_equal(result, 4:1)

# Stops if differences are not integer multiples of increment
x <- c(1, 2, 4 + 1e-8)
expect_error(
  fill_seq(x, tolerance = 1e-9), "Differences \\[2.00000001\\] are not integer multiples of 1"
)

# * Allows differences to be within 'tolerance' of an integer multiple of increment
# * Doesn't replace existing values
x <- c(1, 2, 4 + 1e-8)
result <- fill_seq(x, tolerance = 1e-8)
expect_equal(result, c(1, 2, 3, 4 + 1e-8), tolerance = 1e-9)

# Tolerance works with decreasing sequence
x <- c(1, 2, 3 + 1e-8, 5)
result <- fill_seq(x, tolerance = 1e-8, decreasing = TRUE)
expect_equal(result, c(5, 4, 3 + 1e-8, 2, 1), tolerance = 1e-9)

# Stops with non-numeric input
x <- c("a", "b", "d")
expect_error(fill_seq(x), "is.numeric\\(x\\) is not TRUE")

# Stops if input length < 3
x <- c(1, 2)
expect_error(fill_seq(x), "length\\(x\\) < 3 after deduplicating")

# Stops if input length < 3 after deduplicating
x <- c(1, 2, 2 + 1e-10)
expect_error(
  suppressWarnings(fill_seq(x)),
  "length\\(x\\) < 3 after removing values that differ by <"
)

# Stops if increment is too small
x <- c(1e-6, 2e-6, 4e-6)
expect_error(
  fill_seq(x, tolerance = 1e-5), "Max difference \\[2e-06\\] <= tolerance \\[1e-05\\]"
)
