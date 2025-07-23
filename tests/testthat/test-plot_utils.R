test_data <- data.frame(
  x = c(1, 2, 3, 2),
  y = c(2, 2, 3, 3),
  g = c("A", "A", "B", "B")
)

test_that("calculate_convex_hulls stops if given invalid arguments", {
  expect_error(calculate_convex_hulls(list(2, 3, 4), "z", "y", "g"))
  expect_error(calculate_convex_hulls(test_data, "z", "y", "g"))
  expect_error(calculate_convex_hulls(test_data, "x", "z", "g"))
  expect_error(calculate_convex_hulls(test_data, "x", "y", "z"))
})

test_that("calculate_convex_hulls does accept group_by xdim or ydim", {
  expect_error(calculate_convex_hulls(test_data, "x", "y", "x"))
  expect_error(calculate_convex_hulls(test_data, "x", "y", "y"))
})

test_that("calculate_convex_hulls does not accept xdim = ydim", {
  expect_error(calculate_convex_hulls(test_data, "x", "x", "g"))
})
