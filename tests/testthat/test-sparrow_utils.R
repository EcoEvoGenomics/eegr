test_that("get_sparrow_colours raises no error for valid inputs", {
  valid_species <- c("PDOM", "PITA", "PMON", "PHIS")
  valid_accents <- rep(0, 4)
  expect_no_error(get_sparrow_colours(valid_species, valid_accents))
})

test_that("get_sparrow_colours raises no warning for valid inputs", {
  valid_species <- c("PDOM", "PITA", "PMON", "PHIS")
  valid_accents <- rep(0, 4)
  expect_no_warning(get_sparrow_colours(valid_species, valid_accents))
})

test_that("get_sparrow_colours raises no message for valid inputs", {
  valid_species <- c("PDOM", "PITA", "PMON", "PHIS")
  valid_accents <- rep(0, 4)
  expect_no_message(get_sparrow_colours(valid_species, valid_accents))
})

test_that("get_sparrow_colours raises error for invalid species", {
  invalid_species <- c("foo", "bar")
  valid_accents <- c(0, 0)
  expect_error(get_sparrow_colours(invalid_species, valid_accents))
})

test_that("get_sparrow_colours raises error for invalid accent values", {
  valid_species <- c("PDOM", "PITA", "PMON", "PHIS")
  too_low_accents <- rep(-1.01, 4)
  too_high_accents <- rep(1.01, 4)
  expect_error(get_sparrow_colours(valid_species, too_low_accents))
  expect_error(get_sparrow_colours(valid_species, too_high_accents))
})

test_that("get_sparrow_colours errs if mismatched accents and species", {
  valid_species <- c("PDOM", "PITA", "PMON", "PHIS")
  too_few_accents <- rep(-0.01, 3)
  expect_error(get_sparrow_colours(valid_species, too_few_accents))
})
