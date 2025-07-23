actual_k_values <- c(1, 2, 3)
actual_cv_error <- c(0.3, 0.25, 0.225)
actual_k_min_error <- 3
actual_n_samples <- 5

test_that("Can instantiate new admixture_parser without error", {
  expect_no_error({
    inst <- parse_admixture("../testdata/parse_admixture", paste("ID", seq(5)))
  })
})

test_that("Can instantiate new admixture_parser without warning", {
  expect_no_warning({
    inst <- parse_admixture("../testdata/parse_admixture", paste("ID", seq(5)))
  })
})

test_that("Can instantiate new admixture_parser without message", {
  expect_no_message({
    inst <- parse_admixture("../testdata/parse_admixture", paste("ID", seq(5)))
  })
})

test_that("K values and CV errors are parsed correctly", {
  inst <- parse_admixture("../testdata/parse_admixture", paste("ID", seq(5)))
  expect_equal(inst$k_values, actual_k_values)
  expect_equal(inst$cv_errors, actual_cv_error)
  expect_equal(inst$k_min_error, actual_k_min_error)
})

test_that("Assignments are parsed correctly", {
  inst <- parse_admixture("../testdata/parse_admixture", paste("ID", seq(5)))

  for (k in actual_k_values) {
    assignments <- inst$get_assignments(k)
    ids_are_strings <- typeof(assignments[[1]]) == "character"
    dims_are_correct <- all(dim(assignments) == c(actual_n_samples, k + 1))
    assignments_are_floats <- all(unlist(
      lapply(assignments[-1], function(v) typeof(unlist(v)) == "double")
    ))

    if (ids_are_strings & assignments_are_floats & dims_are_correct) next

    fail()
  }

  succeed()
})

test_that("Function returns identical output as direct instantiation", {
  x <- parse_admixture("../testdata/parse_admixture", paste("ID", seq(5)))
  y <- admixture_parser$new("../testdata/parse_admixture", paste("ID", seq(5)))
  expect_equal(x, y)
})
