eigenval <- "../testdata/parse_plink_pca/test.eigenval"
eigenvec <- "../testdata/parse_plink_pca/test.eigenvec"
popmap <- "../testdata/test_popmap.tsv"

test_that("Can instantiate new plink_pca_parser object without error", {
  expect_no_error({
    inst <- parse_plink_pca(eigenval, eigenvec, popmap)
  })
})

test_that("Can instantiate new plink_pca_parser object without warning", {
  expect_no_warning({
    inst <- parse_plink_pca(eigenval, eigenvec, popmap)
  })
})

test_that("Can instantiate new plink_pca_parser object without message", {
  expect_no_message({
    inst <- parse_plink_pca(eigenval, eigenvec, popmap)
  })
})

test_that("Eigenvalues are loaded correctly", {
  inst <- parse_plink_pca(eigenval, eigenvec, popmap)
  priv <- inst$.__enclos_env__$private

  # Expect same properties as in ../testdata/test.eigenval
  expect_vector(priv$eigenvalues, ptype = numeric(), size = 3)
})

test_that("Eigenvectors are loaded correctly", {
  inst <- parse_plink_pca(eigenval, eigenvec, popmap)
  priv <- inst$.__enclos_env__$private

  # Expect properties accord with input file and user input
  expect_equal(
    colnames(priv$eigenvectors),
    c("ID", "PC1", "PC2", "PC3")
  )
  expect_in(
    class(priv$eigenvectors),
    c("tbl_df", "tbl", "data.frame")
  )
  expect_equal(
    ncol(priv$eigenvectors),
    4
  )
  expect_equal(
    nrow(priv$eigenvectors),
    6
  )
})

test_that("Method get_coordinates returns expected output", {
  inst <- parse_plink_pca(eigenval, eigenvec, popmap)
  priv <- inst$.__enclos_env__$private

  expect_equal(
    inst$get_coordinates(pc = c(1), include_pops = FALSE),
    priv$eigenvectors[c("ID", "PC1")]
  )
  expect_equal(
    inst$get_coordinates(pc = c(1, 2), include_pops = FALSE),
    priv$eigenvectors[c("ID", "PC1", "PC2")]
  )
})

test_that("Method get_variance_explained returns expected output", {
  inst <- parse_plink_pca(eigenval, eigenvec, popmap)

  expect_equal(
    inst$get_variance_explained(pc = c(1, 2, 3), as_percent = FALSE),
    c(0.5, 0.3, 0.2)
  )
  expect_equal(
    inst$get_variance_explained(pc = c(1, 2, 3), as_percent = TRUE),
    c(50, 30, 20)
  )
  expect_vector(inst$get_variance_explained(1), ptype = numeric(), size = 1)
})

test_that("Active field sample_ids returns expected output", {
  inst <- parse_plink_pca(eigenval, eigenvec, popmap)
  expected <- paste("Sample", 1 : 6, sep = "")
  expect_equal(inst$sample_ids, expected)
})

test_that("Active field sample_pops returns expected output", {
  inst <- parse_plink_pca(eigenval, eigenvec, popmap)
  expected <- paste("Pop", rep(c("A", "B", "C"), each = 2), sep = "")
  expect_equal(inst$sample_pops, expected)
})

test_that("Function returns identical output as direct instantiation", {
  inst_function <- parse_plink_pca(eigenval, eigenvec, popmap)
  inst_direct <- plink_pca_parser$new(eigenval, eigenvec, popmap)
  expect_equal(inst_function, inst_direct)
})
