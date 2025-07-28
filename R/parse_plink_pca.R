#' Instantiate new PLINK PCA parser
#'
#' @param eigenval_path Path to PLINK output with .eigenval extension
#' @param eigenvec_path Path to PLINK output with .eigenvec extension
#' @param popmap_path Path to .tsv with headerless columns for ID and pop
#'
#' @returns An object of class plink_pca_parser
#'
#' @importFrom R6 R6Class
#' @importFrom readr read_table
#' @importFrom dplyr as_tibble
#'
#' @export
parse_plink_pca <- function(eigenval_path, eigenvec_path, popmap_path) {
  plink_pca_parser$new(eigenval_path, eigenvec_path, popmap_path)
}

#' Class for PLINK PCA parsers
#'
#' @description
#' An object of class plink_pca_parser helps you easily parse
#' results from a PLINK PCA.
plink_pca_parser <- R6::R6Class(
  classname = "PLINK PCA Parser",
  inherit = popgen_parser,
  public = list(

    # Initialize by importing data from eigenvec and eigencval files
    #
    #' @param eigenval_path Path to PLINK output with .eigenval extension
    #' @param eigenvec_path Path to PLINK output with .eigenvec extension
    #' @param popmap_path Path to .tsv with headerless columns for ID and pop
    #'
    initialize = function(eigenval_path, eigenvec_path, popmap_path) {
      private$eigenvalues <- private$load_eigenvalues(eigenval_path)
      private$eigenvectors <- private$load_eigenvectors(eigenvec_path) |>
        private$parse_eigenvectors()
      private$samples <- private$eigenvectors[[1]]
      private$popmap <- super$load_popmap(popmap_path)
    },

    # Get variance explained by each principal component
    #
    #' @param pc Vector of which principal components to return for
    #' @param signif Number of significant figures
    #' @param as_percent Logical. Return as percent (default) or fraction?
    #'
    get_variance_explained = function(pc, signif = 3, as_percent = TRUE) {
      variance_explained <- private$eigenvalues / sum(private$eigenvalues)
      variance_explained <- signif(variance_explained, signif)
      if (as_percent) variance_explained <- variance_explained * 100
      return(variance_explained[pc])
    },

    # Get coordinates of samples on the principal components
    #
    #' @param pc Vector of which principal components to return
    #' @param include_pops Include sample population in output table
    #'
    get_coordinates = function(pc = c(1, 2), include_pops = TRUE) {
      id_index <- 1
      pc_indices <- pc + id_index
      coordinates <- private$eigenvectors[c(id_index, pc_indices)]
      if (! include_pops) return(coordinates)
      coordinates <- cbind("POP" = self$sample_pops, coordinates) |>
        dplyr::as_tibble()
    }
  ),
  private = list(
    eigenvalues = NULL,
    eigenvectors = NULL,

    # Load and parse a PLINK .eigenval file
    #
    # @param file_path Path to PLINK output with .eigenval extension
    #
    # @returns A vector of eigenvalues for each principal component
    #
    load_eigenvalues = function(eigenval_file_path) {
      scan(eigenval_file_path, quiet = TRUE)
    },

    # Load a PLINK .eigenvec file
    #
    # @param file_path Path to PLINK output with .eigenvec extension
    #
    # @returns A tibble containing sample IDs and eigenvector values
    #
    load_eigenvectors = function(eigenvec_file_path) {
      eigenvec_file_path |>
        readr::read_table(col_names = FALSE, show_col_types = FALSE) |>
        dplyr::as_tibble()
    },

    # Parse a PLINK .eigenvec file
    #
    # @param eigenvectors A tibble of sample and family IDs and principal
    # component coordinates
    #
    # @returns The input tibble, parsed. The family ID column is removed,
    # the sample ID column renamed "ID", and the principal component coordinate
    # columns renamed as "PC1", "PC2", etc.
    #
    parse_eigenvectors = function(eigenvectors) {
      id_index <- 1
      family_id_index <- 2

      eigenvectors <- eigenvectors[-family_id_index]
      pc_count <- ncol(eigenvectors[-id_index])
      pc_columns <- seq(from = id_index, to = pc_count) + 1
      pc_names <- paste("PC", seq(pc_count), sep = "")
      names(eigenvectors)[id_index] <- "ID"
      names(eigenvectors)[pc_columns] <- pc_names

      return(eigenvectors)
    }
  )
)
