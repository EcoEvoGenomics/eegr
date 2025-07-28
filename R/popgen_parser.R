#' Common superclass for population genetics parsers
#'
#' @field sample_ids Ordered vector of sample IDs
#' @field sample_pops Vector of sample populations, ordered like sample_ids
#'
popgen_parser <- R6::R6Class(
  classname = "POPGEN Parser",
  active = list(
    sample_ids = function() {
      private$samples
    },
    sample_pops = function() {
      pops_unsorted <- private$popmap[[1]]
      sorted_indices <- sapply(self$sample_ids, \(x) match(x, pops_unsorted))
      private$popmap[sorted_indices, 2]
    }
  ),
  private = list(
    samples = NULL,
    popmap = NULL,

    # Load a popmap .tsv file
    #
    # @param file_path Path to a tab-separated file with samples in the
    # the first column and their populations in the second. Can be in
    # arbitrary order compared to the eigenvector file
    #
    # @returns A tibble containing sample IDs and populations
    #
    load_popmap = function(popmap_path) {
      read.table(popmap_path, col.names = c("ID", "Population"))
    }
  )
)
