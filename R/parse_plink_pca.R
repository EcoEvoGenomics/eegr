#' Instantiate new PLINK PCA parser
#'
#' @param eigenval_path Path to PLINK output with .eigenval extension
#' @param eigenvec_path Path to PLINK output with .eigenvec extension
#' @param popmap_path Path to .tsv with headerless columns for ID and pop
#'
#' @returns An object of class plink_pca_parser
#'
#' @importFrom R6 R6Class
#' @importFrom ggrepel geom_text_repel
#' @import tidyverse
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
    },

    # Plot coordinates of samples on the principal components
    #
    #' @param pc Vector of which principal components to plot
    #' @param label_pops Logical. Label populations on plot? Default TRUE.
    #' @param pop_cols Vector of colours for each pop.
    #' @param label_seed Random seed for automatic placement of pop labels.
    #' @returns A ggplot object.
    #'
    plot = function(pc = c(1, 2), label_pops = TRUE, pop_cols, label_seed = 1) {

      pcx <- paste("PC", pc[1], sep = "")
      pcy <- paste("PC", pc[2], sep = "")
      pcx_variance <- self$get_variance_explained(pc[1])
      pcy_variance <- self$get_variance_explained(pc[2])

      xmax <- ceiling(max(self$get_coordinates(pc[1])[3]) * 10) / 10
      xmin <-   floor(min(self$get_coordinates(pc[1])[3]) * 10) / 10
      ymax <- ceiling(max(self$get_coordinates(pc[2])[3]) * 10) / 10
      ymin <-   floor(min(self$get_coordinates(pc[2])[3]) * 10) / 10

      pca_plot <- self$get_coordinates(pc = pc) |>
        ggplot(aes(x = .data[[pcx]], y = .data[[pcy]], col = POP)) +
        coord_equal(
          xlim = c(xmin, xmax), ylim = c(ymin, ymax),
          expand = FALSE, clip = FALSE
        ) +
        geom_hline(yintercept = 0, colour = "grey90", linewidth = 0.15) +
        geom_vline(xintercept = 0, colour = "grey90", linewidth = 0.15) +
        annotate(
          "text", label = paste0(pcx, " (", pcx_variance, "%)"),
          x = xmax, y = (ymax - ymin) / 50,
          hjust = 1, vjust = 0,
          colour = "grey75", size = 2
        ) +
        annotate(
          "text", label = xmax,
          x = xmax, y = (ymax - ymin) / -50,
          hjust = 1, vjust = 1,
          colour = "grey75", size = 2
        ) +
        annotate(
          "text", label = xmin,
          x = xmin, y = (ymax - ymin) / -50,
          hjust = 0, vjust = 1,
          colour = "grey75", size = 2
        ) +
        annotate(
          "text", label = paste0(pcy, " (", pcy_variance, "%)"),
          x = (xmax - xmin) / 50, y = ymax,
          hjust = 0, vjust = 1,
          colour = "grey75", size = 2
        ) +
        annotate(
          "text", label = ymax,
          x = (xmax - xmin) / -50, y = ymax,
          hjust = 1, vjust = 1,
          colour = "grey75", size = 2
        ) +
        annotate(
          "text", label = ymin,
          x = (xmax - xmin) / -50, y = ymin,
          hjust = 1, vjust = 0,
          colour = "grey75", size = 2
        ) +
        geom_point(size = 1) +
        scale_colour_manual(values = pop_cols)

      if (label_pops) {
        pca_centroids <- self$get_coordinates(pc = pc) |>
          calculate_convex_hulls(xdim = pcx, ydim = pcy, group_by = "POP") |>
          group_by(POP) |>
          summarise(x = mean(.data[[pcx]]), y = mean(.data[[pcy]]))

        pca_plot <- pca_plot +
          geom_text_repel(
            data = pca_centroids,
            mapping = aes(x = x, y = y, label = POP),
            size = 2,
            force_pull = 25,
            colour = "grey25",
            seed = label_seed,
            segment.size = 0.2,
            min.segment.length = 1.5,
            inherit.aes = FALSE
          )
      }

      pca_plot <- pca_plot +
        theme_bw() +
        theme(
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = "none",
          panel.grid = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank()
        )

      return(pca_plot)
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
