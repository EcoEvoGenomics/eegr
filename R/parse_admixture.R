#' Instantiate new ADMIXTURE parser
#'
#' @param admixture_dir Character singleton. Directory of Admixture output.
#' @param sample_ids Character vector. The IDs of samples input to Admixture.
#' @param popmap_path Path to .tsv with headerless columns for ID and pop
#'
#' @returns An object of class admixture_parser
#'
#' @importFrom R6 R6Class
#' @importFrom stringr str_locate
#' @importFrom stringr str_sub
#' @import tidyverse
#'
#' @export
#'
parse_admixture <- function(admixture_dir, sample_ids, popmap_path) {
  admixture_parser$new(admixture_dir, sample_ids, popmap_path)
}

#' Class for ADMIXTURE parsers
#'
#' @description
#' An object of class admixture_parser helps you easily parse
#' results from an ADMIXTURE analysis.
#'
#' @field k_min_error Singleton vector holding K of least cross-validation error
#' @field k_values Vector of K-values
#' @field cv_errors Vector of cross-validation errors
#'
admixture_parser <- R6::R6Class(
  classname = "Admixture Parser",
  inherit = popgen_parser,
  public = list(

    # Initialize by taking information about ADMIXTURE run
    #
    #' @param admixture_dir Character singleton. Directory of Admixture output.
    #' @param sample_ids Character vector. IDs of samples input to Admixture.
    #' @param popmap_path Path to .tsv with headerless columns for ID and pop
    #'
    initialize = function(admixture_dir, sample_ids, popmap_path) {
      private$directory <- admixture_dir
      private$samples <- sample_ids
      private$popmap <- super$load_popmap(popmap_path)
    },

    # Get membership assignments at a value of K
    #
    #' @param k Integer singleton. Value of K.
    #' @returns A dplyr tibble.
    #'
    get_assignments = function(k = self$k_min_error) {
      private$parse_qfile(private$get_admixture_files(k = k)$qfiles)
    },

    # Plot bar plot
    #
    #' @param k Integer singleton. Value of K.
    #' @param sort_samples Logical. Sort samples by V1? Default TRUE.
    #' @param cluster_cols Vector of colours for the assigned clusters.
    #' @returns A ggplot plot object.
    #'
    plot = function(k = self$k_min_error, sort_samples = TRUE, cluster_cols) {

      stopifnot(length(cluster_cols) == k)

      assig <- self$get_assignments(k)
      assig$POP <- self$sample_pops

      if (sort_samples) {
        assig <- assig |>
          arrange(V1) |>
          mutate(ID = factor(ID, levels = as.character(ID)))
      }

      mix_plot <- assig |>
        pivot_longer(cols = contains("V")) |>
        ggplot(aes(x = ID, y = value, fill = name)) +
        coord_cartesian(expand = FALSE, clip = "off") +
        geom_col(width = 1, show.legend = FALSE) +
        facet_wrap(
          vars(POP),
          space = "free_x",
          scale  = "free_x",
          strip.position = "bottom"
        ) +
        scale_fill_manual(values = cluster_cols) +
        theme_void() +
        theme(
          strip.text = element_text(
            size = 4, colour = "grey25",
            margin = margin(1, 0, 1, 0, unit = "mm")
          ),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), units = "mm")
        )
    },

    # Plot CV errors across K
    #
    #' @returns A ggplot plot object.
    #'
    plot_cv_error = function() {
      errors <- data.frame(ERR = self$cv_errors, K = self$k_values)
      errors |>
        ggplot(aes(x = K, y = ERR)) +
        coord_cartesian(expand = FALSE, clip = "off") +
        geom_line(colour = "grey25", lineend = "round") +
        annotate(
          "text", label = "CV Error",
          x = max(errors$K), y = errors$ERR[which(errors$K == max(errors$K))],
          hjust = -0.1, vjust = 0.5,
          colour = "grey25", size = 1.5
        ) +
        xlab("Value of K") +
        theme(
          axis.text = element_text(size = 4, colour = "grey75"),
          axis.ticks = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 4, colour = "grey75"),
          panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(
            colour = "grey90",
            linewidth = 0.15
          ),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), units = "mm")
        )
    }
  ),
  active = list(
    k_min_error = function() {
      self$k_values[which.min(self$cv_errors)]
    },
    k_values = function() {
      vapply(private$parse_outfiles(), function(x) x$k, integer(1))
    },
    cv_errors = function() {
      vapply(private$parse_outfiles(), function(x) x$cv_error, double(1))
    }
  ),
  private = list(
    directory = NULL,

    # Identify and list Admixture output files in private$directory
    #
    # @param k Integer singleton. Get files for K = k, or for all K if k = NULL.
    #
    # @returns List of two character vectors: outfiles holds paths to Admixture
    # .out files and qfiles paths to Admixture .Q-files. If k is specified,
    # outfiles and qfiles will be character singletons. If k is NULL, the two
    # vectors will hold the paths to output for all K in the Admixture output.
    #
    get_admixture_files = function(k = NULL) {
      search_all_k <- is.null(k)
      if (search_all_k) {
        k <- "*"
      }

      outsuffix <- paste(k, "\\.out", sep = "")
      qsuffix <- paste(k, "\\.Q", sep = "")
      outfiles <- dir(private$directory, pattern = outsuffix, full.names = TRUE)
      qfiles <- dir(private$directory, pattern = qsuffix, full.names = TRUE)

      list(outfiles = outfiles, qfiles = qfiles)
    },

    # Easily parse all outfiles to obtain all K-values and CV errors
    #
    # @returns A list, each item corresponds to an outfile. Each outfile item
    # is a vector with named values for K and CV error.
    #
    parse_outfiles = function() {
      all_outfiles <- private$get_admixture_files()$outfiles
      lapply(all_outfiles, private$parse_outfile)
    },

    # Parse an Admixture .out-file to read K and CV error
    #
    # @description In Admixture .out-files, K and CV error are provided in-text
    # on the second last line. That line is formatted like e.g. "CV error (K=3):
    # 0.28344". In other words, the value of K is obtained between "=" and ")",
    # whereas the CV error is obtained from the fourth character after ")" until
    # the end of the line.
    #
    # @param outfile Character singleton. Path to an Admixture .out-file
    #
    # @returns List of integer singleton (K) and float singleton (CV error)
    #
    parse_outfile = function(outfile) {
      outfile_lines <- readLines(outfile)
      second_last_line <- outfile_lines[[length(outfile_lines) - 1]]
      k_starts <- (stringr::str_locate(second_last_line, "\\=") + 1)[2]
      k_ends <- (stringr::str_locate(second_last_line, "\\)") - 1)[2]
      cv_starts <- k_ends + 4

      k <- as.integer(stringr::str_sub(second_last_line, k_starts, k_ends))
      cv_error <- as.numeric(stringr::str_sub(second_last_line, cv_starts))

      list(k = k, cv_error = cv_error)
    },

    # Parse an Admixture .Q-file to read cluster assignment table
    #
    # @description Admixture outputs cluster assignment tables in .Q-files.
    # Each row represents a sample (in the same order as in the PLINK data
    # set used as input), and each column a cluster. When read, columns are
    # named V1, V2, V3 ... etc. The value in each cell is the cluster assignment
    # probability of the individual (row) in that cluster (column).
    #
    # @param qfile Character singleton. Path to an Admixture .Q-file.
    #
    # @returns A dplyr tibble. The first column holds the sample IDs provided as
    # the object is instantiated with self$initialize(). The remaining K columns
    # holds assignment probabilities.
    #
    parse_qfile = function(qfile) {
      assignments <- read.table(qfile)
      identified_assignments <- cbind(private$samples, assignments)
      colnames(identified_assignments)[1] <- "ID"
      dplyr::as_tibble(identified_assignments)
    }
  )
)
