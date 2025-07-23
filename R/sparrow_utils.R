#' Get conventional sparrow species colours
#'
#' @description This function returns appropriate colour values
#' for plotting Passer sparrow data. You pass in a vector of species
#' codes ("PDOM", "PMON", "PHIS", and "PITA" are currently supported),
#' and the function returns a vector of hexadecimals. You can
#' include each species code an arbitrary amount of times to get colours
#' for many individuals; the length of the output will equal that of your
#' input vector. You must also provide input "accents": a vector of numbers
#' between -1 and 1. This lightens (negative numbers) or darkens (positive
#' numbers) the colour to accent it. This will be applied to each individual
#' colour, not for all colours sharing the same species code.
#'
#' @param species Character vector of species codes ("PDOM", etc.)
#' @param accents Float vector of accent values between -1 and 1. 0 is default.
#'
#' @return A vector of hexadecimal colour codes.
#'
#' @import colorspace
#'
#' @export
#'
get_sparrow_colours <- function(species = c("PDOM"), accents = c(0)) {
  pal <- data.frame(
    species = c("PDOM", "PMON", "PHIS", "PITA"),
    colour = c("#1E90FF", "#008B00", "#8B0000", "#FFD700")
  )

  stopifnot(length(species) == length(accents))
  stopifnot(all(species %in% pal$species))
  stopifnot(all(accents <= 1 & accents >= -1))

  colours <- sapply(species, function(x) pal$colour[match(x, pal$species)])
  for (i in seq_len(length(colours))) {
    colours[i] <- colorspace::darken(colours[i], accents[i])
  }

  return(colours)
}
