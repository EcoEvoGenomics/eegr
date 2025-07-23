#' Calculate convex hulls of grouped data in scatter plots
#'
#' @description This function takes a data frame and calculates the
#' convex hull (the outline) of the data in two dimensions (i.e. data
#' frame columns). Alternatively, you may provide a column `group_by`
#' to calculate the outline of each group individually. The functions
#' outputs a dataframe formatted specifically for use with `geom_polygon`
#' in the `ggplot2` package.
#'
#' @param df Data frame, containing at least two (continuous) dimensions
#' @param xdim Character singleton, name of x dim in which to calculate hulls
#' @param ydim Character singleton, name of y dim in which to calculate hulls
#' @param group_by Character singleton, name of grouping dimension
#'
#' @importFrom plyr ddply
#' @importFrom grDevices chull
#'
#' @export
#'
calculate_convex_hulls <- function(df, xdim, ydim, group_by = NULL) {
  dimnames_are_valid <- all(c(xdim, ydim) %in% colnames(df))
  dimnames_are_different <- xdim != ydim
  grouping_is_valid <- is.null(group_by) | group_by %in% colnames(df)
  grouping_is_not_dimname <- ! group_by %in% c(xdim, ydim)
  stopifnot(dimnames_are_valid & dimnames_are_different)
  stopifnot(grouping_is_valid & grouping_is_not_dimname)

  if (!is.null(group_by)) {
    hulls <- plyr::ddply(df, group_by, calculate_convex_hulls, xdim, ydim)
    return(hulls)
  }
  hull <- df[grDevices::chull(df[[xdim]], df[[ydim]]), ]
  return(hull)
}
