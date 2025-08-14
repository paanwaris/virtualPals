#' Extract Points Within an Ellipsoid
#'
#' Filters a set of points, returning only those that fall within the boundary
#' of a given ellipsoid, based on the squared Mahalanobis distance.
#'
#' @param ell An `ellipsoid` object created by `build_ellipsoid()`.
#' @param points A matrix or data frame of points to test. The column order
#'   must match the order of variables in the ellipsoid's center vector.
#' @return A matrix containing the rows from `points` that are inside the ellipsoid.
#' @family ellipsoid functions
#' @export
points_in_ellipsoid <- function(ell, points) {

  # --- 1. Input Validation ---
  if (!inherits(ell, "ellipsoid")) {
    stop("'ell' must be an object of class 'ellipsoid' from build_ellipsoid().")
  }
  if (!is.matrix(points) && !is.data.frame(points)) {
    stop("'points' must be a matrix or data frame.")
  }
  if (ncol(points) != ell$dimen) {
    stop("The number of columns in 'points' does not match the ellipsoid's dimensions.")
  }

  # --- 2. Calculate Mahalanobis Distance and Filter ---
  pts <- as.matrix(points)

  diffs <- sweep(pts, 2, ell$center, "-")
  m_sq_dist <- rowSums((diffs %*% ell$Sigma_inv) * diffs)

  is_inside <- m_sq_dist <= 1

  return(pts[is_inside, , drop = FALSE])
}
