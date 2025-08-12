#' Extract Points Within an Ellipsoid
#'
#' Filters a set of points, returning only those that fall within the boundary
#' of a given ellipsoid. The determination is made using the squared
#' Mahalanobis distance.
#'
#' @param ell An `ellipsoid` object created by `build_ellipsoid()`.
#' @param points A matrix or data frame of points to test. The column order
#'   must match the order of variables in the ellipsoid's center.
#' @return A matrix containing the rows of `points` that are inside the ellipsoid.
#' @family ellipsoid functions
#' @export
points_in_ellipsoid <- function(ell, points) {
  pts <- as.matrix(points)

  # Calculate squared Mahalanobis distance: d^2 = (x - μ)' Σ⁻¹ (x - μ)
  diffs <- sweep(pts, 2, ell$center, "-")
  m_sq_dist <- rowSums((diffs %*% ell$Sigma_inv) * diffs)

  # A point is inside if its squared distance is <= 1
  is_inside <- m_sq_dist <= 1

  return(pts[is_inside, , drop = FALSE])
}
