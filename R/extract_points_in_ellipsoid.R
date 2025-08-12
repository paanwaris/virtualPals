#' Extract points that fall inside a given ellipsoid
#'
#' @param ell    An "ellipsoid" object from get_parametric_ellipsoid().
#' @param points A numeric matrix of points to test (rows are points, columns are dimensions).
#' @return A matrix containing only the rows (points) that fall inside the ellipsoid.
extract_points_in_ellipsoid <- function(ell, points){
  
  pts <- as.matrix(points)
  cen <- ell$center
  Σinv <- ell$Sigma_inv
  
  # Calculate squared Mahalanobis distance: (x - μ)' Σ⁻¹ (x - μ)
  diffs <- sweep(pts, 2, cen, "-")
  m2 <- rowSums((diffs %*% Σinv) * diffs)
  
  # A point is inside if its squared distance is less than or equal to 1
  inside <- m2 <= 1
  
  return(pts[inside, ,drop = FALSE])
}
