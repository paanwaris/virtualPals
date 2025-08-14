#' Create a parametric 2D or 3D ellipsoid object
#'
#' @param center   Numeric vector of length 2 or 3 giving the ellipsoid center
#' @param axes     Numeric vector of same length giving the semi‐axis lengths
#' @param angles   Rotation angles in radians.
#'                 • 2D: single angle
#'                 • 3D: c(angle_x, angle_y, angle_z), applied in x→y→z order
#' @param n_points Number of subdivisions per parametric circle (for 2D) or grid (for 3D)
#' @return A list of class "ellipsoid" with:
#'   - center, axes, angles, dimen
#'   - R           : the rotation matrix
#'   - Sigma       : the shape (covariance‐like) matrix
#'   - Sigma_inv   : its inverse
#'   - surface     : data.frame of x,y (or x,y,z) for plotting
#' @export
get_parametric_ellipsoid <- function(center   = c(0,0),
                                     axes     = c(1,1),
                                     angles   = c(0,0,0),
                                     n_points = 100) {
  dimen <- length(center)
  stopifnot(dimen %in% c(2,3),
            length(axes)   == dimen,
            length(angles) %in% c(1,dimen))

  # Build rotation matrix R
  if (dimen == 2) {
    theta <- angles[1]
    R <- matrix(c(cos(theta), -sin(theta),
                  sin(theta),  cos(theta)), 2, 2)
  } else {
    ax <- angles[1]; ay <- angles[2]; az <- angles[3]
    Rx <- matrix(c(1,0,0,
                   0,cos(ax),-sin(ax),
                   0,sin(ax), cos(ax)), 3, 3)
    Ry <- matrix(c(cos(ay),0,sin(ay),
                   0,      1,0,
                   -sin(ay),0,cos(ay)), 3, 3)
    Rz <- matrix(c(cos(az),-sin(az),0,
                   sin(az), cos(az),0,
                   0,       0,      1), 3, 3)
    R <- Rz %*% Ry %*% Rx
  }

  # Build shape matrix Σ = R diag(axes^2) Rᵀ
  D       <- diag(axes^2)
  Sigma   <- R %*% D %*% t(R)
  Sigma_inv <- solve(Sigma)

  # Generate surface points
  if (dimen == 2) {
    t      <- seq(0, 2*pi, length.out = n_points)
    circ   <- rbind(axes[1]*cos(t), axes[2]*sin(t))  # 2 x n
    pts2   <- t(R %*% circ)                           # n x 2
    surface <- sweep(pts2, 2, center, "+")
    colnames(surface) <- c("x","y")
    surface <- as.data.frame(surface)

  } else {
    u <- rep(seq(0, 2*pi, length.out = n_points), each = n_points)
    v <- rep(seq(0,   pi, length.out = n_points), times = n_points)
    x0 <- axes[1]*sin(v)*cos(u)
    y0 <- axes[2]*sin(v)*sin(u)
    z0 <- axes[3]*cos(v)
    P  <- rbind(x0, y0, z0)           # 3 x (n*n)
    rot <- t(R %*% P)                # (n*n) x 3
    surf <- sweep(rot, 2, center, "+")
    colnames(surf) <- c("x","y","z")
    surface <- as.data.frame(surf)
  }

  structure(
    list(center = center,
         axes = axes,
         angles = angles,
         dimen = dimen,
         R = R,
         Sigma = Sigma,
         Sigma_inv = Sigma_inv,
         surface   = surface),
    class = "ellipsoid"
  )
}


extract_points_in_ellipsoid <- function(ell, points){

  pts <- as.matrix(points)
  cen <- ell$center
  Sigma_inv <- ell$Sigma_inv

  # subtract across columns
  diffs <- sweep(pts, 2, cen, "-")
  m2 <- rowSums((diffs %*% Sigma_inv) * diffs)

  inside <- m2 <= 1

  return(pts[inside, ,drop = FALSE])

}
