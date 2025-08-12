#' Build a 2D or 3D Ellipsoid Objects
#'
#' Creates a structured object representing a rotated ellipsoid. This object
#' contains all necessary matrices for geometric and statistical calculations,
#' as well as surface points for plotting.
#'
#' @param center Numeric vector of length 2 or 3 defining the ellipsoid's center.
#' @param axes Numeric vector with the same length as `center`, defining the
#'   semi-axis lengths *before* rotation.
#' @param angles Numeric vector of rotation angles in radians.
#'   - For 2D: A single angle.
#'   - For 3D: A vector of three angles `c(ax, ay, az)` applied in the
#'     x, then y, then z order (Tait-Bryan angles).
#' @param n_points Integer; the resolution for the plotted surface.
#' @return An object of class `ellipsoid` containing the defining parameters,
#'   rotation matrix (`R`), shape matrix (`Sigma`), its inverse (`Sigma_inv`),
#'   and a data frame of surface points for plotting.
#' @family ellipsoid functions
#' @export
#' @examples
#' # A 2D ellipse rotated by 30 degrees (pi/6)
#' ell2d <- build_ellipsoid(
#'   center = c(0, 1),
#'   axes = c(3, 1.5),
#'   angles = pi/6
#' )
#'
#' # A 3D ellipsoid with rotations on multiple axes
#' ell3d <- build_ellipsoid(
#'   center = c(10, 20, 30),
#'   axes = c(5, 10, 15),
#'   angles = c(pi/4, 0, pi/3)
#' )
build_ellipsoid <- function(center = c(x = 0, y = 0), # Default with names
                            axes = c(1, 1),
                            angles = 0,
                            n_points = 100) {

  # ... (validation and rotation matrix code is the same) ...

  dimen <- length(center)
  # (All validation and rotation matrix code here)
  if (dimen == 2) {
    theta <- angles[1]
    R <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), 2, 2)
  } else {
    ax <- angles[1]; ay <- angles[2]; az <- angles[3]
    Rx <- matrix(c(1, 0, 0, 0, cos(ax), -sin(ax), 0, sin(ax), cos(ax)), 3, 3)
    Ry <- matrix(c(cos(ay), 0, sin(ay), 0, 1, 0, -sin(ay), 0, cos(ay)), 3, 3)
    Rz <- matrix(c(cos(az), -sin(az), 0, sin(az), cos(az), 0, 0, 0, 1), 3, 3)
    R <- Rz %*% Ry %*% Rx
  }

  # --- Shape (Covariance-like) Matrix (Sigma) ---
  D_sq      <- diag(axes^2)
  Sigma     <- R %*% D_sq %*% t(R)
  Sigma_inv <- solve(Sigma)

  # **NEW**: Assign variable names for robust matching later
  var_names <- names(center)
  if (!is.null(var_names)) {
    colnames(Sigma) <- rownames(Sigma) <- var_names
    colnames(Sigma_inv) <- rownames(Sigma_inv) <- var_names
  }

  # ... (surface point generation code is the same) ...
  if (dimen == 2) {
    t <- seq(0, 2 * pi, length.out = n_points)
    unit_ellipse <- rbind(axes[1] * cos(t), axes[2] * sin(t))
    rotated_points <- t(R %*% unit_ellipse)
    surface <- sweep(rotated_points, 2, center, "+")
    surface <- as.data.frame(surface)
    colnames(surface) <- c("x", "y")
  } else {
    grid <- expand.grid(u = seq(0, 2 * pi, length.out = n_points), v = seq(0, pi, length.out = n_points))
    unit_ellipsoid <- rbind(axes[1] * sin(grid$v) * cos(grid$u), axes[2] * sin(grid$v) * sin(grid$u), axes[3] * cos(grid$v))
    rotated_points <- t(R %*% unit_ellipsoid)
    surface <- sweep(rotated_points, 2, center, "+")
    surface <- as.data.frame(surface)
    colnames(surface) <- c("x", "y", "z")
  }

  structure(
    list(
      center = center, axes = axes, angles = angles, dimen = dimen,
      R = R, Sigma = Sigma, Sigma_inv = Sigma_inv, surface = surface
    ),
    class = "ellipsoid"
  )
}
