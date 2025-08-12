# Load required packages
library(plotly)

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
    θ <- angles[1]
    R <- matrix(c(cos(θ), -sin(θ),
                  sin(θ),  cos(θ)), 2, 2)
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
  Σinv <- ell$Sigma_inv
  
  # subtract across columns
  diffs <- sweep(pts, 2, cen, "-")
  m2 <- rowSums((diffs %*% Σinv) * diffs)
  
  inside <- m2 <= 1

  return(pts[inside, ,drop = FALSE])
  
}



# Examples in 2D and 3D ---------------------------------------------------
# 
# 
# # 2D example of ellipsoid creation and point extraction
# 
# # 1. Load libraries
# library(ggplot2)
# 
# # 2. Simulate a cloud of 2D points
# set.seed(123)
# 
# df2 <- data.frame(
#   x = rnorm(2000, mean = 0, sd = 2.5),
#   y = rnorm(2000, mean = 1, sd = 1.5)
# )
# 
# # 3. Define and build a 2D "ellipsoid" (ellipse) object
# ell2d <- get_parametric_ellipsoid(
#   center   = c(0, 1),       # center at (0,1)
#   axes     = c(3, 1.5),     # semi‑major = 3 in x, semi‑minor = 1.5 in y
#   angles   = pi/6,          # rotate by 30°
#   n_points = 200            # resolution of the outline
# )
# 
# # 4. Extract only the points inside that ellipse
# #    Note: extract_points_in_ellipsoid assumes points matrix columns
# #    correspond to center order (x, y)
# pts_in2 <- extract_points_in_ellipsoid(
#   ell = ell2d,
#   points    = df2
# )
# 
# pts_in2 <- as.data.frame(pts_in2)
# names(pts_in2) <- c("x","y")
# 
# # 5. Plot with ggplot2
# ggplot() +
#   # all points in light grey
#   geom_point(data = df2, aes(x = x, y = y),
#              color = "grey80", size = 1.2) +
#   # points inside ellipse in steelblue
#   geom_point(data = pts_in2, aes(x = x, y = y),
#              color = "steelblue", size = 1.5) +
#   # ellipse outline in red
#   geom_path(data = ell2d$surface, aes(x = x, y = y),
#             color = "red", size = 1) +
#   coord_fixed() +
#   theme_minimal() +
#   labs(
#     title = "2D Ellipse and Points Within",
#     x = "X coordinate",
#     y = "Y coordinate"
#   )
# 
# 
# 
# # 3D Ellipsoid
# library(plotly)
# 
# # 1. Simulate 3D cloud
# set.seed(42)
# df3 <- data.frame(env_1 = rnorm(5000, 100, 80),
#                   env_2 = rnorm(5000, 500, 150),
#                   env_3 = rnorm(5000, 200,  90))
# 
# # 2. Build an ellipsoid object
# ell <- get_parametric_ellipsoid(
#   center   = c(500, 0, 200),
#   axes     = c(150, 100,  80),
#   angles   = c(pi/6, pi/6, pi/4),
#   n_points = 50
# )
# 
# # 3. Extract only the points inside that ellipsoid
# #    (must match column order: env_2, env_1, env_3 → center order)
# pts_in <- extract_points_in_ellipsoid(
#   ell,
#   df3[, c("env_2","env_1","env_3")]
# )
# pts_in <- as.data.frame(pts_in)
# names(pts_in) <- c("env_2","env_1","env_3")
# 
# # 4. Plot: all points in grey, inside‐ellipsoid in green, plus wireframe
# plot_ly() %>%
#   add_markers(data = df3, x=~env_2, y=~env_1, z=~env_3,
#               marker=list(color="lightgrey", size=2),
#               name="All points") %>%
#   add_markers(data = pts_in, x=~env_2, y=~env_1, z=~env_3,
#               marker=list(color="green", size=3),
#               name="Inside ellipsoid") %>%
#   add_trace(data = ell$surface, x=~x, y=~y, z=~z,
#             type="scatter3d", mode="lines",
#             line=list(color="blue"),
#             name="Ellipsoid surface") %>%
#   layout(
#     scene = list(
#       xaxis=list(title="env_2"),
#       yaxis=list(title="env_1"),
#       zaxis=list(title="env_3"),
#       aspectmode="data"
#     ),
#     title="3D Ellipsoid and Points Inside"
#   )
