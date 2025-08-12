get_sample_occurrences <- function(ellipsoid,
                                 raster_df,
                                 pred_cols,
                                 n,
                                 seed = NULL,
                                 sigma_scale = 1) {
  
  if (!is.null(seed)) set.seed(seed)
  # build the env‐space matrix
  env_mat <- as.matrix(raster_df[, pred_cols])
  
  # get mean and (optionally scaled) covariance
  mu    <- ellipsoid$center
  Sigma <- ellipsoid$Sigma * (sigma_scale^2)
  
  # 1) draw from MVN(μ, Σ)
  env_samp <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
  
  # 2) snap each sample to its nearest real cell in env‐space
  nn_idx <- FNN::get.knnx(data = env_mat, query = env_samp, k = 1)$nn.index[,1]

  # 3) NEEDS: Just need to add some constraint about being inside the ellipsoid
  # here
  
  # 4) return those rows (with replacement)
  raster_df[nn_idx, , drop = FALSE]
  
}



# Example -----------------------------------------------------------------
# 
# library(plotly)
# 
# # 1. Simulate a “raster” of 3D env‑space cells
# set.seed(101)
# 
# raster_df <- data.frame(
#   lon  = runif(1000, -100, 100),
#   lat  = runif(1000, -50,  50),
#   env1 = rnorm(1000, 0, 3),
#   env2 = rnorm(1000, 5, 2),
#   env3 = rnorm(1000, -2, 1)
# )
# 
# 
# # 2. Build a 3D ellipsoid in env‑space
# ell3d <- get_parametric_ellipsoid(
#   center   = c(0, 5, -2),
#   axes     = c(4, 3, 2),
#   angles   = c(pi/8, pi/5, pi/7),
#   n_points = 50
# )
# 
# 
# plot_ex <- plot_ly(raster_df, x = ~env1, y = ~env2, z = ~env3,
#         type = 'scatter3d', mode = 'markers',
#         marker = list(color = "lightgrey", size = 2)) %>% 
#   add_trace(data = ell3d$surface, x=~x, y=~y, z=~z,
#             type="scatter3d", mode="lines",
#             line=list(color="blue"),
#             name="Ellipsoid surface", inherit = FALSE) %>%
#   add_markers(x = ell3d$center[1], y = ell3d$center[2], z = ell3d$center[3],
#               marker = list(color = 'red', size = 5),
#               name = "Center") %>%
#   layout(
#     title = "Ellipsoid in 3D with Axes and Center",
#     scene = list(
#       xaxis = list(title = "X"),
#       yaxis = list(title = "Y"),
#       zaxis = list(title = "Z")
#     ),
#     legend = list(x = 0.05, y = 0.95)
#   )
# 
# plot_ex
# 
# # 3. Extract only cells inside that ellipsoid
# inside_pts <- extract_points_in_ellipsoid(
#   ell = ell3d,
#   points    = raster_df[, c("env1","env2","env3")]
# )
# 
# # Add point inside the ellipsoid
# plot_ex %>% 
#   add_markers(data = inside_pts, x=~env1, y=~env2, z=~env3,
#               marker=list(color="green", size = 3),
#               name="Inside ellipsoid", inherit = FALSE)
# 
# inside_df <- dplyr::left_join(as.data.frame(inside_pts), raster_df)
# 
# # 4. Sample 500 points weighted toward the centroid
# samples3d <- sample_gaussian_snap(
#   ellipsoid   = ell3d,
#   raster_df   = raster_df,
#   pred_cols   = c("env1","env2","env3"),
#   n           = 1000,
#   seed        = 101,
#   sigma_scale = 0.5    # half‐variance to sharpen
# )
# 
# # Add point inside the ellipsoid
# plot_ex %>% 
#   add_markers(data = inside_df, x=~env1, y=~env2, z=~env3,
#               marker=list(color="green", size = 3),
#               name="Inside ellipsoid", inherit = FALSE) %>% 
#   add_markers(data = samples3d, x=~env1, y=~env2, z=~env3,
#               marker=list(color="orange", size = 3),
#               name="sampled points", inherit = FALSE)
# 
# 
# 
# # 5. Quick 2D check in lat/lon
# ggplot() +
#   geom_point(data = inside_df, aes(x = lon, y = lat),
#              color = "green", size = 0.8) +
#   geom_point(data = samples3d, aes(x = lon, y = lat),
#              color = "orange",   size = 1.5, alpha = 0.7) +
#   coord_quickmap() +
#   theme_minimal() +
#   labs(title = "Weighted Sampling of Cells Inside 3D Ellipsoid",
#        subtitle = "More points drawn closer to env‑space centroid")
# 
