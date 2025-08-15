#' Plot a 3D Niche in Environmental Space
#'
#' @description
#' Creates an interactive 3D plot showing a species' ellipsoidal niche
#' relative to the available background environments. This version samples a
#' specific number of points from the suitable and background groups separately.
#'
#' @param env_rasters A `SpatRaster` object from `terra` with 3 layers
#'   representing the environmental variables.
#' @param niche An `ellipsoid` object created by `build_ellipsoid()`.
#' @param n_bg An integer specifying the number of background (unsuitable) points
#'   to sample for the plot.
#' @param n_suitable An integer specifying the number of suitable points to sample.
#' @param colors A character vector of three colors for the background points,
#'   suitable points, and niche boundary, respectively.
#' @param title A character string for the plot's main title.
#'
#' @return An interactive `plotly` object.
#' @family ellipsoid functions
#'
#' @importFrom stats as.formula
#' @export
plot_niche_3d <- function(env_rasters,
                          niche,
                          n_bg = 50000,
                          n_suitable = 10000,
                          colors = c("grey", "darkgreen", "blue"),
                          title = "Species Niche in 3D Environmental Space") {

  # --- 1. Input Validation ---
  if (!inherits(env_rasters, "SpatRaster")) stop("'env_rasters' must be a SpatRaster.")
  if (!inherits(niche, "ellipsoid") || niche$dimen != 3) stop("'niche' must be a 3D ellipsoid object.")
  niche_vars <- names(niche$center)
  if (!all(niche_vars %in% names(env_rasters))) stop("Niche variables do not match raster layer names.")

  # --- 2. Data Preparation ---
  message("Preparing data (this can be slow on large rasters)...")
  env_df <- as.data.frame(env_rasters, na.rm = TRUE)

  # Calculate suitability for all points to separate the groups
  diffs <- sweep(as.matrix(env_df[, niche_vars]), 2, niche$center, "-")
  m_sq_dist <- rowSums((diffs %*% niche$Sigma_inv) * diffs)
  env_df$suitability <- as.integer(m_sq_dist <= 1)

  # Separate suitable and unsuitable points
  suitable_points_all <- env_df[env_df$suitability == 1, ]
  unsuitable_points_all <- env_df[env_df$suitability == 0, ]

  # Safely sample the requested number from each group
  n_bg_safe <- min(n_bg, nrow(unsuitable_points_all))
  n_suitable_safe <- min(n_suitable, nrow(suitable_points_all))

  message(paste("Sampling", n_bg_safe, "background and", n_suitable_safe, "suitable points..."))
  bg_sample <- unsuitable_points_all[sample(nrow(unsuitable_points_all), size = n_bg_safe), ]
  suitable_sample <- suitable_points_all[sample(nrow(suitable_points_all), size = n_suitable_safe), ]

  # --- 3. Create the 3D Plot ---
  message("Building plotly graph...")
  var_x <- as.formula(paste0("~", niche_vars[1]))
  var_y <- as.formula(paste0("~", niche_vars[2]))
  var_z <- as.formula(paste0("~", niche_vars[3]))

  p <- plotly::plot_ly(colors = colors) %>%
    plotly::add_markers(data = bg_sample, x = var_x, y = var_y, z = var_z,
                        marker = list(color = colors[1], size = 2, opacity = 0.2),
                        name = "Background Environments") %>%
    plotly::add_markers(data = suitable_sample, x = var_x, y = var_y, z = var_z,
                        marker = list(color = colors[2], size = 2.5),
                        name = "Suitable Environments") %>%
    plotly::add_trace(data = niche$surface, x = ~x, y = ~y, z = ~z,
                      type = "scatter3d", mode = "lines",
                      line = list(color = colors[3], width = 1),
                      name = "Niche Boundary") %>%
    plotly::layout(
      title = title,
      scene = list(
        xaxis = list(title = niche_vars[1]),
        yaxis = list(title = niche_vars[2]),
        zaxis = list(title = niche_vars[3])
      )
    )

  message("Done.")
  return(p)
}
