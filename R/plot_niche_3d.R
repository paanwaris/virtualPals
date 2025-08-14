#' Plot a 3D Niche in Environmental Space
#'
#' Creates an interactive 3D plot showing a species' ellipsoidal niche
#' relative to the available background environments.
#'
#' @param env_data A data frame containing columns for the environmental
#'   variables and a `suitability` column (1 for suitable, 0 for unsuitable).
#' @param niche An `ellipsoid` object created by `build_ellipsoid()`. It must
#'   be a 3D niche.
#' @param n_bg An integer specifying the number of background (unsuitable) points
#'   to sample for the plot.
#' @param n_suitable An integer specifying the number of suitable points to sample.
#' @param title A character string for the plot's main title.
#'
#' @return An interactive `plotly` object.
#' @family ellipsoid functions
#'
#' @importFrom stats as.formula
#' @export
plot_niche_3d <- function(env_data,
                          niche,
                          n_bg = 50000,
                          n_suitable = 10000,
                          title = "Ellipsoid Niche in 3D Environmental Space") {

  # --- 1. Input Validation ---
  if (!is.data.frame(env_data) || !("suitability" %in% names(env_data))) {
    stop("'env_data' must be a data frame with a 'suitability' column.")
  }
  if (!inherits(niche, "ellipsoid") || niche$dimen != 3) {
    stop("'niche' must be a 3D ellipsoid object from build_ellipsoid().")
  }
  niche_vars <- names(niche$center)
  if (!all(niche_vars %in% names(env_data))) {
    stop("Niche variable names not found in the env_data data frame.")
  }

  # --- 2. Data Preparation and Subsampling ---
  message("Preparing data for plotting...")
  set.seed(123) # for reproducibility

  suitable_points_all <- env_data[env_data$suitability == 1, ]
  unsuitable_points_all <- env_data[env_data$suitability == 0, ]

  n_bg_safe <- min(n_bg, nrow(unsuitable_points_all))
  n_suitable_safe <- min(n_suitable, nrow(suitable_points_all))

  unsuitable_points_sample <- unsuitable_points_all[sample(nrow(unsuitable_points_all), size = n_bg_safe), ]
  suitable_points_sample <- suitable_points_all[sample(nrow(suitable_points_all), size = n_suitable_safe), ]

  # --- 3. Create the 3D Plot ---
  message("Building plotly graph...")

  # Dynamically get variable names for axes
  var_x <- as.formula(paste0("~", niche_vars[1]))
  var_y <- as.formula(paste0("~", niche_vars[2]))
  var_z <- as.formula(paste0("~", niche_vars[3]))

  p_inside_3d <- plotly::plot_ly(data = unsuitable_points_sample,
                                 x = var_x, y = var_y, z = var_z,
                                 type = 'scatter3d', mode = 'markers',
                                 marker = list(color = "lightgrey", size = 2),
                                 name = "Background Environment") %>%
    plotly::add_trace(data = suitable_points_sample,
                      x = var_x, y = var_y, z = var_z,
                      type = 'scatter3d', mode = 'markers',
                      marker = list(color = "darkgreen", size = 2.5),
                      name = "Points in Niche") %>%
    plotly::add_trace(data = niche$surface, x = ~x, y = ~y, z = ~z,
                      type = "mesh3d",
                      opacity = 0.15,
                      color = "blue",
                      name = "Niche Surface", inherit = FALSE) %>%
    plotly::add_markers(x = niche$center[1], y = niche$center[2], z = niche$center[3],
                        marker = list(color = 'red', size = 5, symbol = "diamond"),
                        name = "Niche Center", inherit = FALSE) %>%
    plotly::layout(
      title = title,
      scene = list(
        xaxis = list(title = niche_vars[1]),
        yaxis = list(title = niche_vars[2]),
        zaxis = list(title = niche_vars[3]),
        aspectmode = "data"
      ),
      legend = list(x = 0.05, y = 0.95, title = list(text = '<b>Legend</b>'))
    )

  message("Done.")
  return(p_inside_3d)
}
