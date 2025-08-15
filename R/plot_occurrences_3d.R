#' Plot Sampled Occurrences in 3D Environmental Space
#'
#' @description
#' Creates an interactive 3D plot showing the sampled occurrence points
#' relative to the available background environments and the niche boundary.
#'
#' @param occurrences A data frame of occurrence points from `sample_from_env()`.
#' @param env_rasters The `SpatRaster` object used to generate the occurrences.
#' @param niche The `ellipsoid` object used to generate the occurrences.
#' @param n_bg The number of background points to sample for visualization.
#' @param ... Additional arguments passed to `plot_ly`.
#'
#' @return An interactive `plotly` object.
#' @family ellipsoid functions
#' @export
plot_occurrences_3d <- function(occurrences, env_rasters, niche, n_bg = 20000, ...) {

  message("Preparing background points for plotting...")
  env_df <- as.data.frame(env_rasters, na.rm = TRUE)
  n_bg_safe <- min(n_bg, nrow(env_df))
  bg_sample <- env_df[sample(nrow(env_df), size = n_bg_safe), ]

  niche_vars <- names(niche$center)
  var_x <- as.formula(paste0("~", niche_vars[1]))
  var_y <- as.formula(paste0("~", niche_vars[2]))
  var_z <- as.formula(paste0("~", niche_vars[3]))

  p <- plotly::plot_ly(...) %>%
    plotly::add_markers(data = bg_sample, x = var_x, y = var_y, z = var_z,
                        marker = list(color = "grey", size = 2, opacity = 0.2),
                        name = "Background Environment") %>%
    plotly::add_trace(data = niche$surface, x = ~x, y = ~y, z = ~z,
                      type = "scatter3d", mode = "lines",
                      line = list(color = "blue", width = 1),
                      name = "Niche Boundary") %>%
    plotly::add_markers(data = occurrences, x = var_x, y = var_y, z = var_z,
                        marker = list(color = "darkred", size = 2.5),
                        name = "Sampled Occurrences") %>%
    plotly::layout(
      title = "Sampled Occurrences in 3D Environmental Space",
      scene = list(
        xaxis = list(title = niche_vars[1]),
        yaxis = list(title = niche_vars[2]),
        zaxis = list(title = niche_vars[3])
      )
    )
  return(p)
}
