#' Sample Occurrence Points from a Suitable Environment
#'
#' @description
#' Finds all suitable habitats in a set of environmental rasters based on a
#' niche definition, and then samples occurrence points from that suitable
#' space using different ecological scenarios.
#'
#' @param n The number of occurrence points to sample.
#' @param env_rasters A `SpatRaster` object with environmental data.
#' @param niche An `ellipsoid` object created by `build_ellipsoid()`.
#' @param method A character string specifying the sampling method.
#'   - `"random"` (default): Uniformly random sample from all suitable habitats.
#'   - `"center"`: Preferentially samples from suitable habitats that are
#'     closest to the niche's central optimum.
#'   - `"edge"`: Preferentially samples from suitable habitats that are near the
#'     edge of the niche tolerance.
#'
#' @return A data frame of `n` occurrence points, including their geographic
#'   coordinates (x, y) and environmental values.
#' @family ellipsoid functions
#' @export
sample_points_niche <- function(n, env_rasters, niche, method = "random") {

  # --- 1. Find all suitable points in the environment ---
  message("Finding all suitable habitats (this can be slow on large rasters)...")
  env_df <- as.data.frame(env_rasters, xy = TRUE, na.rm = TRUE)
  niche_vars <- names(niche$center)

  # Calculate suitability for all points
  diffs <- sweep(as.matrix(env_df[, niche_vars]), 2, niche$center, "-")
  m_sq_dist <- rowSums((diffs %*% niche$Sigma_inv) * diffs)

  # Create a pool of all points that are inside the niche
  suitable_pool <- env_df[m_sq_dist <= 1, ]
  if (nrow(suitable_pool) == 0) {
    stop("No suitable habitats found in the provided environment for this niche.")
  }

  # Keep the distance calculation for weighted sampling
  suitable_pool$dist_sq <- m_sq_dist[m_sq_dist <= 1]

  # --- 2. Sample from the suitable pool using the chosen method ---
  message(paste("Sampling", n, "points using the '", method, "' method...", sep=""))
  n_safe <- min(n, nrow(suitable_pool))

  sampled_indices <- switch(
    method,
    "random" = {
      sample(nrow(suitable_pool), size = n_safe, replace = FALSE)
    },
    "center" = {
      # Preferentially sample points with a *small* distance to the center
      # A lower distance means a higher weight
      weights <- 1 - sqrt(suitable_pool$dist_sq)
      sample(nrow(suitable_pool), size = n_safe, replace = FALSE, prob = weights)
    },
    "edge" = {
      # Preferentially sample points with a *large* distance to the center
      # A higher distance means a higher weight
      weights <- sqrt(suitable_pool$dist_sq)
      sample(nrow(suitable_pool), size = n_safe, replace = FALSE, prob = weights)
    }
  )

  final_sample <- suitable_pool[sampled_indices, ]

  message("Done.")
  # Return the data frame, removing the temporary distance column
  return(final_sample[, -which(names(final_sample) == "dist_sq")])
}
