#' Generate a Geographic Suitability Map
#'
#' Projects an ellipsoidal niche onto a set of environmental raster layers
#' to create a geographic map of habitat suitability.
#'
#' @param env_rasters A `SpatRaster` object from the `terra` package. Each layer
#'   represents an environmental variable. Layer names must match the variable
#'   names used to define the niche.
#' @param niche An `ellipsoid` object created by `build_ellipsoid()`. The
#'   `center` vector within this object **must be named** (e.g., `center = c(temp = 15, precip = 1200)`).
#' @param output_type Character string specifying the output format. Can be:
#'   - `"binary"` (default): Returns 1 for suitable (inside the niche) and 0 for unsuitable.
#'   - `"probabilistic"`: Returns continuous suitability from 0 to 1 based on a Gaussian decay function from the niche center.
#'   - `"distance"`: Returns the raw Mahalanobis distance from the niche center.
#'
#' @return A `SpatRaster` object representing the geographic map of suitability.
#' @family ellipsoid functions
#' @export
#' @examples
#' \dontrun{
#' library(terra)
#'
#' # This entire block of code will not be run during package checks.
#'
#' # 1. Create a dummy environmental raster stack
#' r <- rast(xmin=-180, xmax=180, ymin=-90, ymax=90, res=10, crs="WGS84")
#' temp <- init(r, fun=function(x) { 15 + 15 * sin(pi * yFromCell(r, x) / 180) })
#' precip <- init(r, fun=function(x) { 1000 - 500 * sin(pi * yFromCell(r, x) / 180) })
#' # ... (rest of the example code) ...
#'
#' plot(suit_map_prob, main = "Probabilistic Suitability")
#' }
generate_suitability <- function(env_rasters, niche, output_type = "binary") {
  # --- 1. Input Validation ---
  if (!inherits(env_rasters, "SpatRaster")) {
    stop("'env_rasters' must be a SpatRaster object from the 'terra' package.")
  }
  if (!inherits(niche, "ellipsoid")) {
    stop("'niche' must be an object of class 'ellipsoid' created by build_ellipsoid().")
  }

  niche_vars <- names(niche$center)
  if (is.null(niche_vars)) {
    stop("Niche definition is missing variable names. Please name the 'center' vector.")
  }
  if (!all(niche_vars %in% names(env_rasters))) {
    missing_vars <- niche_vars[!niche_vars %in% names(env_rasters)]
    stop(paste("Niche variable(s) not found in rasters:", paste(missing_vars, collapse = ", ")))
  }

  # --- 2. Data Preparation ---
  message("Converting rasters to data frame...")
  env_df <- terra::as.data.frame(env_rasters, xy = TRUE, na.rm = TRUE)

  # Robustly select and order columns to match the niche definition
  points_to_classify <- as.matrix(env_df[, niche_vars, drop = FALSE])

  # --- 3. Calculate Mahalanobis Distance ---
  message("Calculating suitability for ", format(nrow(points_to_classify), big.mark = ","), " pixels...")
  diffs <- sweep(points_to_classify, 2, niche$center, "-")
  m_sq_dist <- rowSums((diffs %*% niche$Sigma_inv) * diffs)

  # --- 4. Determine Suitability ---
  suitability <- switch(output_type,
                        binary = as.integer(m_sq_dist <= 1),
                        probabilistic = exp(-0.5 * m_sq_dist), # Gaussian decay
                        distance = sqrt(m_sq_dist),
                        stop("Invalid 'output_type'. Choose 'binary', 'probabilistic', or 'distance'.")
  )

  # --- 5. Reconstruct the Raster ---
  message("Reconstructing suitability raster...")
  result_df <- data.frame(x = env_df$x, y = env_df$y, suitability = suitability)

  suitability_raster <- terra::rast(
    result_df,
    type = "xyz",
    crs = terra::crs(env_rasters)
  )
  names(suitability_raster) <- output_type

  message("Done.")
  return(suitability_raster)
}
