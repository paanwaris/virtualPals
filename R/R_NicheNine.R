# Ensure these packages are installed before running
library(terra)
library(ggplot2)
library(ggpubr)
library(plotly)
library(corrplot)
library(rnaturalearth) # For fetching world map data
library(sf)            # For handling vector data

# --- 2. Load Environmental Data ---
# Load the bioclimatic raster layers. These represent the environmental variables.
# Bio1 = Annual Mean Temperature
# Bio12 = Annual Precipitation
# Bio4 = Temperature Seasonality
bio_1 <- terra::rast("BioClim/Bio1.tif")
bio_12 <- terra::rast("BioClim/Bio12.tif")
bio_4 <- terra::rast("BioClim/Bio4.tif")

# Combine the layers into a single SpatRaster stack
bio_stack <- c(bio_1, bio_12, bio_4)
names(bio_stack) <- c("AnnualTemp", "AnnualPrecip", "TempSeasonality")
# --- 2b. Mask to Terrestrial Areas Only ---
# Get world country boundaries and dissolve them into a single landmass polygon
world_map_sf <- ne_countries(scale = "medium", returnclass = "sf")
land_polygon_vect <- vect(world_map_sf)
plot(land_polygon_vect)
# Mask the environmental raster to keep only land areas
bio_stack_terrestrial <- mask(bio_stack, land_polygon_vect)
plot(bio_stack_terrestrial)

# Convert the masked raster stack to a data frame for analysis.
# 'na.rm=TRUE' is important to remove ocean pixels, making the data frame smaller.
bio_df <- as.data.frame(bio_stack_terrestrial, xy = TRUE, na.rm = TRUE)
df <- bio_df[sample(1:nrow(bio_df), size = 100000, replace = FALSE), ] 
# --- 4. Define the Species Niche (The Ellipsoid) ---

# Define the center and axes (niche breadth) of the 3D environmental ellipsoid.
# These values correspond to the environmental variables in bio_stack:
# Center: c(AnnualTemp, AnnualPrecip, TempSeasonality)
# Axes:   c(Tolerance for Temp, Tolerance for Precip, Tolerance for Seasonality)
niche_center <- c(15, 500, 12000) 
niche_axes <- c(100, 500, 2500)   

# Create the ellipsoid object that represents our virtual species' niche
ell_1 <- get_parametric_ellipsoid(center = niche_center,
                                          axes = niche_axes,
                                          n_points = 50)

p_main_3d <- plot_ly(df, x = ~AnnualTemp, y = ~AnnualPrecip, z = ~TempSeasonality,
                     type = 'scatter3d', mode = 'markers',
                     marker = list(color = "lightgrey",
                                   size = 3),
                     name = "Points") %>%
  add_trace(data = ell_1$surface, x=~x, y=~y, z=~z,
            type="scatter3d", mode="lines",
            line=list(color="blue"),
            name="Ellipsoid surface", inherit = FALSE) %>%
  add_markers(x = ell_1$center[1], y = ell_1$center[2], z = ell_1$center[3],
              marker = list(color = 'red', size = 5),
              name = "Center") %>%
  layout(
    title = "Ellipsoid in 3D with Axes and Center",
    scene = list(
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "Z")
    ),
    legend = list(x = 0.05, y = 0.95)
  )

p_main_3d  # Display the final plot


# --- 5. Classify Environmental Space (The Niche Model) ---

# Get the environmental data for every pixel from our data frame
points_to_classify <- as.matrix(bio_df[, c("AnnualTemp", "AnnualPrecip", "TempSeasonality")])

# Calculate the squared Mahalanobis distance for every pixel to the niche center.
# This determines if a pixel's environment is "inside" the niche definition.
diffs <- sweep(points_to_classify, 2, ell_1$center, "-")
mahalanobis_sq <- rowSums((diffs %*% ell_1$Sigma_inv) * diffs)

# A pixel is suitable (1) if its distance is <= 1, and unsuitable (0) otherwise.
# We add this classification as a new column to our main data frame.
bio_df$suitability <- as.integer(mahalanobis_sq <= 1)

# Report how many suitable pixels were found
cat("Number of suitable pixels found:", sum(bio_df$suitability), "\n")


# --- 6. Predict to Geographic Space (Create the Binary Map) ---

# Create a new data frame with just the geographic coordinates (x, y)
# and the binary suitability score (0 or 1).
binary_map_df <- bio_df[, c("x", "y", "suitability")]

# Convert this data frame back into a SpatRaster.
# 'type="xyz"' tells rast() that the columns are x, y, and the cell value.
# We must assign the same Coordinate Reference System (CRS) as the original raster.
binary_map <- terra::rast(binary_map_df, type = "xyz", crs = crs(bio_stack))


# Get world country boundaries for context
world_map <- ne_countries(scale = "medium", returnclass = "sf")
world_vect <- vect(world_map)

# Plot the final binary map first
plot(binary_map,
     col = c("grey90", "darkgreen"))

# Add the world boundaries on top of the raster plot
lines(world_vect, col = "black", lwd = 0.5)

# --- 8. (Optional) Visualize Niche in 3D Environmental Space ---

# Create a data frame of only the points that are inside the ellipsoid
points_inside_df <- bio_df[bio_df$suitability == 1, ]
set.seed(123) # for reproducibility
unsuitable_points_sample <- bio_df[bio_df$suitability == 0, ][sample(sum(bio_df$suitability == 0), 50000), ]
suitable_points <- bio_df[bio_df$suitability == 1, ][sample(sum(bio_df$suitability == 1), 10000), ]
bio_df_sample <- rbind(unsuitable_points_sample, suitable_points)


# Create the 3D plot using the subsampled data
p_inside_3d <- plot_ly(data = unsuitable_points_sample, 
            x = ~AnnualTemp, y = ~AnnualPrecip, z = ~TempSeasonality,
            type = 'scatter3d', mode = 'markers',
            marker = list(color = "lightgrey", size = 2),
            name = "Background Environment") %>%
  # Add the points inside the niche in green
  add_trace(data = suitable_points, 
            x = ~AnnualTemp, y = ~AnnualPrecip, z = ~TempSeasonality,
            type = 'scatter3d', mode = 'markers',
            marker = list(color = "darkgreen", size = 1.5),
            name = "Points in Niche") %>%
  # Add the ellipsoid surface wireframe
  add_trace(data = ell_1$surface, x=~x, y=~y, z=~z,
            type="scatter3d", mode="lines",
            line=list(color="blue", width = 2),
            name="Ellipsoid Niche Surface", inherit = FALSE) %>%
  # Add the center point of the niche
  add_markers(x = ell_1$center[1], y = ell_1$center[2], z = ell_1$center[3],
              marker = list(color = 'red', size = 5),
              name = "Niche Center", inherit = FALSE) %>%
  layout(
    title = "Ellipsoid Niche in 3D Environmental Space (Subsampled)",
    scene = list(
      xaxis = list(title = "Annual Temp"),
      yaxis = list(title = "Annual Precip"),
      zaxis = list(title = "Temp Seasonality")
    ),
    # The legend is now created automatically by the 'color' mapping
    legend = list(x = 0.05, y = 0.95, title=list(text='<b>Suitability</b>'))
  )

# Display the final plot
p_inside_3d
