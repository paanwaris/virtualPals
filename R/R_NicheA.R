# Title: Virtual Species NicheA adaptation in R
# Author: Mariana Castaneda-Guzman, Connor and Nine
# Date Created: 07/17/2025
# Date Last Updated: 07/30/2025


# Packages ----------------------------------------------------------------

library(ggplot2)
library(ggpubr)
library(terra)
library(dplyr)
library(plotly)

# Environmental Predictors ------------------------------------------------

# Dummy Data
# set.seed(123)
# n <- 1000
# 
# env_x <- rnorm(n, mean = 0.2, sd = 1)
# env_y <- runif(n, min = 0, max = 3)
# env_z <- rbeta(n, shape1 = 0.7, shape2 = 0.2)
# 
# df <- data.frame(env_1 = env_1, 
#                  env_2 = env_2,
#                  env_3 = env_3)


# Based on WorldClim Data
bio_1 <- terra::rast("BioClim/Bio1.tif")
bio_4 <- terra::rast("BioClim/Bio4.tif")
bio_12 <- terra::rast("BioClim/Bio12.tif")

# Sample this distributions assume normality
set.seed(567)
n <- 5000

bio_stack <- c(bio_1, bio_12, bio_4)
bio_df <- as.data.frame(bio_stack, xy = TRUE)

df <- bio_df[sample(1:nrow(bio_df), size = n, replace = FALSE), ] 
names(df) <- c("x", "y", "env_x", "env_y", "env_z")

ggplot(data = df) +
  geom_density(aes(x = env_x), fill = "lightblue", alpha = 0.5) +
  geom_density(aes(x = env_y), fill = "lightgreen", alpha = 0.5) +
  geom_density(aes(x = env_z), fill = "red", alpha = 0.5) +
  theme_bw()

pairs(df[3:5])
corrplot::corrplot(cor(df))

p_main_y_x <- ggplot(df, aes(x = env_y, y = env_x)) +
  geom_point(alpha = 0.5, color = "grey") +
  theme_bw() +
  theme(
    axis.title = element_blank()
  )
p_main_y_x

p_main_z_x <- ggplot(df, aes(x = env_z, y = env_x)) +
  geom_point(alpha = 0.5, color = "grey") +
  theme_bw() +
  theme(
    axis.title = element_blank()
  )
p_main_z_x

p_main_z_y <- ggplot(df, aes(x = env_z, y = env_y)) +
  geom_point(alpha = 0.5, color = "grey") +
  theme_bw() +
  theme(
    axis.title = element_blank()
  )
p_main_z_y

x_name <- ggplot() +
  theme_void() +
  geom_text(aes(0,0,label='ENV X')) +
  xlab(NULL)

y_name <- ggplot() +
  theme_void() +
  geom_text(aes(0,0,label='ENV Y')) +
  xlab(NULL)

z_name <- ggplot() +
  theme_void() +
  geom_text(aes(0,0,label='ENV Z')) +
  xlab(NULL)

ggarrange(x_name, p_main_y_x, p_main_z_x,
          NULL, y_name, p_main_z_y,
          NULL, NULL, z_name, ncol = 3, nrow = 3, 
          widths = c(0.15, 0.425, 0.425),
          heights = c(0.45, 0.45, 0.15))


p_main_3d <- plot_ly(df, x = ~env_x, y = ~env_y, z = ~env_z,
                     type = 'scatter3d', mode = 'markers',
                     marker = list(color = "lightgrey", size = 2))
p_main_3d


# Create Ellipsoids -------------------------------------------------------

source("get_parametric_ellipsoid.R")

# First create a 3d ellispoid

# 3D Ellipsoid #
center <- c(150, 800, 1400) # center
axes <- c(50, 500, 1000) # offsets
# angles = c(pi/6, pi/6, pi/4)
n_points <- 50 # more for plotting

# center <- c(0, 1000, 500) # center
# axes <- c(15, 500, 250) # offsets
# # angles = c(pi/6, pi/6, pi/4)
# n_points <- 50 # more for plotting


ell_1 <- get_parametric_ellipsoid(center = center,
                                  axes = axes,
                                  # angles = c(pi/6, pi/6, pi/4),
                                  n_points = n_points)


p_main_3d <- plot_ly(df, x = ~env_x, y = ~env_y, z = ~env_z,
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

# Extract points
pts_in <- extract_points_in_ellipsoid(ell_1,
                                      df[, c("env_x","env_y","env_z")])
pts_in <- as.data.frame(pts_in)


# Add point inside the ellipsoid
p_main_3d %>% 
  add_markers(data = pts_in, x=~env_x, y=~env_y, z=~env_z,
              marker=list(color="green", size = 6),
              name="Inside ellipsoid", inherit = FALSE)



# NOW VISUALIZE IT IN 2D


# 2D Ellipse 

# center <- c(10, 500, 400) # center
# axes <- c(15, 300, 300) # offsets

# For env_2 vs env_1
# Center at x = 500 and y = 0

center_y_x <- c(ell_1$center[2], ell_1$center[1]) # center
axes_y_x <- c(ell_1$axes[2], ell_1$axes[1]) # offsets

center_z_x <- c(ell_1$center[3], ell_1$center[1]) # center
axes_z_x <- c(ell_1$axes[3], ell_1$axes[1]) # offsets

center_z_y <- c(ell_1$center[3], ell_1$center[2]) # center
axes_z_y <- c(ell_1$axes[3], ell_1$axes[2]) # offsets


ell2d_y_x <- get_parametric_ellipsoid(center = center_y_x,
                                      axes = axes_y_x,
                                      angles = c(0,0))                               

ell2d_z_x <- get_parametric_ellipsoid(center = center_z_x,
                                      axes = axes_z_x,
                                      angles = c(0,0))

ell2d_z_y <- get_parametric_ellipsoid(center = center_z_y,
                                      axes = axes_z_y,
                                      angles = c(0,0))  


ell_y_x <- p_main_y_x +
  geom_path(data = ell2d_y_x$surface, mapping = aes(x, y), 
            color = "blue", size = 0.5) +
  geom_segment(aes(x = ell2d_y_x$center[1] - ell2d_y_x$axes[1], 
                   xend = ell2d_y_x$center[1] + ell2d_y_x$axes[1], 
                   y = ell2d_y_x$center[2], yend = ell2d_y_x$center[2]), 
               color = "orange", linetype = "dashed") +
  geom_segment(aes(y = ell2d_y_x$center[2] - ell2d_y_x$axes[2], 
                   yend = ell2d_y_x$center[2] + ell2d_y_x$axes[2], 
                   x = ell2d_y_x$center[1], xend = ell2d_y_x$center[1]), 
               color = "orange", linetype = "dashed") +
  geom_point(aes(x = ell2d_y_x$center[1], y = ell2d_y_x$center[2]),
             color = "red", size =2)


ell_z_x <- p_main_z_x +
  geom_path(data = ell2d_z_x$surface, mapping = aes(x, y), 
            color = "blue", size = 0.5) +
  geom_segment(aes(x = ell2d_z_x$center[1] - ell2d_z_x$axes[1], 
                   xend = ell2d_z_x$center[1] + ell2d_z_x$axes[1], 
                   y = ell2d_z_x$center[2], yend = ell2d_z_x$center[2]), 
               color = "orange", linetype = "dashed") +
  geom_segment(aes(y = ell2d_z_x$center[2] - ell2d_z_x$axes[2], 
                   yend = ell2d_z_x$center[2] + ell2d_z_x$axes[2], 
                   x = ell2d_z_x$center[1], xend = ell2d_z_x$center[1]), 
               color = "orange", linetype = "dashed") +
  geom_point(aes(x = ell2d_z_x$center[1], y = ell2d_z_x$center[2]), 
             color = "red", size =2) 


ell_z_y <- p_main_z_y +
  geom_path(data = ell2d_z_y$surface, mapping = aes(x, y), 
            color = "blue", size = 0.5) +
  geom_segment(aes(x = ell2d_z_y$center[1] - ell2d_z_y$axes[1], 
                   xend = ell2d_z_y$center[1] + ell2d_z_y$axes[1], 
                   y = ell2d_z_y$center[2], yend = ell2d_z_y$center[2]), 
               color = "orange", linetype = "dashed") +
  geom_segment(aes(y = ell2d_z_y$center[2] - ell2d_z_y$axes[2], 
                   yend = ell2d_z_y$center[2] + ell2d_z_y$axes[2], 
                   x = ell2d_z_y$center[1], xend = ell2d_z_y$center[1]), 
               color = "orange", linetype = "dashed") +
  geom_point(aes(x = ell2d_z_y$center[1], y = ell2d_z_y$center[2]),
             color = "red", size =2) 


ggarrange(x_name, ell_y_x, ell_z_x,
          NULL, y_name, ell_z_y,
          NULL, NULL, z_name, ncol = 3, nrow = 3, 
          widths = c(0.15, 0.425, 0.425),
          heights = c(0.45, 0.45, 0.15))


# Extract Points from Ellipsoid -------------------------------------------

pts_in <- extract_points_in_ellipsoid(ell_1,
                                      df[, c("env_x","env_y","env_z")])

ell_y_x <- p_main_y_x +
  geom_path(data = ell2d_y_x$surface, mapping = aes(x, y), 
            color = "blue", size = 0.5) +
  geom_segment(aes(x = ell2d_y_x$center[1] - ell2d_y_x$axes[1], 
                   xend = ell2d_y_x$center[1] + ell2d_y_x$axes[1], 
                   y = ell2d_y_x$center[2], yend = ell2d_y_x$center[2]), 
               color = "orange", linetype = "dashed") +
  geom_segment(aes(y = ell2d_y_x$center[2] - ell2d_y_x$axes[2], 
                   yend = ell2d_y_x$center[2] + ell2d_y_x$axes[2], 
                   x = ell2d_y_x$center[1], xend = ell2d_y_x$center[1]), 
               color = "orange", linetype = "dashed") +
  geom_point(aes(x = ell2d_y_x$center[1], y = ell2d_y_x$center[2]),
             color = "red", size =2) +
  geom_point(data = pts_in, aes(x = env_y, y = env_x), 
             color = "green4")

ell_y_x


ell_z_x <- p_main_z_x +
  geom_path(data = ell2d_z_x$surface, mapping = aes(x, y), 
            color = "blue", size = 0.5) +
  geom_segment(aes(x = ell2d_z_x$center[1] - ell2d_z_x$axes[1], 
                   xend = ell2d_z_x$center[1] + ell2d_z_x$axes[1], 
                   y = ell2d_z_x$center[2], yend = ell2d_z_x$center[2]), 
               color = "orange", linetype = "dashed") +
  geom_segment(aes(y = ell2d_z_x$center[2] - ell2d_z_x$axes[2], 
                   yend = ell2d_z_x$center[2] + ell2d_z_x$axes[2], 
                   x = ell2d_z_x$center[1], xend = ell2d_z_x$center[1]), 
               color = "orange", linetype = "dashed") +
  geom_point(aes(x = ell2d_z_x$center[1], y = ell2d_z_x$center[2]), 
             color = "red", size =2) +
  geom_point(data = pts_in, aes(x = env_z, y = env_x), color = "green4")


ell_z_y <- p_main_z_y +
  geom_path(data = ell2d_z_y$surface, mapping = aes(x, y), 
            color = "blue", size = 0.5) +
  geom_segment(aes(x = ell2d_z_y$center[1] - ell2d_z_y$axes[1], 
                   xend = ell2d_z_y$center[1] + ell2d_z_y$axes[1], 
                   y = ell2d_z_y$center[2], yend = ell2d_z_y$center[2]), 
               color = "orange", linetype = "dashed") +
  geom_segment(aes(y = ell2d_z_y$center[2] - ell2d_z_y$axes[2], 
                   yend = ell2d_z_y$center[2] + ell2d_z_y$axes[2], 
                   x = ell2d_z_y$center[1], xend = ell2d_z_y$center[1]), 
               color = "orange", linetype = "dashed") +
  geom_point(aes(x = ell2d_z_y$center[1], y = ell2d_z_y$center[2]),
             color = "red", size =2) +
  geom_point(data = pts_in, aes(x = env_z, y = env_y), color = "green4")
ell_z_y


ggarrange(x_name, ell_y_x, ell_z_x,
          NULL, y_name, ell_z_y,
          NULL, NULL, z_name, ncol = 3, nrow = 3, 
          widths = c(0.15, 0.425, 0.425),
          heights = c(0.45, 0.45, 0.15))



# Sampling Occurrences ----------------------------------------------------

source("get_sample_occurrences.R")


sampled_pts <- get_sample_occurrences(ellipsoid = ell_1,
                                      raster_df = df,
                                      pred_cols = c("env_x","env_y","env_z"),
                                      n = 100,
                                      seed = 101,
                                      sigma_scale = 0.5 # half‐variance to sharpen
                                      )


# Create density points to add to the sides
# env x goes to the right, 
# env y goes to top and right, 
# and env z goes top

ell_y_x <- p_main_y_x +
  geom_path(data = ell2d_y_x$surface, mapping = aes(x, y), 
            color = "blue", size = 0.5) +
  geom_segment(aes(x = ell2d_y_x$center[1] - ell2d_y_x$axes[1], 
                   xend = ell2d_y_x$center[1] + ell2d_y_x$axes[1], 
                   y = ell2d_y_x$center[2], yend = ell2d_y_x$center[2]), 
               color = "orange", linetype = "dashed") +
  geom_segment(aes(y = ell2d_y_x$center[2] - ell2d_y_x$axes[2], 
                   yend = ell2d_y_x$center[2] + ell2d_y_x$axes[2], 
                   x = ell2d_y_x$center[1], xend = ell2d_y_x$center[1]), 
               color = "orange", linetype = "dashed") +
  geom_point(aes(x = ell2d_y_x$center[1], y = ell2d_y_x$center[2]),
             color = "red", size =2) +
  geom_point(data = sampled_pts, aes(x = env_y, y = env_x), 
             color = "lightblue")

ell_y_x


ell_z_x <- p_main_z_x +
  geom_path(data = ell2d_z_x$surface, mapping = aes(x, y), 
            color = "blue", size = 0.5) +
  geom_segment(aes(x = ell2d_z_x$center[1] - ell2d_z_x$axes[1], 
                   xend = ell2d_z_x$center[1] + ell2d_z_x$axes[1], 
                   y = ell2d_z_x$center[2], yend = ell2d_z_x$center[2]), 
               color = "orange", linetype = "dashed") +
  geom_segment(aes(y = ell2d_z_x$center[2] - ell2d_z_x$axes[2], 
                   yend = ell2d_z_x$center[2] + ell2d_z_x$axes[2], 
                   x = ell2d_z_x$center[1], xend = ell2d_z_x$center[1]), 
               color = "orange", linetype = "dashed") +
  geom_point(aes(x = ell2d_z_x$center[1], y = ell2d_z_x$center[2]), 
             color = "red", size =2) +
  geom_point(data = sampled_pts, aes(x = env_z, y = env_x),
             color = "lightblue")


ell_z_y <- p_main_z_y +
  geom_path(data = ell2d_z_y$surface, mapping = aes(x, y), 
            color = "blue", size = 0.5) +
  geom_segment(aes(x = ell2d_z_y$center[1] - ell2d_z_y$axes[1], 
                   xend = ell2d_z_y$center[1] + ell2d_z_y$axes[1], 
                   y = ell2d_z_y$center[2], yend = ell2d_z_y$center[2]), 
               color = "orange", linetype = "dashed") +
  geom_segment(aes(y = ell2d_z_y$center[2] - ell2d_z_y$axes[2], 
                   yend = ell2d_z_y$center[2] + ell2d_z_y$axes[2], 
                   x = ell2d_z_y$center[1], xend = ell2d_z_y$center[1]), 
               color = "orange", linetype = "dashed") +
  geom_point(aes(x = ell2d_z_y$center[1], y = ell2d_z_y$center[2]),
             color = "red", size =2) +
  geom_point(data = sampled_pts, aes(x = env_z, y = env_y), 
             color = "lightblue")
ell_z_y


env_z_top <- ggplot(sampled_pts, aes(x = env_z)) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  scale_x_continuous(limits = c(min(df$env_z), max(df$env_z))) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) 

env_y_top <- ggplot(sampled_pts, aes(x = env_y)) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  scale_x_continuous(limits = c(min(df$env_y), max(df$env_y))) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) 

env_x_right <- ggplot(sampled_pts, aes(x = env_x)) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  coord_flip() +
  scale_x_continuous(limits = c(min(df$env_x), max(df$env_x))) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

env_y_right <- ggplot(sampled_pts, aes(x = env_y)) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  coord_flip() +
  scale_x_continuous(limits = c(min(df$env_y), max(df$env_y))) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
    )


# env x goes to the right, 
# env y goes to top and right, 
# and env z goes top

ggarrange(NULL, env_y_top, env_z_top, NULL,
          x_name, ell_y_x, ell_z_x, env_x_right,
          NULL, y_name, ell_z_y, env_y_right,
          NULL, NULL, z_name, NULL,
          ncol = 4, nrow = 4, 
          widths = c(0.1, 0.4, 0.4, 0.1),
          heights = c(0.1, 0.4, 0.4, 0.1))



# Add occ point inside the ellipsoid
p_main_3d %>% 
  add_markers(data = sampled_pts, x=~env_x, y=~env_y, z=~env_z,
              marker=list(color="lightblue", size = 3),
              name="Occurrence", inherit = FALSE)




# Plot in geography -------------------------------------------------------

# 1. Plot the background layer
plot(bio_stack[[1]],
     main = "Sampled points inside ellipsoid",
     col  = terrain.colors(50))

# 2. Overlay the sampled points
points(sampled_pts$x, sampled_pts$y,
       pch   = 20,         # solid circle
       col   = "red",
       cex   = 0.7)        # point size multiplier

# Have to plot the ellipsoid area in geography


# Code Graveyard ----------------------------------------------------------


# img1 <- png::readPNG("newplot.png")
# 
# im_A <- ggplot() + 
#   background_image(img1) 
#   
# 
# ggarrange(p_env1, ell_2_1, ell_3_1,
#           NULL, p_env2, ell_3_2,
#           NULL, im_A, p_env3, ncol = 3, nrow = 3, 
#           widths = c(0.15, 0.425, 0.425))









