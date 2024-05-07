# HEADER ---------------------------------------------------------------
#
# Author: Charles Cunningham
# Email: charles.cunningham@york.ac.uk
# 
# Script name: Aggregate climate quantiles into single plots
#
# Script Description: Generate single 'climate' quantiles based on
# all climate data, and only temperature-related climate data
# N.B. Takes several hours to complete

# LOAD LIBRARIES & INSTALL PACKAGES ---------------------

# Change  library to R: (not enough space on C:)
.libPaths("R:/rsrch/cb751/lab/Charles/R/PackageLibrary")

# Load packages
library(terra)
library(sf)
library(tidyverse)
library(cowplot)

# Set fraction of RAM that may be used by the program
terraOptions(memfrac = 0.9)

# LOAD SPATIAL AND CLIMATE DATA -----------------------

# Climate data
climQuantsMerge <- rast("../Data/ProcessedData/climQuantsMerge.tif")

# Land map
land <- vect("../Data/land.shp")

# Reintroduction sites
sites <- vect("../Data/reintroductionSites.shp",
              crs = "EPSG:4326")

# SET PARAMETERS ---------------------------------------
# N.B. Cannot plot with decimal values as issue with large memory spatRaster,
# so use integers (use labels as guide)

# Quantiles to use for site selection (20%, 30%, 40%, 50%)
quants <- c(0.2, 0.3, 0.4, 0.5)

# Set labels (N.B. note for integer interpretation)
plotLabels <- c("0" = ">50%",
                "1" = "50%",
                "2" = "40%",
                "3" = "30%",
                "4" = "20%")
# Set colours
plotColours <- c("0" = "#E5FFFF",
                 "1" = "#99E5FF",
                 "2" = "#65BFFF",
                 "3" = "#3288FF",
                 "4" = "#003FFF")

# Plot breaks
plotBreaks <- c("4", "3", "2", "1", "0")

# Set title labels
titleLabeller <- data.frame(
  aggregationType = c("All",
                      "Temperature only"),
  label = c("Overall climate similarity\n                   ",
            "Overall temperature-related\nclimate similarity"))

# Set index labels
indexLabeller <- data.frame(
  aggregationType = c("All",
                      "Temperature only"),
  label = c("(a)", "(b)"))

# ASSESS CLIMATE VARIABLES TOGETHER -----------------------------

# Find cells which are within a given quantile for all climate covariates
allQuants <- min(climQuantsMerge)

# Convert allQuants to data frame
allQuants_df <- as.data.frame(allQuants, xy = TRUE) %>%
  na.omit() %>%
  as_tibble # Speeds up plot

# Plot
allQuantMap <- ggplot(allQuants_df) +

  # Set equal coordinates
  coord_equal() +

  # Climate raster
  geom_tile( aes(x = x, y = y,
                 colour = factor(min), fill = factor(min))) +

  # Set colour
  scale_colour_manual("Climate\nsimilarity\nquantile",
                      values = plotColours,
                      labels = plotLabels,
                      breaks = plotBreaks,
                      aesthetics = c("colour", "fill")) +

  # Country boundary polygons and sites
  geom_sf(data = st_as_sf(land), fill = NA,
          colour = "black", inherit.aes = FALSE) +
  geom_sf(data = st_as_sf(sites),
          colour = "#D91630", inherit.aes = FALSE) +

  # Add title
  geom_text(x = -4, y = 70,
            label = "Overall climate similarity   ",
            size = 10) +

  # Set theme parameters
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white"),
        legend.position = c(0.15, 0.5),
        legend.title = element_text(size=24),
        legend.text = element_text(size=24))

# Save
ggsave(filename = paste0("../Plots/", "Climate_similarity_all.png"),
       allQuantMap,
       dpi = 600,
       units = "px", width = 8000, height = 7000)

# Remove objects and free unused memory
rm(allQuants_df, allQuantMap)
gc()

# ASSESS CLIMATE VARIABLES TOGETHER (WITHOUT RAINFALL) ----------------------

# Find cells which are within a given quantile for all climate covariates
tempQuants <- subset(climQuantsMerge,
                     "totalRain",
                     negate = TRUE) %>%
  min

# Convert tempQuants to data frame
tempQuants_df <- as.data.frame(tempQuants, xy = TRUE) %>%
  na.omit() %>%
  as_tibble

# Plot
tempQuantMap <- ggplot(data = tempQuants_df) +

  # Set equal coordinates
  coord_equal() +

  # Climate raster
  geom_tile( aes(x = x, y = y,
                 colour = factor(min), fill = factor(min))) +

  # Set colour
  scale_colour_manual("Climate\nsimilarity\nquantile",
                      values = plotColours,
                      labels = plotLabels,
                      breaks = plotBreaks,
                      aesthetics = c("colour", "fill")) +

  # Add title
  geom_text(x = -2, y = 70,
            label = "Overall temperature-related\nclimate similarity",
            size = 10) +

  # Country boundary polygons and sites
  geom_sf(data = st_as_sf(land), fill = NA,
          colour = "black", inherit.aes = FALSE) +
  geom_sf(data = st_as_sf(sites),
          colour = "#D91630", inherit.aes = FALSE) +

  # Set theme parameters
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white"),
        legend.position = c(0.15, 0.5),
        legend.title = element_text(size=24),
        legend.text = element_text(size=24))

# Save, with large text title
ggsave(filename = paste0("../Plots/", "Climate_similarity_temp.png"),
       tempQuantMap,
       dpi = 600,
       units = "px", width = 8000, height = 7000)

# Remove objects and free unused memory
rm(tempQuants_df, tempQuantMap)
gc()

# CREATE COMBINED PLOT ----------------------------------------------

# Join quantiles aggregated by 'all climate' variables, and 'just 
# temperature' variables from previous two sections into single spatRast
combinedQuants <- c(allQuants, tempQuants)

# Change names to reflect aggregation type
names(combinedQuants) <- c("All", "Temperature only")

# Create data frame from combinedQuants for plot
combinedQuants_df <- as.data.frame(combinedQuants, xy = TRUE ) %>% 
  na.omit() %>%
  pivot_longer(!c(x,y),
               names_to = "aggregationType",
               values_to = "value")

# Plot
combinedQuantMap <- ggplot(data = combinedQuants_df) +
  
  # Set equal coordinates
  coord_equal() +
  
  # Climate raster
  geom_tile( aes(x = x, y = y,
                 colour = factor(value), fill = factor(value))) +
  facet_wrap(~ aggregationType) +
  
  # Set colour
  scale_colour_manual("Climate similarity quantile",
                      values = plotColours,
                      labels = plotLabels,
                      breaks = plotBreaks,
                      aesthetics = c("colour", "fill")) +
  
  # Country boundary polygons and sites
  geom_sf(data = st_as_sf(land), fill = NA,
          colour = "black", inherit.aes = FALSE) +
  geom_sf(data = st_as_sf(sites),
          colour = "#D91630", inherit.aes = FALSE) +
  
  # Add title and index labels
  geom_text(data = titleLabeller,
            x = 0, y = 71.2,
            aes(label = label),
            size  = 5) +
  geom_text(data = indexLabeller,
            x = -28, y = 73.3,
            aes(label = label),
            size  = 6,
            fontface = "bold") +
  
  # Set theme parameters
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white"),
        legend.position = c(0.5,-0.02),
        legend.title = element_text(size=18),
        legend.text = element_text(size=18),
        legend.direction = "horizontal",
        strip.text = element_blank(),
        plot.margin = margin(0,-1,0,-1, "lines"))

# Save
ggsave(filename = paste0("../Plots/", "Climate_similarity_combined.png"),
       combinedQuantMap,
       dpi = 600,
       units = "px", width = 8000, height = 4000)
