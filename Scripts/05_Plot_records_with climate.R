# HEADER ---------------------------------------------------------------
#
# Author: Charles Cunningham
# Email: charles.cunningham@york.ac.uk
# 
# Script name: Plot BVW records with climate analogues to identify
# potential source populations
#
# Script Description: Using data created in previous scripts, plot BVW records
# and climate quantiles with zoomed in regions and overview maps

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

# Land map
land <- vect("../Data/land.shp")

# Species records
BVW_records <- vect("../Data/ProcessedData/BVW_records.shp")

# Reintroduction locations at different sites
locations <- read.csv("../Data/RawData/possible_locations.csv") %>%
  vect(.,
       geom = c("Longitude", "Latitude"),
       crs = "EPSG:4326")

# SET PARAMETERS ---------------------------------------
# N.B. Cannot plot with decimal values as issue with large memory spatRaster,
# so use integers (use labels as guide)

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

# Set 'cut-out' extents for Brittany and Eastern Pyrenees 
extentBrittany <- c(xmin = -5.9, xmax = 1.3, ymin = 46.2, ymax = 49.9)
extentPyrenees <- c(xmin = -2, xmax = 4, ymin = 40.4, ymax = 44.2)

# LOOP THROUGH SITES -------------------------------------------

# Start loop through different sites here, loop continues to end of script
for (i in unique(locations$Site)) {
  
  # Subset locations to site i
  site_i <- terra::subset(locations, locations$Site == i)
  
  # Load climate quantile data for site i
  climQuantsMerge <- paste0("../Data/ProcessedData/climQuantsMerge_", i, ".tif") %>%
    rast

# PROCESS CLIMATE QUANTILE AND RECORD DATA -------------------------

  # Find cells which are within a given quantile for temperature climate covariates only
  tempQuants <- subset(climQuantsMerge,
                       "totalRain",
                       negate = TRUE) %>%
    min

  # Extract temperature quantile (into "min" column)
  BVW_records <- terra::extract(tempQuants, BVW_records, bind = TRUE)

  # Add column for plot (are records within 30% quantile [>=3] or not)
  BVW_records$similarity <- if_else(BVW_records$min >= 3, "Similar", "Not similar")

  # Drop "min" column
  BVW_records$min <- NULL

  # Subset to only current records
  BVW_records_current <- subset(BVW_records, BVW_records$plotType == "Current")

  # Sort to ensure similar points are placed on top for subsequent plots
  BVW_records_current <- sort(BVW_records_current, "similarity")

# PLOT 'WITHIN QUANTILE' RECORDS ---------------------------

  # Plot
  quantRecordsMap <- ggplot() +
  
    # Set equal coordinates
    coord_equal() +
  
    # Country boundary polygons
    geom_sf(data = st_as_sf(land), colour = "grey80") +
  
    # Plot records
    geom_sf(data = st_as_sf(BVW_records_current),
            inherit.aes = FALSE,
            aes(colour = similarity),
            size = 0.8) +
    
    # Plot sites
    geom_sf(data = st_as_sf(site_i),
            colour = "black", inherit.aes = FALSE) +
  
    # Set aesthetics
    scale_colour_manual("",
                        values = c("#EBCC2A", "#F21A00"),
                        labels = c("Records within\n30% temperature\nquantile\n",
                                   "Other records"),
                        breaks = c("Similar", "Not similar")) +
    guides(colour = guide_legend(override.aes = list(size = 6))) +
  
    # Set theme parameters

    theme_void() +
    
    # Set aesthetics

    theme_void() +
    theme(legend.position = c(0.15,0.4),
          legend.title = element_text(size=24),
          legend.text = element_text(size=24),
          plot.background = element_rect( fill = "white", colour = "white"))

  # Save
  ggsave(filename = paste0("../Plots/", i, "/BVW_records_similarity.png"),
         quantRecordsMap,
         dpi = 600,
         units = "px", width = 8000, height = 7000)

# CREATE ZOOMED IN MAPS -----------------------------------

  # Loop through extents
  for (j in c("extentBrittany", "extentPyrenees")) {

    # Crop land
    zoomLand <- crop(land, get(j))
    
    # Crop temperature quantiles
    zoomQuants <- crop(tempQuants, get(j))
  
    # Crop records
    zoomBVW_records <- crop(BVW_records_current, get(j))
    
    # Plot
    tempQuantMap <- ggplot(data = as.data.frame(zoomQuants, xy = TRUE) %>%
                             na.omit()) +
  
      # Set equal coordinates
      coord_equal() +
  
      # Climate raster (can ignore warning messages)
      geom_raster( aes(x = x, y = y,
                       fill = factor(min))) +
  
      # Country boundary polygons
      geom_sf(data = st_as_sf(zoomLand), fill = NA,
              colour = "black") +
  
      # Plot records
      geom_sf(data = st_as_sf(zoomBVW_records),
              inherit.aes = FALSE,
              aes(colour = similarity),
              size = 0.8) +
  
      # Set aesthetics
      scale_fill_manual("\nClimate\nsimilarity\nquantile",
                        values = plotColours,
                        labels = plotLabels,
                        breaks = plotBreaks) +
      scale_colour_manual("",
                          values = c("#EBCC2A", "#F21A00"),
                          labels = c("Records within\n30% temperature\nquantile\n",
                               "Other records"),
                          breaks = c("Similar", "Not similar")) +
      guides(colour = guide_legend(override.aes = list(size = 6))) +
  
      # Set theme parameters
      theme_void() +
      theme(legend.title = element_text(size = 14),
            legend.text = element_text(size = 14))

    # Assign to object
    assign(paste0(j, "_panel"), tempQuantMap)

  }

# PLOT OVERVIEW MAPS -------------------------------------------

  # Loop through extents
  for (j in c("extentBrittany", "extentPyrenees")) {

    overviewMap <- ggplot() +
  
      # Set coordinates
      coord_cartesian() +
  
      # Country boundary polygons
      geom_sf(data = st_as_sf(land), fill = NA,
              colour = "black", inherit.aes = FALSE) +
  
      # Add panel border 
      geom_rect(data = ext(land) %>% as.vector() %>% t %>% data.frame(),
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = NA, col = "black") +
  
      # Plot zoomed extent
      geom_rect(data = get(j) %>% t %>% data.frame(),
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = NA, col = "#F21A00") +

      # Set theme parameters
      theme_void()

    # Assign to object
    assign(paste0(j, "_overview"), overviewMap)

  }

# COMBINE PANELS INTO SINGLE PLOT ------------------------------

  # Extract legend from one panel
  legend <- get_legend(extentPyrenees_panel)

  # Add plots together
  combinedPlot <- ggdraw() +
    draw_plot(extentBrittany_panel + theme(legend.position="none"),
              0, 0, 0.5, 1) +
    draw_plot(extentPyrenees_panel + theme(legend.position="none"),
              0.5, 0, 0.5, 1) +
    draw_plot(extentBrittany_overview, 0, 0.05, 0.25, 0.25) +
    draw_plot(extentPyrenees_overview, 0.8, 0.05, 0.25, 0.25) +
    draw_label("(a)", 0.015, 0.96, size = 22) +
    draw_label("(b)", 0.525, 0.96, size = 22) +
    theme(plot.background = element_rect( fill = "white", colour = "white"))
  
  # Add legend
  combinedPlot <- plot_grid(combinedPlot,
                            legend,
                            rel_widths = c(3, .5))

  # Save
  ggsave(filename = paste0("../Plots/", i, "/BVW_records_similarity_zoom.png"),
         combinedPlot,
         dpi = 600,
         units = "px", width = 8000, height = 3000)
}
