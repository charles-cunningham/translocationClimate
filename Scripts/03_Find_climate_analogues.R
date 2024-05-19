# HEADER ---------------------------------------------------------------
#
# Author: Charles Cunningham
# Email: charles.cunningham@york.ac.uk
# 
# Script name: Find climate analogues for potential UK release sites
#
# Script Description: Use possible release sites to 
# find suitable climate analogues in continental Europe.

# LOAD LIBRARIES & INSTALL PACKAGES ---------------------

# Change  library to R: (not enough space on C:)
.libPaths("R:/rsrch/cb751/lab/Charles/R/PackageLibrary")

# Load packages
library(terra)
library(sf)
library(tidyverse)

# Set fraction of RAM that may be used by the program
terraOptions(memfrac = 0.9)

# LOAD SPATIAL AND CLIMATE DATA -----------------------

# Climate data
GDD5_R <- rast("../Data/ProcessedData/GDD5.tif")
MTCO_R <- rast("../Data/ProcessedData/MTCO.tif")
tasCV_R <- rast("../Data/ProcessedData/tasCV.tif")
RAIN_R <- rast("../Data/ProcessedData/RAIN.tif")

# Land map
land <- vect("../Data/land.shp")

# Reintroduction locations at different sites
locations <- read.csv("../Data/RawData/possible_locations.csv") %>%
  vect(.,
       geom = c("Longitude", "Latitude"),
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
  climateVariable = c("growingDegreeDays",
                      "MTCO",
                      "tasCV",
                      "totalRain"),
  label = c("Growing degree days\nabove 5°C similarity",
            "Mean temperature of coldest\nmonth similarity",
            "Temperature seasonality\nsimilarity",
            "Total annual precipitation\nsimilarity"))

# Set index labels
indexLabeller <- data.frame(
  climateVariable = c("growingDegreeDays",
                      "MTCO",
                      "tasCV",
                      "totalRain"),
  label = c("(a)", "(b)", "(c)", "(d)"))

# EXTRACT CLIMATE NEAR POTENTIAL REINTRODUCTION SITES --------------------

# Combine climate covariates into single spatRaster
climCovar <- c(GDD5_R, MTCO_R, tasCV_R, RAIN_R)

# Extract climate values
locationClimate <- terra::extract(climCovar, locations, bind = TRUE)

# LOOP THROUGH SITES -------------------------------------------

# Get unique sites
siteNames <- unique(locations$Site)

# Create directory for plots for each site
lapply(siteNames, function(x) {
  paste0(paste0("../Plots/", x)) %>%
  dir.create
})

# Start loop through different sites here, loop continues to end of script
for (i in siteNames) {
  
  # Subset climate values to site i
  siteClimate_i <- terra::subset(locationClimate, locationClimate$Site == i)

# FIND MOST SIMILAR CELLS FOR EACH CLIMATE VARIABLE -------------------

  # Select climate columns and then find mean of reintroduction site climate
  siteClimateMean <- siteClimate_i[, names(climCovar)] %>%
    as.data.frame %>%
    colMeans

  # Calculate difference between site cliamte means and all other European climate values
  diffR <- abs(climCovar - siteClimateMean)
  
  # Find quantile limits for each quantile, and each climate variable, and print
  quantLimits <- global(diffR, quantile, probs = quants, na.rm = TRUE)
  print(quantLimits)

  # Find cells within quantile limit for each climate variable, for each quantile
  climQuants <- apply(quantLimits,
                      MARGIN = 2, # For each column (quantile)...
                      FUN = function(x) { 
                        
    # ... assign a TRUE value if cell is within quantile limit of climate difference
    quantR <- diffR < x;  return(quantR)
    }
    )

  # Create single raster stack of different quantiles
  climQuantsMerge <- lapply(1:nlyr(climCovar), function(x) { # For each climate variable...
    
    # ...extract values from every quantile raster
    allQuantR <- lapply(climQuants,
                        function(r) { r[[x]] }) %>%
      
      # Convert from list to spatRaster then sum together
      rast %>%
      sum
    
    # Set name, and return
    names(allQuantR) <- names(climCovar)[[x]]
    return(allQuantR)
    
  }) %>%
    
    # Then combine each climate spatRaster together
    rast
  
  # Remove redundant objects and clear memory
  rm(diffR, climQuants)
  gc()

#  PLOT INDIVIDUAL CLIMATE VARIABLES -------------------------

  # Create data frame from climQuantsMerge for plot
  climQuantsMerge_df <- as.data.frame(climQuantsMerge, xy = TRUE) %>% 
    na.omit() %>%
    pivot_longer(!c(x,y),
                 names_to = "climateVariable",
                 values_to = "value")

  # Plot
  climQuantMap <- ggplot(data = climQuantsMerge_df) +
    
    # Set equal coordinates
    coord_equal() +
    
    # Climate raster
    geom_tile( aes(x = x, y = y,
                 colour = factor(value), fill = factor(value))) +
    facet_wrap(~ climateVariable) +
    
    # Set colour
    scale_colour_manual("Climate similarity quantile",
                        values = plotColours,
                        labels = plotLabels,
                        breaks = plotBreaks,
                        aesthetics = c("colour", "fill")) +
  
    # Country boundary polygons and site (only site i)
    geom_sf(data = st_as_sf(land),
            fill = NA,
            colour = "black",
            inherit.aes = FALSE ) +
    
    geom_sf(data = st_as_sf(siteClimate_i),
            colour = "#D91630",
            inherit.aes = FALSE ) +
    
    # Add title and index labels
    geom_text(data = titleLabeller,
              x = 0, y = 71,
              aes(label = label),
              size  = 5) +
    geom_text(data = indexLabeller,
              x = -28, y = 73.3,
              aes(label = label),
              size  = 6,
              fontface = "bold") +
    
    # Set theme parameters
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", colour = "white"),
      legend.position = c(0.5, -0.02),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.direction = "horizontal",
      strip.text = element_blank(),
      plot.margin = margin(0, -1, 2, -1, "lines"))
  
  # Save
  ggsave(
    filename = paste0("../Plots/", i,  "/Similarity_quantiles.png"),
    climQuantMap,
    dpi = 600,
    units = "px",
    width = 8000,
    height = 7000)
  
# SAVE SPATRASTERS --------------------------------------------------
  
  # Save separate climate variables
  writeRaster(climQuantsMerge,
              paste0("../Data/ProcessedData/climQuantsMerge_", i, ".tif"),
              overwrite = TRUE)
  
  # Remove redundant objects and clear memory
  rm(climQuantsMerge, climQuantsMerge_df, climQuantMap)
  gc()
  
}
