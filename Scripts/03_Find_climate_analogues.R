# HEADER ---------------------------------------------------------------
#
# Author: Charles Cunningham
# Email: charles.cunningham@york.ac.uk
# 
# Script name: Find climate analogues for potential UK release sites
#
# Script Description: Use release point locations in south England to 
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

#Set facet labels
facetLabeller <- data.frame(
  climateVariable = c("growingDegreeDays",
                      "MTCO",
                      "tasCV",
                      "totalRain"),
  label = c("Growing degree days similarity to\npotential UK translocation sites",
            "Mean temperature of coldest month (K) similarity to\npotential UK translocation sites",
            "Temperature seasonality similarity to\npotential UK translocation sites",
            "Total annual precipitation (mm) similarity to\npotential UK translocation sites"))
  
# EXTRACT CLIMATE NEAR REINTRODUCTION SITES --------------------

# Combine climate covariates into single spatRaster
climCovar <- c(GDD5_R, MTCO_R, tasCV_R, RAIN_R)

# Extract climate values
siteClimate <- terra::extract(climCovar, sites, ID = FALSE)

# FIND MOST SIMILAR CELLS FOR EACH CLIMATE VARIABLE ------------

# Find mean of reintroduction sites
siteClimateMean <- colMeans(siteClimate)

# Calculate difference between sites and all other European climate values
diffR <- abs(climCovar - siteClimateMean)

# Find quantile limits for each quantile, and each climate variable, and print
quantLimits <- global(diffR, quantile, probs = quants, na.rm = TRUE); print(quantLimits)

# Find cells within quantile limit for each climate variable, for each quantile
climQuants <- apply(quantLimits,
                    MARGIN = 2, # For each column (quantile)...
                    FUN = function(x) { 

  # ... assign a TRUE value if cell is within quantile limit of climate difference
  quantR <- diffR < x;  return(quantR)
})

# Create single raster stack of different quantiles
climQuantsMerge <- lapply(1:nlyr(climCovar), function(x) { # For each climate variable...
  
  # ...extract values from every quantile raster
  allQuantR <- lapply(climQuants, function(r) { r[[x]] }) %>%
    
    # Convert from list to spatRaster then sum together
    rast %>%
    sum
  
  # Set name, and return
  names(allQuantR) <- names(climCovar)[[x]]; return(allQuantR)
  
}) %>%
  # Then combine each climate spatRaster together
  rast

###  PLOT INDIVIDUAL CLIMATE VARIABLES -------------------------

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
  
  # Land cover raster
  geom_tile( aes(x = x, y = y,
                 colour = factor(value), fill = factor(value))) +
  facet_wrap(~ climateVariable) +
  
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
  
  # Add facet labels
  geom_text(data = facetLabeller,
            x = 2, y = 72,
            aes(label = label),
            size  = 4) +
  
  # Set theme parameters
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white"),
        legend.position = c(0.5,-0.02),
        legend.title = element_text(size=18),
        legend.text = element_text(size=18),
        legend.direction = "horizontal",
        strip.text = element_blank(),
        plot.margin = margin(0,-1,2,-1, "lines"))

# Save
ggsave(filename = paste0("../Plots/", "Climate_similarity.png"),
       climQuantMap,
       dpi = 600,
       units = "px", width = 8000, height = 7000)

# Remove redundant objects and clear memory
rm(climQuantsMerge_df)
gc()

# ASSESS CLIMATE VARIABLES TOGETHER -----------------------------

# Find cells which are within a given quantile for all climate covariates
allQuants <- min(climQuantsMerge)

# Plot
allQuantMap <- ggplot(data = as.data.frame(allQuants, xy = TRUE) %>%
                        na.omit()) +
  
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
  annotate(geom="text", 
           x=-5, y=70, 
           label = "Overall climate similarity to\npotential UK translocation sites", 
           size = 10) +
  
  # Set theme parameters
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white"),
        legend.position = c(0.15,0.5),
        legend.title = element_text(size=24),
        legend.text = element_text(size=24))

# Save
ggsave(filename = paste0("../Plots/", "Climate_similarity_all.png"),
       allQuantMap,
       dpi = 600,
       units = "px", width = 8000, height = 7000)

# ASSESS CLIMATE VARIABLES TOGETHER (WITHOUT RAINFALL) ----------------------

# Find cells which are within a given quantile for all climate covariates
tempQuants <- subset(climQuantsMerge,
                     "totalRain",
                     negate = TRUE) %>%
  min

# Plot
tempQuantMap <- ggplot(data = as.data.frame(tempQuants, xy = TRUE) %>% 
                         na.omit()) +
  
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
  annotate(geom="text", 
           x=-5, y=70, 
           label = "Overall climate similarity to\npotential UK translocation sites", 
           size = 10) +
  
  # Set theme parameters
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white"),
        legend.position = c(0.15,0.5),
        legend.title = element_text(size=24),
        legend.text = element_text(size=24))

# Save
ggsave(filename = paste0("../Plots/", "Climate_similarity_temp.png"),
       tempQuantMap,
       dpi = 600,
       units = "px", width = 8000, height = 7000)

# SAVE SPATRASTERS --------------------------------------------------

### Separate climate variables

# Save
writeRaster(climQuantsMerge,
            paste0("../Data/ProcessedData/climQuantsMerge.tif"),
            overwrite = TRUE)

### All climate variables

# Save
writeRaster(allQuants,
            paste0("../Data/ProcessedData/climQuantsAll.tif"),
            overwrite = TRUE)