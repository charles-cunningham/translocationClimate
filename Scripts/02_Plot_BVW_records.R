# HEADER ---------------------------------------------------------------
#
# Author: Charles Cunningham
# Email: charles.cunningham@york.ac.uk
# 
# Script name: Plot black-veined white (BVW) records
#
# Script Description: Process and plot black-veined white records for:
# i.   2014-2023 inclusive
# ii.  pre-1925 records for Britain only
# iii. both 2014-2023 inclusive and pre-1925 records for Britain only

# LOAD LIBRARIES & INSTALL PACKAGES ---------------------

# Change  library to R: (not enough space on C:)
.libPaths("R:/rsrch/cb751/lab/Charles/R/PackageLibrary")

# Load packages
library(terra)
library(sf)
library(tidyverse)
library(CoordinateCleaner) 

# N.B. CoordinateCleaner installed with:
# library(devtools)
# install_github("ropensci/CoordinateCleaner")

# Set fraction of RAM that may be used by the program
terraOptions(memfrac = 0.9)

# LOAD SPATIAL AND CLIMATE DATA -----------------------

# Land map
land <- vect("../Data/land.shp")

# Species records
BVW_df <- read.table("../Data/RawData/SpeciesData/occurrence.txt",
                          sep="\t",
                          fill=TRUE,
                          header=TRUE,
                          quote="") 

# Reintroduction locations at different sites
locations <- read.csv("../Data/RawData/possible_locations.csv") %>%
  vect(.,
       geom = c("Longitude", "Latitude"),
       crs = "EPSG:4326")

# PROCESS DATA -------------------------------------------

### INITIAL CHECKS

# Remove records without coordinates, and with geospatial issues 
BVW_df <- BVW_df %>%
  filter(hasCoordinate == "true") %>%
  filter(hasGeospatialIssues == "false")

# Remove records with zero counts
BVW_df <- BVW_df %>%
  filter(individualCount > 0 |
           is.na(individualCount))

# Remove records with low coordinate precision
BVW_df <- BVW_df %>%
  filter(coordinateUncertaintyInMeters < 10000 | # i.e. 10km
           is.na(coordinateUncertaintyInMeters))

# Filter out doubtful records based on verification status
 BVW_df <- BVW_df %>%
   filter(identificationVerificationStatus != "Unvalidated" |
            identificationVerificationStatus != "Non réalisable" |
            identificationVerificationStatus != "Douteux" |
            identificationVerificationStatus != "Not able to validate" |
            identificationVerificationStatus != "Needs IDs" |
            identificationVerificationStatus != "Unconfirmed - not reviewed")

### COORDINATE CLEANER

# Flag additional issues using coordinate cleaner
flags <- clean_coordinates(x = BVW_df,
                           lon = "decimalLongitude", 
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           # Drop land test as we mask to land later anyway
                           tests = c("capitals", "centroids", "equal", "gbif",
                                     "institutions", "outliers", "zeros"))

#Exclude problematic records
BVW_df <- BVW_df[flags$.summary,]

### FILTER POST-2000 GB RECORDS FOR PLOT (CONFIRMED LOCALLY EXTINCT)

# Remove records that are both post-2000 and GBR records
BVW_df <- BVW_df %>%
  filter(level0Gid != "GBR" | 
           year < 2000)

# ADD DIFFERENT RECORD SUBSETS FOR PLOTTING -----------------------------

# N.B. We want to plot two types of records separately that we classify below:
# i.   2014-2023 inclusive
# ii.  pre-1925 records for Britain only

BVW_df <- BVW_df %>%
  # Create plotType column with (i) subset
  mutate(plotType = if_else(year >= 2014 & year < 2024,
         "Current",
         NA)) %>%
  # Add (ii) to plotType column
  mutate(plotType = if_else(level0Gid == "GBR" & year <= 1925,
         "GB_pre-1925",
         plotType))

# CONVERT TO SPATIAL DATA ------------------------------------------

# Convert to spatVector
BVW_vect <- vect(BVW_df, geom = c("decimalLongitude", "decimalLatitude"),
                 crs = "EPSG:4326",
                 keepgeom = TRUE)

# Mask to land
BVW_vect <- mask(BVW_vect, land)

# Only keep single plot data column to save memory for saving
BVW_plot_vect <- BVW_vect[,"plotType"]

# Save for future plotting
writeVector(BVW_plot_vect,
            "../Data/ProcessedData/BVW_records.shp",
            overwrite = TRUE)

# PLOT RECORDS FROM LAST 10 YEARS -----------------------------------

plotCurrent <- BVW_plot_vect %>%
  
  # Filter data to current 
  subset(BVW_plot_vect$plotType == "Current") %>%
  
  # Convert to sf
  st_as_sf(.) %>%
  
  # Plot
  ggplot() +
  
  # Add land cover
  geom_sf(data = st_as_sf(land), colour = "grey80") +
  
  # Add points
  geom_sf() +
  
  # Set aesthetics
  theme_void() +
  theme(plot.background = element_rect( fill = "white", colour = "white"))

# Save
ggsave(filename = paste0("../Plots/", "BVW_records_current.png"),
       plotCurrent,
       dpi = 600,
       units = "px", width = 8000, height = 7000)

# PLOT PRE-1925 RECORDS FOR GB ---------------------------------------

plotGB <- BVW_plot_vect %>%
  
  # Filter data to GB and pre-1925
  subset(BVW_plot_vect$plotType == "GB_pre-1925") %>%
  
  # Convert to sf
  st_as_sf(.) %>%
  
  # Plot
  ggplot() +
  
  # Add land cover
  geom_sf(data = st_as_sf(land), colour = "grey80") +

  # Add points
  geom_sf() +
  
  # Set aesthetics
  theme_void() +
  theme(plot.background = element_rect( fill = "white", colour = "white"))

# Save
ggsave(filename = paste0("../Plots/", "BVW_records_GB.png"),
       plotGB,
       dpi = 600,
       units = "px", width = 8000, height = 7000)

# PLOT RECORDS FROM LAST 10 YEARS & PRE-1925 RECORDS FOR GB, WITH SITES --------

# Get unique sites
siteNames <- unique(locations$Site)

# Create plot
plotAll <- BVW_plot_vect %>%
    
  # Filter data to remove records not in either plot type
  subset(!is.na(BVW_plot_vect$plotType)) %>%
  
  # Convert to sf
  st_as_sf(.) %>%
  
  # Plot
  ggplot() +
  
  # Add land cover
  geom_sf(data = st_as_sf(land), colour = "grey80") +
  
  # Add  points
  geom_sf(aes(colour = plotType)) +
  
  # Set colour
  scale_colour_manual(
    "",
    values = c("#EBCC2A", "#3B9AB2"),
    labels = c("2014-2023 records", "Pre-1925 GB records"),
    breaks = c("Current", "GB_pre-1925"),
    aesthetics = c("colour", "fill")
  ) +
  guides(colour = guide_legend(override.aes = list(size = 10))) +
  
  # Set aesthetics
  theme_void() +
  theme(
    legend.position = c(0.15, 0.4),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 24),
    plot.background = element_rect(fill = "white", colour = "white")
  )

# Start loop through different sites here, with additional plot without sites
for (i in c(siteNames, "noSite")) {
  
  # If plot doesn't include a site, identical to plotAll
  if (i == "noSite") { plotAll_sites <- plotAll }
  
  # If plot includes a site...
  if (i != "noSite") {
    
    # Subset to site i, and only select first location for plot
    site_i <- terra::subset(locations, locations$Site == i)[1]
    
    # Add site to plot (red triangle)
    plotAll_sites <- plotAll +
      geom_sf(
        data = st_as_sf(site_i),
        fill = "#D91630",
        colour = "black",
        size = 3,
        shape = 24,
        inherit.aes = FALSE)
  }
  
  # Save
  ggsave(
    filename = paste0("../Plots/", "BVW_records_GB_and_current_", i, ".png"),
    plotAll_sites,
    dpi = 600,
    units = "px",
    width = 8000,
    height = 7000)
}
