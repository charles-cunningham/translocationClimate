# HEADER ---------------------------------------------------------------
#
# Author: Charles Cunningham
# Email: charles.cunningham@york.ac.uk
# 
# Script name: Calculate climate metrics from downscaled climate data
#
# Script Description: Using the downloaded downscaled ERA5 data, process the .nc files 
# into usable spatRasters. Calculate climatic means from these: 
# total annual precipitation (RAIN), minimum winter 
# temperature (MTCO), growing degree days (GDD5) and seasonality (tasCV). 
# N.B. temperature units in Kelvin (K).

# LOAD LIBRARIES & INSTALL PACKAGES ---------------------------------

# Change  library to R: (not enough space on C:)
.libPaths("R:/rsrch/cb751/lab/Charles/R/PackageLibrary")

# Load packages
library(terra)
library(sf)
library(tidyverse)

# Set fraction of RAM that may be used by the program
terraOptions(memfrac = 0.9)

# SET PARAMETERS AND LIST DIRECTORIES ------------------------------

# Data directory
dataDir <-  "../Data/ClimateData"

# LOAD COUNTY BOUNDARIES -------------------------------------------

# Download countries
land <- geodata::world(resolution=1,
                       path = "../Data")

# Remove all boundaries
land <- aggregate(land)

# DOWNLOAD ENVIRONMENTAL VARIABLES -----------------------------------

### DATASET DETAILS:

### Name: Downscaled bioclimatic indicators for selected regions from 1979 to 2018 derived from reanalysis

### Documentation available here:
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-biodiversity-era5-regional?tab=doc

### Citation:
# Wouters, H., (2021): Downscaled bioclimatic indicators for selected regions from 1979 to 2018 
# derived from reanalysis. Copernicus Climate Change Service (C3S) Climate Data Store (CDS).
# DOI: 10.24381/cds.fe90a594 (Accessed on 30-01-2024)

### FOLLOW INSTRUCTIONS TO DOWNLOAD:

### Use this link:
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-biodiversity-era5-regional?tab=form

### For all select:
# Region: Europe
# Origin: ERA5
# Statistic: Mean
# Version: 1.0
# Format: Zip file(.zip)

### For each variable, select the following parameters and download seperately: 

### Growing Degree Days (GDD5)
# Variable: Vegetation sensitive indicators -> Growing degree days
# Derived variable: Annual sum

### Mean temperature of coldest month (MTCO) [Need to calculate manually]
# Variable: Essential climate variables -> 2m Temperature
# Derived variable: Monthly mean

### Temperature seasonality (tasCV)
# Variable:  Bioclimatic indicators as in WORLDCLIM -> Temperature seasonality (BIO04)

### Annual rainfall (RAIN)
# Variable:  Bioclimatic indicators as in WORLDCLIM -> Annual precipitation (BIO12) 

### Save and unzip to file locations specified in next section

# LOAD ENVIRONMENTAL VARIABLES ------------------------------------

### Growing Degree days (GDD5)
# [sum of daily degrees above daily mean temperature of 278.15 K (5°C)]

GDD5_R <- paste0(dataDir,
                "/growing-degree-days_annual-sum_era5-to-1km_1979-2018-mean_v1.0.nc") %>%
  rast

# Rename
names(GDD5_R) <- "growingDegreeDays"

### Mean temperature of coldest month (MTCO) [units: Kelvin)]

MTCO_R <- paste0(dataDir,
                 "/temperature_monthly-mean_era5-to-1km_1979-2018-mean_v1.0.nc") %>%
  rast %>% # 12 layers (one for each month's mean temperature)
  min # Get minimum month for each cell

# Rename
names(MTCO_R) <- "MTCO"

### Temperature seasonality (tasCV)
# [Standard deviation of the monthly mean temperature multiplied by 100]

tasCV_R <- paste0(dataDir,
                  "/BIO04_era5-to-1km_1979-2018-mean_v1.0.nc") %>%
  rast

# Rename
names(tasCV_R) <- "tasCV"

### Annual precipitation
#[Annual mean of the monthly mean precipitation rate]

RAIN_R <- paste0(dataDir,
                 "/BIO12_era5-to-1km_1979-2018-mean_v1.0.nc") %>%
  rast
 
# Convert RAIN_R from monthly mean precipitation rate to 
# total precipitation sum over the year (mm/year)
RAIN_R <- RAIN_R * 3600*24*365*1000

# Rename
names(RAIN_R) <- "totalRain"

# PROCESS ENVIRONMENTAL VARIABLES -------------------------------

# Specify CRS (WGS 84)
crs(land) <- crs(GDD5_R) <- crs(MTCO_R) <- crs(tasCV_R) <- crs(RAIN_R) <- "EPSG:4326"

# Crop land to climate variable extent
land <- crop(land, GDD5_R)

# Mask to land
GDD5_R <- mask(GDD5_R, land)
MTCO_R <- mask(MTCO_R, land)
tasCV_R <- mask(tasCV_R, land)
RAIN_R <- mask(RAIN_R, land)

# SAVE SPATIAL OBJECTS --------------------------------------------

# Write rasters
writeRaster(GDD5_R,
            paste0("../Data/ProcessedData/GDD5.tif"),
            overwrite = TRUE)
writeRaster(MTCO_R,
            paste0("../Data//ProcessedData/MTCO.tif"),
            overwrite = TRUE)
writeRaster(tasCV_R,
            paste0("../Data//ProcessedData/tasCV.tif"),
            overwrite = TRUE)
writeRaster(RAIN_R,
            paste0("../Data//ProcessedData/RAIN.tif"),
            overwrite = TRUE)

# Write land map
writeVector(land,
            "../Data/land.shp",
            overwrite = TRUE)

# PLOT ----------------------------------------------------------

# Plot GDD5
GDD5map <- ggplot(data = as.data.frame(GDD5_R, xy = TRUE) %>% na.omit()) +
  
  # Set equal coordinates
  coord_equal() +
  
  # Climate raster
  geom_tile( aes(x = x, y = y, colour = growingDegreeDays, fill = growingDegreeDays)) +
  
  # Set colours and legend
  scale_colour_gradient2(midpoint = 4000,
                         low = "#313695",
                         mid = "#fee090",
                         high = "#a50026",
                         guide = NULL) +
  scale_fill_gradient2( "",
                        midpoint = 4000,
                        low = "#313695",
                        mid = "#fee090",
                        high = "#a50026",
                        guide = guide_colourbar(ticks = TRUE,
                                                draw.ulim = FALSE,
                                                draw.llim = FALSE,
                                                title.position = "top",
                                                label.position = "right",
                                                label.theme = element_text(size = 12),
                                                title.theme = element_text(size = 12),
                                                barwidth = unit(2, "lines"),
                                                barheight = unit(8, "lines"))) +
  
  # Country boundary polygon
  geom_sf(data = st_as_sf(land), fill = NA, colour = "black", inherit.aes = FALSE) +
  
  # Add title
  annotate(geom="text", 
           x=0, y=70, 
           label = "Growing degree days\nabove 5\u00B0C\n[1979-2018 mean]", 
           size = 5) +
  
  # Set theme parameters
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white"),
        legend.position = c(0.15,0.5),
        plot.margin = margin(-4,0,-4,0, "lines"))

# Plot MTCO
MTCOmap <- ggplot(data = as.data.frame(MTCO_R, xy = TRUE) %>% na.omit()) +
  
  # Set equal coordinates
  coord_equal() +
  
  # Climate raster
  geom_tile( aes(x = x, y = y, colour = MTCO, fill = MTCO)) +
  
  # Set colours and legend
  scale_colour_gradient2(midpoint = 265,
                         low = "#313695",
                         mid = "#fee090",
                         high = "#a50026",
                         guide = NULL) +
  scale_fill_gradient2( "",
                        midpoint = 265,
                        low = "#313695",
                        mid = "#fee090",
                        high = "#a50026",
                        guide = guide_colourbar(ticks = TRUE,
                                                draw.ulim = FALSE,
                                                draw.llim = FALSE,
                                                title.position = "top",
                                                label.position = "right",
                                                label.theme = element_text(size = 12),
                                                title.theme = element_text(size = 12),
                                                barwidth = unit(2, "lines"),
                                                barheight = unit(8, "lines"))) +
  
  # Country boundary polygon
  geom_sf(data = st_as_sf(land), fill = NA, colour = "black", inherit.aes = FALSE) +
  
  # Add title
  annotate(geom="text", 
           x=0, y=70, 
           label = "Mean temperature\nof coldest month (K)\n[1979-2018 mean]", 
           size = 5) +
  
  # Set theme parameters
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white"),
        legend.position = c(0.15,0.5),
        plot.margin = margin(-4,0,-4,0, "lines"))

# Plot tasCV
tasCVmap <- ggplot(data = as.data.frame(tasCV_R, xy = TRUE) %>% na.omit()) +
  
  # Set equal coordinates
  coord_equal() +
  
  # Climate raster
  geom_tile( aes(x = x, y = y, colour = tasCV, fill = tasCV)) +
  
  # Set colours and legend
  scale_colour_gradient2(midpoint = 1000,
                         low = "#313695",
                         mid = "#fee090",
                         high = "#a50026",
                         guide = NULL) +
  scale_fill_gradient2( "",
                        midpoint = 1000,
                        low = "#313695",
                        mid = "#fee090",
                        high = "#a50026",
                        guide = guide_colourbar(ticks = TRUE,
                                                draw.ulim = FALSE,
                                                draw.llim = FALSE,
                                                title.position = "top",
                                                label.position = "right",
                                                label.theme = element_text(size = 12),
                                                title.theme = element_text(size = 12),
                                                barwidth = unit(2, "lines"),
                                                barheight = unit(8, "lines"))) +
  
  # Country boundary polygon
  geom_sf(data = st_as_sf(land), fill = NA, colour = "black", inherit.aes = FALSE) +
  
  # Add title
  annotate(geom="text", 
           x=0, y=70, 
           label = "Temperature seasonality\n[1979-2018 mean]", 
           size = 5) +
  
  # Set theme parameters
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white"),
        legend.position = c(0.15,0.5),
        plot.margin = margin(-4,0,-4,0, "lines"))


# Plot RAIN
RAINmap <- ggplot(data = as.data.frame(RAIN_R, xy = TRUE) %>% na.omit()) +
  
  # Set equal coordinates
  coord_equal() +
  
  # Climate raster
  geom_tile( aes(x = x, y = y, colour = totalRain, fill = totalRain)) +
  
  # Set colours and legend
  scale_colour_gradient2( "",
                          midpoint = 1500,
                          low = "#a50026",
                          mid = "#fee090",
                          high = "#313695",
                          guide = NULL) +
  scale_fill_gradient2( "",
                        midpoint = 1500,
                        low = "#a50026",
                        mid = "#fee090",
                        high = "#313695",
                        guide = guide_colourbar(ticks = TRUE,
                                                draw.ulim = FALSE,
                                                draw.llim = FALSE,
                                                title.position = "top",
                                                label.position = "right",
                                                label.theme = element_text(size = 12),
                                                title.theme = element_text(size = 12),
                                                barwidth = unit(2, "lines"),
                                                barheight = unit(8, "lines"))) +
  
  # Country boundary polygon
  geom_sf(data = st_as_sf(land), fill = NA, colour = "black", inherit.aes = FALSE) +
  
  # Add title
  annotate(geom="text", 
           x=0, y=70, 
           label="Total annual\nprecipitation (mm)\n[1979-2018 mean]",
           size = 5) +
  
  # Set theme parameters
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white"),
        legend.position = c(0.15,0.5),
        plot.margin = margin(-4,0,-4,0, "lines"))

# Aggregate all climate covariate maps together
allClimateMap <- gridExtra::arrangeGrob(GDD5map, MTCOmap, tasCVmap, RAINmap,
                                        nrow = 2, ncol = 2)

# Save to .png file
ggsave(filename = paste0("../Plots/", "Climate_covariate_means.png"),
       allClimateMap,
       dpi = 600,
       units = "px", width = 8000, height = 7000)
