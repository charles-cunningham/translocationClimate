# HEADER -------------------------------------------------
#
# Author: Charles Cunningham
# Email: charles.cunningham@york.ac.uk
# 
# Script Name: Extract primary land cover type for each Scottish SSSI
#
# Script Description: Using the 2021 10m land cover map, extract cell 
# cover within each SSSI (some SSSIs consist of multiple polygons) 
# and then calculate percentage cover and primary cover class.

# N.B. 
# - Exclude marine SSSIs
# - Include NA class, i.e. where there is no LCM data, typically 
# marine or very small offshore islands

### CODE --------------------------------------------------

# LOAD LIBRARIES & INSTALL PACKAGES ----------------------

# Change  library to C: (R: doesn't have enough space for packages):
.libPaths("R:/rsrch/cb751/lab/Charles/R/PackageLibrary")

# Load packages
library(terra)
library(tidyverse)

# READ DATA ---------------------------------------------

# Spatial data folder
dirData<- "../Data/"

# 2021 Land Cover Map
LCM2021 <- paste0(dirData, "LCM/gblcm10m2021.tif") %>%
  rast(.) %>%
  .[[1]]

# Rename LCM spatRast
names(LCM2021) <- "Identifier"

# Scottish SSSI
scotSSSI <- paste0(dirData, "SSSI/Scotland/SSSI_SCOTLAND.shp") %>%
  vect(.)

# Specify the LCM classes, add on "No Data"
classLCM <- c(
  "Deciduous woodland",
  "Coniferous woodland",
  "Arable",
  "Improved grassland",
  "Neutral grassland",
  "Calcareous grassland",
  "Acid grassland",
  "Fen",
  "Heather",
  "Heather grassland",
  "Bog",
  "Inland rock",
  "Saltwater",
  "Freshwater",
  "Supralittoral rock",
  "Supralittoral sediment",
  "Littoral rock",
  "Littoral sediment",
  "Saltmarsh",
  "Urban",
  "Suburban",
  "No data" # This is added (not in original LCM Classes)
)

# CREATE SSSI DATAFRAME ------------------------------------

# Create data frame from SSSI vect
scotSSSI_df <- as.data.frame(scotSSSI) %>%
  .[.$GEO_LOC != "MARINE", ]  %>% # Remove marine SSSIs
  unique(.) # Keep unique rows only (some SSSIs have >1 vect object)

# Create LCM identifier/class data frame
# N.B. "No Data" identifier is 'NA'
LCM_df <- data.frame("Identifier" = c(1:(length(classLCM) - 1),
                                      NA),
                     "Class" = classLCM)

# Add LCM columns to SSSI data frame
scotSSSI_df[, classLCM] <- NA

# Find columns that match to LCM classes
colNumsLCM <- names(scotSSSI_df) %in% classLCM %>%
  which(.)

# EXTRACT COVERAGE -----------------------------------------
# N.B. This is main operation of script, runs in approx. 1 hour

# Create progress bar
progressBar = txtProgressBar(min = 0, 
                             max = NROW(scotSSSI_df),
                             initial = 0,
                             style = 3) 

# Start loop iterating through every Scottish SSSI (excluding marine)
for (i in 1:NROW(scotSSSI_df)) {
  
  # Find all SSSI vect objects with PA_code 'i' (part of the same SSSI)
  SSSI_i <-
    scotSSSI[ scotSSSI$PA_CODE == scotSSSI_df$PA_CODE[i], ]
  
  # Extract all 10m cells for each land cover class present for SSSI 'i'
  SSSIcells <- terra::extract(LCM2021, SSSI_i)
  
  # Count number of cells for each class
  # N.B. some classes may not be included as count is 0
  SSSIcount <- count(SSSIcells, Identifier)
  
  # Create percentage cover column from count
  SSSIcount$PercentCover <- SSSIcount$n / sum(SSSIcount$n) * 100
  
  # Add class names by joining coverage vales to LCM data frame
  SSSIcount <- full_join(LCM_df, SSSIcount, by = "Identifier") %>%
    replace_na(list(PercentCover = 0)) # Convert PercentCover NAs to 0
  
  # Add proportion coverage for each class to SSSI data frame
  scotSSSI_df[i, colNumsLCM] <-
    SSSIcount$PercentCover
  
  # Iterate progress bar
  setTxtProgressBar(progressBar, i)
  
}

# Close progress bar
close(progressBar)

# FIND PRIMARY LAND COVER TYPE FOR EACH SSSI ---------------

# Create new column, find maximum cover of different classes, ...
scotSSSI_df$MainCover <- max.col(scotSSSI_df[, colNumsLCM],
                                 ties.method = "random") %>%
  classLCM[.] # ... and assign corresponding column name

# SAVE DATA FRAME -------------------------------------------

# Save to file
saveRDS(scotSSSI_df, 
        file = paste0(dirData, "Processed/ScotSSSI_landcover.Rda"))
