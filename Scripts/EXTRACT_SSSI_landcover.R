# HEADER -------------------------------------------------
#
# Author: Charles Cunningham
# Email: charles.cunningham@york.ac.uk
#
# Script Name: Extract primary land cover type for each GB SSSI
#
# Script Description: Using the 2021 25m land cover map, extract cell
# cover within each SSSI (some SSSIs consist of multiple polygons)
# and then calculate percentage cover and primary cover class.

# N.B.
# - Include NA class, i.e. where there is no LCM data, typically
# marine or very small offshore islands

### CODE --------------------------------------------------

# LOAD LIBRARIES & INSTALL PACKAGES ----------------------

# Change library to R: (C: doesn't have enough space for packages)
.libPaths("R:/rsrch/cb751/lab/Charles/R/PackageLibrary")

# Load packages
library(terra)
library(tidyverse)

# READ DATA ---------------------------------------------

# Spatial data folder
dirData <- "../Data/"

### Land Cover Map

# 2021 25m Land Cover Map
# Available: https://www.ceh.ac.uk/data/ukceh-land-cover-maps
LCM2021 <- paste0(dirData, "LCM/gblcm25m2021.tif") %>%
  rast(.) %>%
  .[[1]] # First band is land cover class

# Rename LCM spatRast
names(LCM2021) <- "Identifier"

### SSSI

# English SSSI
# Available: https://naturalengland-defra.opendata.arcgis.com/
engSSSI <- paste0(dirData,
                  "SSSI/England/Sites_of_Special_Scientific_Interest_(England)___Natural_England.shp") %>%
  vect(.)

# Scottish SSSI
# Available: https://www.data.gov.uk/dataset/d64bf689-4ce8-465b-b00e-6a57dec94a22/site-of-special-scientific-interest-scotland
scotSSSI <- paste0(dirData,
                   "SSSI/Scotland/SSSI_SCOTLAND.shp") %>%
  vect(.)

# Welsh SSSI
# Available: http://lle.gov.wales/catalogue/item/ProtectedSitesSitesOfSpecialScientificInterest/?lang=en
walesSSSI <- paste0(dirData,
                    "SSSI/Wales/NRW_DS98776_SSSI.shp") %>%
  vect(.)

# List of all three national SSSI object strings
allSSSI <- tibble( Nation =     c("England", "Scotland", "Wales"),
                   SSSIobject = c("engSSSI", "scotSSSI", "walesSSSI"),
                   UniqueID =   c("GID",     "PA_CODE",  "SSSI_ID"))

# DEFINE LCM CLASSES ---------------------------------------

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

# Create LCM identifier/class data frame
# N.B. "No Data" identifier is 'NA'
LCM_df <- data.frame("Identifier" = c(1:(length(classLCM) - 1),
                                      NA),
                     "Class" = classLCM)

# CREATE SSSI DATAFRAME ------------------------------------

# Start loop to include nations here
for (i in 1: NROW(allSSSI)) {
  
  # Get variables for nation i
  nation <- allSSSI[[i, "Nation"]]
  SSSI <- get(allSSSI[[i, "SSSIobject"]])
  uniqueID <- allSSSI[[i, "UniqueID"]]
  
  # Create data frame from SSSI object
  # N.B. row = entire SSSI, not individual SSSI polygon
  SSSI_df <- as.data.frame(SSSI) %>% # Create data frame
    unique(.) # Keep unique rows only (some SSSIs have >1 polygon)
  
  # Add LCM columns to SSSI data frame
  SSSI_df[, classLCM] <- NA
  
  # Find columns that match to LCM classes
  colNumsLCM <- names(SSSI_df) %in% classLCM %>%
    which(.)
  
  # EXTRACT COVERAGE -----------------------------------------
  # N.B. This is main operation of script, runs in approx. 2 hours
  
  # Print nation currently processing
  print(nation)
  
  # Create progress bar
  progressBar = txtProgressBar( min = 0,
                                max = NROW(SSSI_df),
                                initial = 0,
                                style = 3)
  
  # Start loop iterating through every SSSI
  for (j in 1:NROW(SSSI_df)) {
    
    # Find all SSSI vect objects with same unique ID (part of SSSI 'j')
    SSSI_j <- subset(SSSI,
                     subset = values(SSSI)[, uniqueID] == SSSI_df[j, uniqueID])
    
    # Extract all 25m cells for each land cover class present for SSSI 'i'
    SSSIcells <- terra::extract(LCM2021, SSSI_j)
    
    # Count number of cells for each class
    # N.B. some classes may not be included as count is 0
    SSSIcount <- count(SSSIcells, Identifier)
    
    # Create percentage cover column from count
    SSSIcount$PercentCover <- SSSIcount$n / sum(SSSIcount$n) * 100
    
    # Add class names by joining coverage values to LCM data frame
    SSSIcount <- full_join(LCM_df, SSSIcount, by = "Identifier") %>%
      replace_na(list(PercentCover = 0)) # Convert PercentCover NAs to 0
    
    # Add proportion coverage for each class to SSSI data frame (row j)
    SSSI_df[j, colNumsLCM] <-
      SSSIcount$PercentCover
    
    # Iterate progress bar
    setTxtProgressBar(progressBar, j)
    
  }
  
  # Close progress bar
  close(progressBar)
  
  # FIND PRIMARY LAND COVER TYPE FOR EACH SSSI ---------------
  
  # Create new column, find maximum cover of different classes, ...
  SSSI_df$MainCover <- max.col(SSSI_df[, colNumsLCM],
                               ties.method = "random") %>%
    classLCM[.] # ... and assign corresponding column name
  
  # SAVE DATA FRAME -------------------------------------------
  
  # Save to file
  saveRDS(SSSI_df,
          file = paste0(dirData, "Processed/",
                        allSSSI[[i, "Nation"]],
                        "_landcover.rds"))
  
}
