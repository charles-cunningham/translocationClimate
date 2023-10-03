# HEADER -------------------------------------------------
#
# Author: Charles Cunningham
# Email: charles.cunningham@york.ac.uk
#
# Script Name: Extract landscape and agricultural archetypes for each GB SSSI
#
# Script Description: Using the Goodwin et al. 2022 archetypes
# (https://iopscience.iop.org/article/10.1088/1748-9326/ac810e#erlac810es3)
# extract cover within each SSSI (some SSSIs consist of multiple archetypes)
# and then calculate percentage cover and primary archetype.
# N.B. Most SSSIs will be 100% a single archetype as archetype resolution is 1km

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

### Archetypes
# Available: https://catalogue.ceh.ac.uk/documents/3b44375a-cbe6-468c-9395-41471054d0f3

# Tier 1 archetypes
arch1 <- paste0(dirData, "Archetypes/tier1archetypesgb.tif") %>%
  rast(.) %>%
  .[[1]] # First layer is archetype layer

# Tier 2 archetypes
arch2 <- paste0(dirData, "Archetypes/tier2archetypesgb.tif") %>%
  rast(.) %>%
  .[[1]] # First layer is archetype layer

# Rename archetype layer name
names(arch1) <- names(arch2) <- "Identifier"

### Associated archetype codes

# Read in tier 1 codes
arch1Codes <- paste0(dirData, 
                     "Archetypes/supporting-documents/tier1codes.csv") %>%
  read.csv(.)
  
# Process tier 1 codes
arch1Codes <- dplyr::select(arch1Codes, Archetype_id, R_name_short ) %>% # Only need ID and names
  rename( Identifier = Archetype_id ) %>% # Rename ID column to Identifier (same as SpatRast)
  rbind(., c(NA, "No data")) %>% # Add 'NA / No data row' to end of data frame
  mutate_at( "Identifier", as.numeric ) # Change type to numeric
  
# Read in tier 2 codes
arch2Codes <- paste0(dirData, 
                     "Archetypes/supporting-documents/tier2codes.csv") %>%
  read.csv(.)
  
# Process tier 2 codes
arch2Codes <- dplyr::select(arch2Codes, Archetype_id, R_name_short ) %>% # Only need ID and names
  rename( Identifier = Archetype_id ) %>% # Rename ID column to Identifier (same as SpatRast)
  rbind(., c(NA, "Not agricultural land")) %>% # Add 'NA / Not agricultural land' row to end of data frame
  mutate_at( "Identifier", as.numeric ) # Change type to numeric

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

# CREATE ARCHETYPE DATAFRAMES ------------------------------------

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
  
  ### Create archetype 1 data frame
  
  # Copy SSSI dataframe
  arch1_df <- SSSI_df
  
  # Add LCM columns to SSSI data frame
  arch1_df[, arch1Codes$R_name_short] <- NA
  
  # Find columns that match to LCM classes
  colNumsArch1 <- names(arch1_df) %in% arch1Codes$R_name_short %>%
    which(.)
  
  ### Create archetype 2 data frame
  
  # Copy SSSI dataframe
  arch2_df <- SSSI_df
  
  # Add LCM columns to SSSI data frame
  arch2_df[, arch2Codes$R_name_short] <- NA
  
  # Find columns that match to LCM classes
  colNumsArch2 <- names(arch2_df) %in% arch2Codes$R_name_short %>%
    which(.)

  # EXTRACT COVERAGE -----------------------------------------
  # N.B. This is main operation of script, runs in approx. 1 hour
  
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
    
    # Extract 1km cell coverage for each archetype present for SSSI 'j'
    arch1cells <- terra::extract(arch1, SSSI_j)
    arch2cells <- terra::extract(arch2, SSSI_j)
    
    # Count number of cells for each archetype
    # N.B. most archetypes will not be included as count is 0
    arch1count <- count(arch1cells, Identifier )
    arch2count <- count(arch2cells, Identifier )

    # Create percentage cover column from count
    arch1count$PercentCover <- arch1count$n / sum(arch1count$n) * 100
    arch2count$PercentCover <- arch2count$n / sum(arch2count$n) * 100
    
    # Add archetype names by joining coverage values to archetype data frame
    arch1count <- full_join(arch1Codes, arch1count, by = "Identifier") %>%
      replace_na(list(PercentCover = 0)) # Convert PercentCover NAs to 0
    arch2count <- full_join(arch2Codes, arch2count, by = "Identifier") %>%
      replace_na(list(PercentCover = 0)) # Convert PercentCover NAs to 0
    
    # Add proportion coverage for each archetype to SSSI data frame (row j)
    arch1_df[j, colNumsArch1] <- arch1count$PercentCover
    arch2_df[j, colNumsArch2] <- arch2count$PercentCover
    
    # Iterate progress bar
    setTxtProgressBar(progressBar, j)
    
  }
  
  # Close progress bar
  close(progressBar)
  
  # FIND MAIN ARCHETYPES FOR EACH SSSI ---------------
  
  # Create new column, find maximum cover of different archetypes,
  # and assign corresponding column name
  arch1_df$MainCover <- max.col(arch1_df[, colNumsArch1],
                               ties.method = "random") %>%
    arch1Codes$R_name_short[.]
  
  arch2_df$MainCover <- max.col(arch2_df[, colNumsArch2],
                                ties.method = "random") %>%
    arch2Codes$R_name_short[.]
  
  # SAVE DATA FRAME -------------------------------------------
  
  # Save to file
  saveRDS(arch1_df,
          file = paste0(dirData, "Processed/",
                        allSSSI[[i, "Nation"]],
                        "_Tier1ArcheptypesCover.rds"))
  saveRDS(arch2_df,
          file = paste0(dirData, "Processed/",
                        allSSSI[[i, "Nation"]],
                        "_Tier1ArcheptypesCover.rds"))
  
}
