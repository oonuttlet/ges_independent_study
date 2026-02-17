library(arcgislayers)
library(sf)
library(dplyr)
library(lubridate)
library(tigris)

# ========================================================
# DATA INGESTION AND INITIAL CLEANING
# ========================================================
where_clause <- "SRType IN ('SW-Dirty Alley', 'SW-Dirty Street')" # Passing the WHERE clause to the server reduces client-side processing time

# Get records from 2025, 2024, and 2023, each of which are contained in their own Feature Services
cfs_2025 <- arcgislayers::arc_read(r"(https://services1.arcgis.com/UWYHeuuJISiGmgXx/arcgis/rest/services/311_Customer_Service_Requests_2025/FeatureServer/0)",
                     where = where_clause) |> 
  sf::st_drop_geometry() |> # Incoming geometry does not have a CRS attached, so cannot be reprojected. We will drop and re-create from latitude and longitude
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) |>
  dplyr::mutate(HashedRecord = "") # This column is an unknown type to R, and will raise issues later on

cfs_2024 <- arcgislayers::arc_read(r"(https://services1.arcgis.com/UWYHeuuJISiGmgXx/arcgis/rest/services/311_Customer_Service_Requests_2024/FeatureServer/0)",
                     where = where_clause)
cfs_2023 <- arcgislayers::arc_read(r"(https://services1.arcgis.com/UWYHeuuJISiGmgXx/arcgis/rest/services/311_Customer_Service_Requests_2023/FeatureServer/0)",
                     where = where_clause)

# Get records from 2010-2022, which are layers in the same Feature Service
cfs_2010_2022 <- lapply(0:12, \(x) {yr <- x + 2010 # Create year variable for debug messages
                                    dat <- try(
                                             arcgislayers::arc_read(
                                               paste0(r"(https://services1.arcgis.com/UWYHeuuJISiGmgXx/ArcGIS/rest/services/311_Customer_Service_Requests_Yearly/FeatureServer/)", x),
                                                   where = where_clause)
                                    )
                                    print(paste0(yr, ": ", nrow(dat), " obs"))
                                    return(dat)
                                    }) |> 
  dplyr::bind_rows()                               # Combine these records into one dataframe, which will then be combined with the 2023-2025 DFs

# Combine records 2010-2025 into one dataframe
all_cfs <- dplyr::bind_rows(cfs_2025,
                     cfs_2024,
                     cfs_2023,
                     cfs_2010_2022)

plot(all_cfs$geometry) # Plot geometries, to see if records are valid and in the expected location

# ========================================================
# CHECKS FOR IDs
# ========================================================

sum(duplicated(all_cfs$SRRecordID))        # Check SRRecordID is unique (should return zero if unique)
summary(nchar(all_cfs$SRRecordID))         # See the lengths of SRRecordID, to see if the format changes

unique(                                    # Check to see if the different years matter over time (where is it longer than 9?)
  lubridate::year(all_cfs$CreatedDate[nchar(all_cfs$SRRecordID) > 9])
  ) 
unique(                                    # Check to see if the different years matter over time (where is it equal to 9?)
  lubridate::year(all_cfs$CreatedDate[nchar(all_cfs$SRRecordID) == 9])
) 

sum(duplicated(all_cfs$ServiceRequestNum)) # Check the more useful ServiceRequestNum for uniqueness
summary(nchar(all_cfs$ServiceRequestNum))  # See the lengths of ServiceRequestNum, to see if the format changes

# ========================================================
# GEOGRAPHIC CHECKS
# ========================================================

md_counties <- tigris::counties(state = "MD", # Get all counties in Maryland
                                    cb = FALSE,
                                    year = 2020) |>
  sf::st_transform(4326) # Project county boundaries to match CfS records

balt_boundaries <- md_counties[md_counties$GEOID == '24510',] # Filter counties to only Baltimore City (GEOID 24510)

cfs_balt_int <- all_cfs[balt_boundaries,]       # Filter CfS to fall within Baltimore City boundaries
cfs_balt_disj <- setdiff(all_cfs, cfs_balt_int) # Store the points which fall *outside* City limits
nrow(cfs_balt_disj)                             # Count how many points fall outside City limits
nrow(cfs_balt_disj) / nrow(all_cfs)             # Percentage of points outside City limits


# ========================================================
# DATETIME CHECKS
# ========================================================

sum(is.na(all_cfs$CreatedDate))                    # Count NA values in CreatedDate
sum(is.na(all_cfs$CreatedDate)) / nrow(all_cfs)    # Proportion of NA values in CreatedDate
sum(is.na(all_cfs$CloseDate))                      # Count NA values in CloseDate
sum(is.na(all_cfs$CloseDate)) / nrow(all_cfs)      # Proportion of NA values in CloseDate
sum(is.na(all_cfs$StatusDate))                     # Count NA values in StatusDate
sum(is.na(all_cfs$StatusDate)) / nrow(all_cfs)     # Proportion of NA values in StatusDate

table(all_cfs$SRStatus[is.na(all_cfs$CloseDate)])  # Table of SRStatus values when CloseDate is NA
table(all_cfs$SRStatus[!is.na(all_cfs$CloseDate)]) # Table of SRStatus values when CloseDate is not NA

# ========================================================
# SRStatus CHECKS
# ========================================================

nrow(all_cfs[all_cfs$SRStatus != 'Closed (Duplicate)',]) # Count how many records remain after filtering all Closed (Duplicate) records

# ========================================================
# CREATE EXPORT FOR CLEANING
# ========================================================

all_cfs_trim <- all_cfs |> # Select only the fields necessary for analysis
  dplyr::select(
    SRRecordID,
    ServiceRequestNum,
    SRType,
    CreatedDate,
    SRStatus,
    CloseDate,
    StatusDate,
    Agency,
    Outcome,
    Address,
    Latitude,
    Longitude,
    geometry
  )

sf::st_write(obj = all_cfs_trim, # Write CfS data to the /data folder
         dsn = "data/cfs_baci_2010_2025.gpkg",
         layer = "all_ds_da_cfs_baci_2010_2025")

sf::st_write(obj = md_counties, # Write county data to the /data folder
         dsn = "data/cfs_baci_2010_2025.gpkg",
         layer = "md_cnty_500k_2020")
