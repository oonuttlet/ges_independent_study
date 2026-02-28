# ========================================================
# Author: Harrison DeFord
# Date: Feb 17, 2026
# Title: 02 - Clean Baltimore 311 Requests
#
# This script takes the raw data returned from script 01 - Get Baltimore 311 Requests.
# First, it removes trailing whitespace from records in Agency, to ensure that all
# values are 'Solid Waste'. It capitalizes Address, amd converts Latitude and Longitude
# to numeric.
#
# It then filters out records that have been marked as duplicate by Baltimore City
# employees, which records have been determined to represent the same event. This 
# cleaning step also handles most records with missing CloseDate values.
#
# It then handles cases where CloseDate is NA by replacing it with StatusDate, which
# reflects the most recent time the CfS's status was updated. 
#
# Next, it geocodes records with missing geometries to ensure that every CfS has a 
# point location. It extracts the Latitude and Longitude values from the geocoded
# location. 
#
# Finally, it performs checks and reports class conformity, missingness numbers,
# counts and proportions of values in non-unique fields, and investigates changes
# in values over time.
# ========================================================

# ========================================================
# INSTALL AND LOAD LIBRARIES
# ========================================================

package_missing <- setdiff(c("sf", "dplyr", "lubridate", "arcgisgeocode", "ggplot2"), installed.packages()) # Check for any missing required libraries

install.packages(package_missing) # Install missing libraries

library(sf) # Simple Features for R
library(dplyr) # A Grammar of Data Manipulation
library(lubridate) # Make Dealing With Dates a Little Easier
library(arcgisgeocode) # A Robust Interface to ArcGIS 'Geocoding Services'

# ========================================================
# READ IN DATA FROM SCRIPT 01
# ========================================================

all_cfs_trim <- sf::st_read(dsn = "data/cfs_baci_2010_2025.gpkg",
                        layer = "all_ds_da_cfs_baci_2010_2025") # This data is created in Script 01, or downloaded from the public download

summary(all_cfs_trim) # Check that data was properly read

# ========================================================
# BASIC STRING CLEANING
# ========================================================

all_cfs_trim_ws <- all_cfs_trim |>
  dplyr::mutate(
    dplyr::across(
      dplyr::where(is.character), # Operate on all fields which are character
      trimws                      # Trim whitespace from both sides of values to ensure no hanging spaces
      ),
    Address = toupper(Address),   # Uppercase addresses to standardize
    dplyr::across(
      c(Latitude, Longitude),     # Operate on Latitude and Longitude
      as.numeric                  # Convert from numeric to character. If no warnings, that means no NAs were introduced.
    )
    )

# ========================================================
# FILTER DUPLICATE RECORDS (AS MARKED BY BALTIMORE) AND CLEAN DATES
# ========================================================

all_cfs_nodup <- all_cfs_trim_ws |>
  dplyr::filter(SRStatus != "Closed (Duplicate)") |>   # Filter out records marked as Duplicate by Baltimore Solid Waste
  dplyr::mutate(
    CloseDate = dplyr::case_when(
      is.na(CloseDate) ~ StatusDate,                   # If CloseDate is NA, then replace with StatusDate
      TRUE ~ CloseDate                                 # Else, if CloseDate is NOT NA, then keep CloseDate
    ),                                                 
    StatusDate = dplyr::case_when(
      StatusDate >= as.Date('2030-01-01') ~ CloseDate, # If StatusDate is too far in the future, then replace with CloseDate
      TRUE ~ StatusDate                                # Else, if StatusDate is reasonable, then keep StatusDate
    )
  )

# ========================================================
# GEOCODE EMPTY GEOMETRIES
# ========================================================

all_cfs_nodup_empty_geom <- all_cfs_nodup |>
  dplyr::filter(st_is_empty(geom)) # Filter only empty geometries, to be geocoded with MD Multirole Locator

md_multirole_locator <- arcgisgeocode::geocode_server(r"(https://mdgeodata.md.gov/imap/rest/services/GeocodeServices/MD_MultiroleLocator/GeocodeServer)") # Create GeocodeServer object from MD Multirole Service

all_cfs_nodup_empty_geocoded <- arcgisgeocode::geocode_addresses(single_line = all_cfs_nodup_empty_geom$Address, # Single-line geocoding with address field
                                                                 geocoder = md_multirole_locator) |> # Use MD Multirole Locator
  sf::st_transform(4326)

all_cfs_nodup_empty_bind <- all_cfs_nodup_empty_geom |>
  dplyr::mutate(geom = st_geometry(all_cfs_nodup_empty_geocoded)) |> # Overwrite (empty) geometry column with geocoded geometry
  dplyr::filter(!sf::st_is_empty(geom)) |>                           # Filter out any geometries which are still empty
  sf::st_as_sf()                                                     # Ensure output is sf object

all_cfs_nodup_valid_geom <- all_cfs_nodup |>
  dplyr::filter(!sf::st_is_empty(geom)) |>                                               # From de-duplicated object, filter rows which have a geometry (saves on memory by avoiding storage)
  dplyr::bind_rows(all_cfs_nodup_empty_bind) |>                                          # Bind geocoded rows to end of table to ensure that the entire dataset has valid geometry
  mutate(Longitude = dplyr::case_when(is.na(Longitude) ~ sf::st_coordinates(geom)[,"X"], # Replace NA Longitude with extracted Longitude from geocoded record
                                      TRUE ~ Longitude),
         Latitude = dplyr::case_when(is.na(Latitude) ~ sf::st_coordinates(geom)[,"Y"],   # Replace NA Latitude with extracted Latitude from geocoded record
                                     TRUE ~ Latitude)
  )

plot(all_cfs_nodup_valid_geom$geom) # Plot geometry to check shape and CfS locations. Most records should have landed back in Baltimore City

# ========================================================
# INVESTIGATION OF CLEANED DATA
# ========================================================

library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
options(scipen = 999) # Suppress scientific notation for plotting

# Investigate distribution of date columns

ggplot2::ggplot(all_cfs_nodup_valid_geom) +   # Distribution of CreatedDate
  ggplot2::geom_bar(aes(x = as.Date(
    lubridate::floor_date(CreatedDate, "quarter")
  )), stat = "count") +
  ggplot2::labs(title = "Distribution of CreatedDate after cleaning", x = "CreatedDate", y = "Count of CfS") +
  ggplot2::scale_x_date(date_breaks = "2 years") +
  ggplot2::theme_minimal()

summary(all_cfs_nodup_valid_geom$CreatedDate) # Summary of CreatedDate

ggplot2::ggplot(all_cfs_nodup_valid_geom) +   # Distribution of CloseDate
  ggplot2::geom_bar(aes(x = as.Date(
    lubridate::floor_date(CloseDate, "quarter")
  )), stat = "count") +   
  ggplot2::labs(title = "Distribution of CloseDate after cleaning", x = "CloseDate", y = "Count of CfS") +
  ggplot2::scale_x_date(date_breaks = "2 years") +
  ggplot2::theme_minimal()

summary(all_cfs_nodup_valid_geom$CloseDate)  # Summary of CloseDate

ggplot2::ggplot(all_cfs_nodup_valid_geom) +  # Distribution of StatusDate
  ggplot2::geom_bar(aes(x = as.Date(
    lubridate::floor_date(StatusDate, "quarter")
  )), stat = "count") +  
  ggplot2::labs(title = "Distribution of StatusDate after cleaning", x = "StatusDate", y = "Count of CfS") +
  ggplot2::scale_x_date(date_breaks = "2 years") +
  ggplot2::theme_minimal()

summary(all_cfs_nodup_valid_geom$StatusDate) # Summary of StatusDate

# Investigate distribution of numeric columns

summary(all_cfs_nodup_valid_geom$Latitude)  # Summary of Latitude (should be around 39 for Maryland)
summary(all_cfs_nodup_valid_geom$Longitude) # Summary of Longitude (should be around -76 for Maryland)

# Investigate distribution of character columns (non-unique)

ggplot2::ggplot(all_cfs_nodup_valid_geom) + 
  ggplot2::geom_bar(aes(x = SRType), stat = "count") # Distribution of SRType (should contain only SW-Dirty Street and SW-Dirty Alley)
summary(all_cfs_nodup_valid_geom$SRType)             # Summary of SRType (since character, mostly just a class check)
sum(is.na(all_cfs_nodup_valid_geom$SRType))          # Count NA values in SRType
table(all_cfs_nodup_valid_geom$SRType); table(all_cfs_nodup_valid_geom$SRType) / nrow(all_cfs_nodup_valid_geom)

ggplot2::ggplot(all_cfs_nodup_valid_geom) + 
  ggplot2::geom_bar(aes(x = SRStatus), stat = "count")   # Distribution of SRStatus (should contain only Closed, Closed (Transferred), and New)
summary(all_cfs_nodup_valid_geom$SRStatus)               # Summary of SRStatus (since character, mostly just a class check)
sum(is.na(all_cfs_nodup_valid_geom$SRStatus))            # Count NA values in SRStatus
table(all_cfs_nodup_valid_geom$SRStatus); table(all_cfs_nodup_valid_geom$SRStatus) / nrow(all_cfs_nodup_valid_geom)

ggplot2::ggplot(all_cfs_nodup_valid_geom) +
  ggplot2::geom_bar(aes(x = lubridate::year(CreatedDate),  # Distribution of Outcome (often null, so not reliable for cleaning)
                        fill = Outcome), stat = "count") +
  ggplot2::labs(title = "Temporal distribution of Outcome",
                x = "Year",
                y = "Count") +
  ggplot2::theme_minimal()
summary(all_cfs_nodup_valid_geom$Outcome)                  # Summary of Outcome (since character, mostly just a class check)
sum(is.na(all_cfs_nodup_valid_geom$Outcome))               # Count NA values in Outcome
table(all_cfs_nodup_valid_geom$Outcome); table(all_cfs_nodup_valid_geom$Outcome) / nrow(all_cfs_nodup_valid_geom)

ggplot2::ggplot(all_cfs_nodup_valid_geom) + 
  ggplot2::geom_bar(aes(x = Agency), stat = "count")  # Distribution of Agency (should always be Solid Waste)
summary(all_cfs_nodup_valid_geom$Agency)              # Summary of Agency (since character, mostly just a class check)
sum(is.na(all_cfs_nodup_valid_geom$Agency))           # Count NA values in Agency
table(all_cfs_nodup_valid_geom$Agency); table(all_cfs_nodup_valid_geom$Agency) / nrow(all_cfs_nodup_valid_geom)

# Check NAs in unique columns

sum(is.na(all_cfs_nodup_valid_geom$SRRecordID))
table(nchar(all_cfs_nodup_valid_geom$SRRecordID))
ggplot2::ggplot(all_cfs_nodup_valid_geom) + 
  ggplot2::geom_bar(aes(x = lubridate::year(CreatedDate), fill = as.factor(nchar(SRRecordID))), stat = "count")

sum(is.na(all_cfs_nodup_valid_geom$ServiceRequestNum))
table(nchar(all_cfs_nodup_valid_geom$ServiceRequestNum))
ggplot2::ggplot(all_cfs_nodup_valid_geom) + 
  ggplot2::geom_bar(aes(x = lubridate::year(CreatedDate), fill = as.factor(nchar(ServiceRequestNum))), stat = "count")

st_write(all_cfs_nodup_valid_geom,
         dsn = "data/cfs_baci_2010_2025.gpkg",
         layer = "cleaned_ds_da_cfs_baci_2010_2025_v1")
