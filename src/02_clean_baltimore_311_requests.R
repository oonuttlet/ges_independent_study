# ========================================================
# Author: Harrison DeFord
# Date: Feb 17, 2026
# Title: 02 - Clean Baltimore 311 Requests
#
# This script takes the raw data returned from script 01 - Get Baltimore 311 Requests.
# It then filters out records that have been marked as duplicate by Baltimore City
# employees, which records have been determined to represent the same event. This 
# cleaning step also handles most records with missing CloseDate values.
#
# It then handles cases where CloseDate is NA by replacing it with StatusDate, which
# reflects the most recent time the CfS's status was updated. 
# ========================================================

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
# FILTER DUPLICATE RECORDS (AS MARKED BY BALTIMORE)
# ========================================================

all_cfs_nodup <- all_cfs_trim_ws |>
  dplyr::filter(SRStatus != "Closed (Duplicate)") |> # Filter out records marked as Duplicate by Baltimore Solid Waste
  dplyr::mutate(
    CloseDate = dplyr::case_when(
      is.na(CloseDate) ~ StatusDate,                 # If CloseDate is NA, then replace with StatusDate
      TRUE ~ CloseDate
    )                                                # Else, if CloseDate is NOT NA, then keep CloseDate
  )

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
  dplyr::filter(!sf::st_is_empty(geom)) |>   # From de-duplicated object, filter rows which have a geometry (saves on memory by avoiding storage)
  dplyr::bind_rows(all_cfs_nodup_empty_bind) # Bind geocoded rows to end of table to ensure that the entire dataset has valid geometry

plot(all_cfs_nodup_valid_geom$geom) # Plot geometry to check shape and CfS locations. Most records should have landed back in Baltimore City

