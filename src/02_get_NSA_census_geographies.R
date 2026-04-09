# ========================================================
# Author: Harrison DeFord
# Date: Mar 2, 2026
# Title: 02 - Get NSA and Geographies
#
# Description: This script will scrape Neighborhood Statistical Area (NSA)
# boundaries from the Open Baltimore feature services to form the basis of "place"
# for aggregation. From the polygonal boundaries, different spatial units will
# be used to aggregate CfS points to build representations of CfS patterns across space.
#
# The script begins by querying the Open Baltimore feature service containing the 2020
# NSA boundaries. It then verifies that the geometries are valid, that IDs are unique, and
# performs basic summary statistics on count, area, etc. These summary statistics will be 
# useful when creating novel areal units (e.g., a grid with cell area equivalent to the 
# median polygon area, or a set number of grid cells based on the total number of 
# geographies).

# To replicate the analysis exactly, this script should not be run, and instead data 
# provided via the public data download should be used. This data was accessed on 
# March 7, 2026. Because records and boundaries can change as Baltimore City and the 
# Census Bureau update data, records fetched at a later date could impact the 
# results of the following scripts. It is mainly provided for transparency in analysis.
# ========================================================

# ========================================================
# INSTALL AND LOAD LIBRARIES
# ========================================================

package_missing <- setdiff(c("arcgislayers", "sf", "dplyr", "units"), installed.packages()) # Check for any missing required libraries

install.packages(package_missing) # Install missing libraries

library(arcgislayers) # Harness ArcGIS Data Services
library(sf)           # Simple Features for R
library(dplyr)        # A Grammar of Data Manipulation
library(units)        # Measurement Units for R Vectors

# ========================================================
# GET NSA POLYGONS
# ========================================================

# Read Baltimore City NSA records from Feature Server
baci_nsa_2020 <- arcgislayers::arc_read(r"(https://geodata.baltimorecity.gov/egis/rest/services/CityView/Neighborhoods/FeatureServer/0)")

nrow(baci_nsa_2020)                    # Count total number of rows in NSA polygons 
length(unique(baci_nsa_2020$OBJECTID)) # Ensure that every OBJECTID is unique (no duplicated records)
length(unique(baci_nsa_2020$Name))     # Ensure that all names are unique (no duplicated records)

sf::st_is_valid(baci_nsa_2020$geometry,           # Check for invalid geometry in NSA areas
                reason = TRUE)                    # Why is the geometry invalid?
baci_nsa_2020 <- sf::st_make_valid(baci_nsa_2020) # Repair invalid geometries using valid structure
sum(!sf::st_is_valid(baci_nsa_2020))              # Ensure that the geometry has been correctly repaired

st_crs(baci_nsa_2020)        # Check CRS. Must match CfS data for spatial operations

baci_nsa_2020_proj <- baci_nsa_2020 |>
  sf::st_transform(2248) # Project data to match the CfS data for geoprocessing: EPSG:2248 NAD83 / Maryland (ftUS)

baci_nsa_2020_proj$Area_sqkm <- as.numeric(units::set_units(sf::st_area(baci_nsa_2020_proj), "km2")) # Compute area directly from geometry, and convert to sq. km

summary(baci_nsa_2020_proj$Area_sqkm) # Find summary statistics of NSA area, in square kilometers
hist(baci_nsa_2020_proj$Area_sqkm)    # Investigate full distribution of NSA area

plot(baci_nsa_2020_proj$geometry) # Plot geometry to ensure that the shape is correct and to check for invalid geometry

baci_nsa_2020_proj <- baci_nsa_2020_proj |>
  dplyr::select(OBJECTID,   # Trim columns to only necessary columns for analysis
                Name,
                Population,
                HH_Total,
                Area_sqkm,
                geometry)

# ========================================================
# INVESTIGATION OF RAW DATA
# ========================================================

# Investigate unique columns

summary(baci_nsa_2020_proj$OBJECTID)         # Summary of OBJECTID: should be min = 1, max = nrow of object
sum(is.na(baci_nsa_2020_proj$OBJECTID))      # Count NA values in OBJECTID
sum(duplicated(baci_nsa_2020_proj$OBJECTID)) # Ensure that there are no duplicated OBJECTID values

summary(baci_nsa_2020_proj$Name)         # Summary of Name (since character, mostly just a class check)
sum(is.na(baci_nsa_2020_proj$Name))      # Count NA values in Name
sum(duplicated(baci_nsa_2020_proj$Name)) # Ensure that there are no duplicated Name values

# Investigate numeric columns

summary(baci_nsa_2020_proj$Population)           # Summary of Population
sum(is.na(baci_nsa_2020_proj$Population))        # Count NA values in Population; this means there is no population and should be replaced with 0
hist(baci_nsa_2020_proj$Population, breaks = 20) # Investigate shape of distribution of Population

summary(baci_nsa_2020_proj$HH_Total)           # Summary of HH_Total
sum(is.na(baci_nsa_2020_proj$HH_Total))        # Count NA values in HH_Total; this means there are no households and should be replaced with 0
hist(baci_nsa_2020_proj$HH_Total, breaks = 20) # Investigate shape of distribution of HH_Total

summary(baci_nsa_2020_proj$Area_sqkm)           # Summary of Area_sqkm
sum(is.na(baci_nsa_2020_proj$Area_sqkm))        # Count NA values in Area_sqkm; this means the area computation failed or the geometry type is incorrect
hist(baci_nsa_2020_proj$Area_sqkm, breaks = 20) # Investigate shape of distribution of Area_sqkm

# Investigate geometry types

summary(baci_nsa_2020_proj$geometry) # Summary of geometry; ensure that all geometry are same type

baci_nsa_2020_proj <- baci_nsa_2020_proj |> 
  sf::st_cast("MULTIPOLYGON") # Cast all geometry to MULTIPOLYGON (some with only one polygon) to ensure the same type

summary(baci_nsa_2020_proj$geometry) # Summary of geometry; ensure that casting to MULTIPOLYGON was successful

# ========================================================
# CLEAN ATTRIBUTE VALUES
# ========================================================

baci_nsa_2020_proj <- baci_nsa_2020_proj |>
  dplyr::mutate(dplyr::across(c(Population, HH_Total), ~replace(., is.na(.), 0))) # Replace NA values with 0 in Population and HH_Total

plot(baci_nsa_2020_proj$Population, baci_nsa_2020_proj$HH_Total) # Investigate if Population and HH_Total are correlated
cor(baci_nsa_2020_proj$Population, baci_nsa_2020_proj$HH_Total)  # Pearson coefficient of correlation b/w Population and HH_Total

sf::st_write(baci_nsa_2020_proj, 
             dsn = "data/cfs_baci_2010_2025.gpkg",
             layer = "cleaned_nsa_bnd_baci_2020_v1",
             append = FALSE)
