# ========================================================
# Author: Harrison DeFord
# Date: Mar 7, 2026
# Title: 05 - Create Isochrone Units Using r5r
#
# Description: This script will create walkshed isochrones using the r5r package.
# This package requires advanced setup, including an installation of a Java SDK and 
# adjustment of Java memory parameters. The source data is an OSM extract from BBBike.org
# of the Baltimore area, extracted on March 7, 2026. Specific parameters are below:
#
# OSM extract downloaded from BBBike.org on March 7, 2026
# Name: Baltimore_Area_PBF_CfS_Isochrones
# Coordinates: -76.782,39.175 x -76.442,39.432
# Script URL: https://extract.bbbike.org/?sw_lng=-76.782&sw_lat=39.175&ne_lng=-76.442&ne_lat=39.432&format=osm.pbf&coords=-76.776%2C39.178%7C-76.604%2C39.175%7C-76.442%2C39.175%7C-76.442%2C39.431%7C-76.664%2C39.432%7C-76.782%2C39.431&city=Baltimore_Area_PBF_CfS_Isochrones&lang=en
# Square kilometre: 837
# Granularity: 100 (1.1 cm)
# Format: osm.pbf
# File size: 41.4 MB
# MD5 checksum: da14b9880813c591df4bdca1494e7106
# Last planet.osm database update: 2026-03-06T23:00:00Z
# License: OpenStreetMap License
#
# The script first sets Java parameters to ensure that there is a Java SDK installed
# and Java has enough memory for computation. It then defines the path in which the
# BBBike OSM extract is stored, and builds a linear network from it. The center-of-mass
# points created in script 04 are read as the origins for the isochrones.
#
# 15-minute isochrones are then calculated for each of the 279 NSA centers to define
# the "walkable distance" from the neighborhood core. The isochrone geometry is then 
# combined with the tabular data from the center-of-mass points to create one polygon
# object, which is then written to the novel spatial units geopackage.
# ========================================================

# ========================================================
# INSTALL AND LOAD LIBRARIES
# ========================================================

package_missing <- setdiff(c("r5r", "rJavaEnv", "rJava", "sf"), installed.packages()) # Check for any missing required libraries

library(r5r)
library(rJavaEnv)
library(rJava)
library(sf)

# ========================================================
# SET JAVA OPTIONS
# ========================================================

rJavaEnv::java_quick_install(21)    # Will only download if not found and available to rJava
options(java.parameters = '-Xmx2G') # Increase Java memory allocation

# ========================================================
# BUILD NETWORK AND COMPUTE ISOCHRONES
# ========================================================

pbf_path <- "data/r5r/" # Contains .pbf file extracted from BBBike.org

nsa_geom_com <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units.gpkg", # Read center-of-mass points from geopackage
                            layer = "baci_nsa_geom_center_of_mass") |>
  sf::st_transform(4326) |>                                              # Project to EPSG:4326 (WGS 1984) for compatibility with r5r
  dplyr::mutate(id = dplyr::row_number())                                # Create ID column for compatibility with r5r

baci_network <- r5r::build_network(data_path = pbf_path) # Build network using data in the data/r5r folder

nsa_geom_com_isochrone <- r5r::isochrone(r5r_network = baci_network, # Create isochrones using network created previously
                                         origins = nsa_geom_com,     # Origins are center-of-mass points
                                         mode = "walk",              # Walking distance (not transit or driving)
                                         cutoffs = 15)               # 15-minute cutoff, at a default speed of 3.6kph (2.24mph)

names(nsa_geom_com_isochrone) # Print names to see what the output object contains

nsa_geom_com_isochrone_wdata <- nsa_geom_com |>
  dplyr::mutate(geom = nsa_geom_com_isochrone$polygons) |> # Overwrite geom column with isochrone polygons
  dplyr::select(-id) |>                                    # Drop ID column, it is now unnecessary
  sf::st_transform(2248)                                   # Project back to EPSG:2248 for continuity with other units and CfS

# ========================================================
# WRITE DATA TO GEOPACKAGE
# ========================================================

sf::st_write(nsa_geom_com_isochrone_wdata, # Write isochrones out to novel units geopackage
             dsn = "data/baci_nsa_2020_novel_units.gpkg",
             layer = "baci_nsa_center_of_mass_isochrone",
             append = FALSE)

