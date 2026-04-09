# ========================================================
# Author: Harrison DeFord
# Date: Mar 7, 2026
# Title: 04 - Create Novel Spatial Units
#
# Description: This script will use the NSA polygons gathered and cleaned in
# script 03 as the basis of "place" to create novel spatial units for aggregation.
# The output of this script is six sets of spatial records: a square fishnet grid,
# a hexagonal fishnet grid, geometric centroids of each NSA, point-on-surface points
# of each NSA, 0.5-mile buffered NSA polygons, and symmetrical buffers equivalent
# to the median area of the NSA polygons.
#
# The script begins by reading the cleaned NSA data from script 03. It then
# computes the median polygon area, and creates the two fishnet grids covering
# the study area, and  filters out only those polygons which intersect with
# the NSA boundaries for across aggregation. It then creates the two sets of
# points: one using sf::st_centroid(), which uses the geographic boundaries
# of the polygons (which can lead to points outside of polygons in some cases);
# and one using sf::st_point_in_polygon(), which must be inside the polygon 
# boundaries. These two units will be used for inward aggregation. Finally, the
# script creates the three sets of units used for outwards aggregation: it first 
# buffers the NSA polygons by 0.5 miles using sf::st_buffer(); then it buffers the
# center-of-mass points to create circles with the median area of the set of NSA
# polygons.

#
# After the new spatial units are created, they are written to a new geopackage.
# ========================================================

# ========================================================
# INSTALL AND LOAD LIBRARIES
# ========================================================

package_missing <- setdiff(c("sf", "dplyr", "units"), installed.packages()) # Check for any missing required libraries

install.packages(package_missing) # Install missing libraries

library(sf)           # Simple Features for R
library(dplyr)        # A Grammar of Data Manipulation
library(units)        # Measurement Units for R Vectors

# ========================================================
# READ CLEANED NSA DATA AND GET MEDIAN AREA
# ========================================================

baci_nsa_2020_proj <- sf::st_read(dsn = "data/cfs_baci_2010_2025.gpkg", # Read cleaned data from script 03
                              layer = "cleaned_nsa_bnd_baci_2020_v1")

med_nsa_area <- median(baci_nsa_2020_proj$Area_sqkm) # Store median area as a memory object for use in geometry operations

# ========================================================
# CREATE ACROSS AGGREGATION UNITS (FISHNETS)
# ========================================================

# Square fishnet

nsa_med_area_square_fishnet <- sf::st_make_grid(x = sf::st_buffer(baci_nsa_2020_proj, 1118),        # Equivalent to the half the side length of the square, this should roughly center the grid overhang on both sides of the study area
                                                cellsize = units::as_units(med_nsa_area, "km2")) |> # Create grid cells across the area
  sf::st_as_sf() |>                                                                                 # Convert SFC to SF
  sf::st_filter(baci_nsa_2020_proj) |>                                                              # Filter cells which intersect with NSAs
  dplyr::mutate(grid_id = dplyr::row_number())                                                      # Because these do not have fields from NSAs, create ID for joining


plot(baci_nsa_2020_proj$geom, col = "red")
plot(sf::st_geometry(nsa_med_area_square_fishnet), lwd = 2, add = TRUE) # Plot NSA areas and fishnet grid to ensure geometry is correct

# Hex fishnet

nsa_med_area_hex_fishnet <- sf::st_make_grid(x = sf::st_buffer(baci_nsa_2020_proj, 1387), # Equivalent to the circumcircle radius of the hexagon, this should roughly center the grid overhang on both sides of the study area
                                                cellsize = units::as_units(med_nsa_area, "km2"),
                                             square = FALSE) |>                           # Create grid cells across the area
  sf::st_as_sf() |>                                                                       # Convert SFC to SF
  sf::st_filter(baci_nsa_2020_proj) |>                                                    # Filter cells which intersect with NSAs
  dplyr::mutate(grid_id = dplyr::row_number())                                            # Because these do not have fields from NSAs, create ID for joining

plot(baci_nsa_2020_proj$geom, col = "lightblue")
plot(sf::st_geometry(nsa_med_area_hex_fishnet), lwd = 2, add = TRUE) # Plot NSA areas and fishnet grid to ensure geometry is correct

# ========================================================
# CREATE INWARDS AGGREGATION UNITS (POINTS)
# ========================================================

# Geometric centroid
nsa_geom_cent <- sf::st_centroid(baci_nsa_2020_proj) # Geometric center-of-gravity of NSA polygons

#Center-of-mass
nsa_geom_surface <- sf::st_point_on_surface(baci_nsa_2020_proj) # Point on NSA polygons, guaranteed to be on surface

plot(baci_nsa_2020_proj$geom, border = "black")
plot(nsa_geom_cent$geom, col = "red", pch = 19, cex = 0.5, add = TRUE)
plot(nsa_geom_surface$geom, col = "blue", pch = 19, cex = 0.5, add = TRUE) # Plot center-of-gravity vs. point on surface to see shift in position

# ========================================================
# CREATE OUTWARDS AGGREGATION UNITS
# ========================================================

# Polygonal 0.25mi buffer
nsa_025mi_buffer <- sf::st_buffer(baci_nsa_2020_proj, 1320) # Buffer NSA areas by 0.25mi

example_nsa <- "Hamilton" # Select one NSA as an example for plotting of geometry

plot(nsa_025mi_buffer$geom[nsa_025mi_buffer$Name == example_nsa], border = "red") # Plot new unit, existing NSA, and other NSAs to check output
plot(baci_nsa_2020_proj$geom, border = "#dbdbdb", add = TRUE)
plot(baci_nsa_2020_proj$geom[baci_nsa_2020_proj$Name == example_nsa], border = "black", lwd = 2, add = TRUE)

# Point buffer equivalent to median area
med_nsa_area_equiv_radius <- sqrt((med_nsa_area * 10763910.41671)/pi) # Convert median NSA area to sq. feet and convert area to radius 

nsa_surface_area_buffer <- sf::st_buffer(nsa_geom_surface, med_nsa_area_equiv_radius) # Buffer point-on-surface points by computed radius

plot(nsa_geom_surface$geom, pch = 19, cex = 0.5) # Plot point-on-surface points with buffered radius
plot(nsa_surface_area_buffer$geom, border = "red", add = TRUE)

# ========================================================
# WRITE AGGREGATION UNITS TO GPKG
# ========================================================

sf::st_write(nsa_med_area_square_fishnet, # Write square fishnet to novel units geopackage
             dsn = "data/baci_nsa_2020_novel_units.gpkg",
             layer = "baci_nsa_med_area_square_fishnet",
             append = FALSE)

sf::st_write(nsa_med_area_hex_fishnet, # Write hexagon fishnet to novel units geopackage
             dsn = "data/baci_nsa_2020_novel_units.gpkg",
             layer = "baci_nsa_med_area_hex_fishnet",
             append = FALSE)

sf::st_write(nsa_geom_cent, # Write geometric centroids to novel units geopackage
             dsn = "data/baci_nsa_2020_novel_units.gpkg",
             layer = "baci_nsa_geom_centroid",
             append = FALSE)

sf::st_write(nsa_geom_surface, # Write point-on-surface points to novel units geopackage
             dsn = "data/baci_nsa_2020_novel_units.gpkg",
             layer = "baci_nsa_geom_on_surface",
             append = FALSE)

sf::st_write(nsa_025mi_buffer, # Write buffered polygons to novel units geopackage
             dsn = "data/baci_nsa_2020_novel_units.gpkg",
             layer = "baci_nsa_025mi_buffer",
             append = FALSE)

sf::st_write(nsa_surface_area_buffer, # Write buffered point-on-surface points to novel units geopackage
             dsn = "data/baci_nsa_2020_novel_units.gpkg",
             layer = "baci_nsa_on_surface_area_buffer",
             append = FALSE)