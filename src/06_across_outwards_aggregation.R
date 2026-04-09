# ========================================================
# Author: Harrison DeFord
# Date: Mar 7, 2026
# Title: 06 - Perform Across and Outwards Aggregation
#
# Description: This script will perform "across" and "outwards" aggregation of CfS calls into
# spatial units. In particular, these aggregations are simple point-in-polygon
# counts using square fishnet grids, hexagon fishnet grids, and the NSA units themselves;
# as well as polygonal buffers, symmetrical buffers, and walkshed isochrones (asymmetric buffers).
#
# It first reads the cleaned CfS data from script 02, and then the polygon
# units for aggregation. Then, the CfS points are spatially joined and counted 
# per unit. The spatial units, with the counts in a CfS_Count field, are then
# written to a results geopackage.
# ========================================================

# ========================================================
# INSTALL AND LOAD LIBRARIES
# ========================================================

package_missing <- setdiff(c("sf", "dplyr"), installed.packages()) # Check for any missing required libraries

install.packages(package_missing) # Install missing libraries

library(sf)           # Simple Features for R
library(dplyr)        # A Grammar of Data Manipulation

# ========================================================
# DEFINE FUNCTION FOR POINT COUNTING
# ========================================================

count_points_in_polygon <- function(points, polygons){
  if(sum(sf::st_geometry_type(points) != "POINT") > 0){       # Check to ensure that the points argument is actually points
    stop("Non-point observations found in argument `points`") # Stop function if there are any non-point arguments
  }
  
  polygons_w_id <- polygons |> # Create column in polygons with ID to be used only within the function environment
    dplyr::mutate(pid = dplyr::row_number())
  
  points_sj <- points |>
    sf::st_join(polygons_w_id) |>            # Spatial join points to polygons with ID
    sf::st_drop_geometry() |>                # Drop point geometry to make summarization faster
    dplyr::group_by(pid) |>                  # Group by polygon ID created previously
    dplyr::summarize(CfS_Count = dplyr::n()) # Count point records per polygon
  
  polygons_w_count <- polygons_w_id |>       
    dplyr::left_join(points_sj, by = join_by("pid")) |>                   # Join counted points table to polygons
    dplyr::mutate(CfS_Count = replace(CfS_Count, is.na(CfS_Count), 0)) |> # If there are NA, it is because no points were counted, so replace with 0
    dplyr::select(-pid) |>                                                # Drop internal ID column because object has original polygon columns again
    sf::st_as_sf()                                                        # Ensure output is an SF object
  
  return(polygons_w_count)
}

# ========================================================
# READ DATA (CFS AND NOVEL UNITS)
# ========================================================

all_cfs_cleaned <- sf::st_read(dsn = "data/cfs_baci_2010_2025.gpkg", # Read cleaned CfS data
                               layer = "cleaned_ds_da_cfs_baci_2010_2025_v2")

baci_nsa_2020 <- sf::st_read(dsn = "data/cfs_baci_2010_2025.gpkg", # Read NSA polygons
                             layer = "cleaned_nsa_bnd_baci_2020_v1")

nsa_med_area_square_fishnet <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units.gpkg", # Read square fishnet grid
                                           layer = "baci_nsa_med_area_square_fishnet")

nsa_med_area_hex_fishnet <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units.gpkg", # Read hexagon fishnet grid
                                        layer = "baci_nsa_med_area_hex_fishnet")

nsa_025mi_buffer <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units.gpkg", # Read buffered polygon areas
                                layer = "baci_nsa_025mi_buffer")

nsa_surface_area_buffer <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units.gpkg", # Read buffered point-on-surface areas
                                   layer = "baci_nsa_on_surface_area_buffer")

nsa_geom_surface_isochrone <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units.gpkg", # Read isochrone walksheds
                                            layer = "baci_nsa_on_surface_isochrone")

# ========================================================
# COUNT POINTS IN POLYGONS (ACROSS)
# ========================================================

baci_nsa_2020_CfS <- count_points_in_polygon(all_cfs_cleaned, baci_nsa_2020)                             # Count CfS across NSA areas
nsa_med_area_square_fishnet_CfS <- count_points_in_polygon(all_cfs_cleaned, nsa_med_area_square_fishnet) # Count CfS across square fishnet
nsa_med_area_hex_fishnet_CfS <- count_points_in_polygon(all_cfs_cleaned, nsa_med_area_hex_fishnet)       # Count CfS across hex fishnet

# ========================================================
# COUNT POINTS IN POLYGONS (OUTWARDS)
# ========================================================

nsa_025mi_buffer_CfS <- count_points_in_polygon(all_cfs_cleaned, nsa_025mi_buffer)
nsa_surface_area_buffer_CfS <- count_points_in_polygon(all_cfs_cleaned, nsa_surface_area_buffer)
nsa_geom_surface_isochrone_CfS <- count_points_in_polygon(all_cfs_cleaned, nsa_geom_surface_isochrone)

# ========================================================
# WRITE RESULTS TO GEOPACKAGE
# ========================================================

sf::st_write(nsa_med_area_square_fishnet_CfS, # Write square fishnet with CfS to results geopackage
             dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
             layer = "baci_nsa_med_area_square_fishnet_CfS",
             append = FALSE)

sf::st_write(nsa_med_area_hex_fishnet_CfS, # Write hex fishnet with CfS to results geopackage
             dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
             layer = "baci_nsa_med_area_hex_fishnet_CfS",
             append = FALSE)

sf::st_write(baci_nsa_2020_CfS, # Write NSA polygons with CfS to results geopackage
             dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
             layer = "baci_nsa_2020_CfS",
             append = FALSE)

sf::st_write(nsa_025mi_buffer_CfS, # Write buffered NSA polygons with CfS to results geopackage
             dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
             layer = "baci_nsa_025mi_buffer_CfS",
             append = FALSE)

sf::st_write(nsa_surface_area_buffer_CfS, # Write buffered NSA center-of-mass with CfS to results geopackage
             dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
             layer = "baci_nsa_surface_area_buffer_CfS",
             append = FALSE)

sf::st_write(nsa_geom_surface_isochrone_CfS, # Write isochrone polygons with CfS to results geopackage
             dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
             layer = "baci_nsa_geom_surface_isochrone_CfS",
             append = FALSE)