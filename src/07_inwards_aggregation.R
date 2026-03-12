# ========================================================
# Author: Harrison DeFord
# Date: Mar 7, 2026
# Title: 07 - Perform Inwards Aggregation
#
# Description: This script will perform "inwards" aggregation of CfS calls to
# point units. To do this, each CfS record is related to its nearest NSA core
# point, both geometric centroids and point-on-surface points. Because inwards
# aggregation can capture all points, even those far outside the study area,
# the CfS calls are spatially filtered to include only those within 0.25 miles
# of the Baltimore City boundaries.
#
# It first reads the cleaned CfS data from script 02, and then the novel point
# units for aggregation. Then, the CfS points are filtered to include only those
# within a quarter-mile of the Baltimore City boundary. The points are then related
# to their nearest core point feature. After the IDs of the related cores have been
# captured, the CfS calls are counted. A convex hull is created to create a polygon
# representation of the extent of each core point's area of influence, and to allow
# for the computation of results which require polygon units.
#
# The counts of CfS related to each core point are then appended to the core points
# themselves. Finally, the convex hulls and core points are written to the results
# geopackage for analysis.
# ========================================================

# ========================================================
# INSTALL AND LOAD LIBRARIES
# ========================================================

package_missing <- setdiff(c("sf", "dplyr"), installed.packages()) # Check for any missing required libraries

install.packages(package_missing) # Install missing libraries

library(sf)           # Simple Features for R
library(dplyr)        # A Grammar of Data Manipulation

# ========================================================
# DEFINE FUNCTION FOR INWARDS POINT COUNTING
# ========================================================

get_nearest_core <- function(points, cores){
  cfs_nearest_cores_vec <- sf::st_nearest_feature(points, # Get vector (length of points) containing the index of the nearest core
                                                  cores)
  
  cfs_w_nearest_core <- points |> 
    dplyr::mutate(nearest_core = cfs_nearest_cores_vec) # Create column containing the index of the nearest core computed previously
  
  return(cfs_w_nearest_core)
}

build_convex_hull <- function(points, cores){
  cfs_w_nearest_core <- get_nearest_core(points, cores) # Get CfS records with nearest points, using function above
  
  cfs_mult_nearest_core <- cfs_w_nearest_core |>
    dplyr::group_by(nearest_core) |>        # Group by the nearest core point
    dplyr::summarize(geom = st_union(geom), # Combine points into a MULTIPOINT geometry type, with a MULTIPOINT related to each core
                     CfS_Count = n())       # Count points related to each core point
  
  df_all_cores <- cores |>
    dplyr::mutate(nearest_core = row_number()) |>
    sf::st_drop_geometry()
  
  cfs_convex_hull <- df_all_cores |>
    left_join(cfs_mult_nearest_core, by = "nearest_core") |>
    sf::st_as_sf() |>
    sf::st_convex_hull() |>                                            # Create convex hull wrapped around the CfS points for each core to define an "area of influence"
    sf::st_as_sf() |>
    dplyr::mutate(CfS_Count = replace(CfS_Count, is.na(CfS_Count), 0)) # Replace NA values with 0, because that means no points were closest to that core

  return(cfs_convex_hull)
}

return_points_w_CfS <- function(cores, hulls){
  df_all_cores <- cores |>
    dplyr::mutate(nearest_core = row_number()) # Create column containing the index of the core (row number in same order)
  
  hulls_CfS <- hulls |>
    sf::st_drop_geometry() |>              # Drop geometry to speed up computation
    dplyr::select(nearest_core, CfS_Count) # Select only ID and CfS count to avoid unnecessary columns
  
  cores_w_CfS <- df_all_cores |>
    dplyr::left_join(hulls_CfS, by = "nearest_core") |>                # Join previous dataframe on row index to relate counts to points
    sf::st_as_sf()                                                     # Ensure that object is an sf object
  
  return(cores_w_CfS)
}

# ========================================================
# READ DATA FROM GEOPACKAGES
# ========================================================

all_cfs_cleaned <- sf::st_read(dsn = "data/cfs_baci_2010_2025.gpkg", # Read cleaned CfS data
                               layer = "cleaned_ds_da_cfs_baci_2010_2025_v1")

md_counties <- sf::st_read(dsn = "data/cfs_baci_2010_2025.gpkg", # Read MD county boundaries
                           layer = "md_cnty_500k_2020")

nsa_geom_cent <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units.gpkg", # Read geometric NSA centroids
                             layer = "baci_nsa_geom_centroid")

nsa_geom_surface <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units.gpkg", # Read NSA point-on-surface points
                            layer = "baci_nsa_geom_on_surface")

balt_boundaries <- md_counties[md_counties$GEOID == '24510',] # Filter counties to only Baltimore City (GEOID 24510)
balt_boundaries_edge <- balt_boundaries |> 
  sf::st_buffer(1320)

all_cfs_cleaned_edge <- sf::st_filter(all_cfs_cleaned, balt_boundaries_edge) # Keep only points within 0.25mi of Baltimore boundaries, to handle edge effects

# ========================================================
# COUNT POINTS CLOSEST TO EACH NEIGHBORHOOD CORE
# ========================================================

nsa_geom_cent_hulls <- build_convex_hull(all_cfs_cleaned_edge, # Count points closest to each NSA geometric centroid and generate convex hulls
                       nsa_geom_cent)

nsa_geom_cent_CfS <- return_points_w_CfS(nsa_geom_cent, nsa_geom_cent_hulls) # Append CfS counts to centroid points

nsa_geom_surface_hulls <- build_convex_hull(all_cfs_cleaned_edge, # Count points closest to each NSA point-on-surface and generate convex hulls
                                            nsa_geom_surface)

nsa_geom_surface_CfS <- return_points_w_CfS(nsa_geom_surface, nsa_geom_surface_hulls) # Append CfS counts to point-on-surface points

# ========================================================
# WRITE RESULTS TO GEOPACKAGE
# ========================================================

sf::st_write(nsa_geom_cent_hulls, # Write geometric centroid convex hull with CfS to results geopackage
             dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
             layer = "baci_nsa_geom_cent_hulls_CfS",
             append = FALSE)

sf::st_write(nsa_geom_cent_CfS, # Write geometric centroid points with CfS to results geopackage
             dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
             layer = "baci_nsa_geom_cent_CfS",
             append = FALSE)

sf::st_write(nsa_geom_surface_hulls, # Write NSA point-on-surface convex hull with CfS to results geopackage
             dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
             layer = "baci_nsa_geom_com_hulls_CfS",
             append = FALSE)

sf::st_write(nsa_geom_surface_CfS, # Write NSA point-on-surface points with CfS to results geopackage
             dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
             layer = "baci_nsa_geom_surface_CfS",
             append = FALSE)
