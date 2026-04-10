# ========================================================
# Author: Harrison DeFord
# Date: Apr 9, 2026
# Title: 09 - Compute CfS Distances
#
# This script will use the CfS points and the spatial units previously created to
# identify the mean distance that a CfS event shifted between its actual location
# and the location to which it was assigned after aggregation. Comparison of the
# shifts in distance between aggregation methods will be used as evidence towards
# H2: that the impact of the decisions made early in analysis are not significant
# enough to warrant special care.
#
# It begins by reading in the cleaned CfS points and the novel aggregation units.
# For polygon units, the point-on-surface centroid was computed as the 'core'
# of the unit to which the event was assigned—in other words, the way that the
# spatial pattern would appear on a map. The distance between each original CfS
# point and the related geometry is then computed. For points that fall into more
# than one geometry (in outwards aggregation), the distance to each containing
# spatial unit is averaged.
# ========================================================

# ========================================================
# INSTALL AND LOAD LIBRARIES
# ========================================================

package_missing <- setdiff(c("sf", "dplyr", "purrr"), installed.packages()) # Check for any missing required libraries

install.packages(package_missing) # Install missing libraries

library(sf)    # Simple Features for R
library(dplyr) # A Grammar of Data Manipulation
library(purrr) # Functional Programming Tools

# ========================================================
# READ DATA FROM GEOPACKAGES
# ========================================================

all_cfs_cleaned <- sf::st_read(dsn = "data/cfs_baci_2010_2025.gpkg", # Read cleaned CfS data
                               layer = "cleaned_ds_da_cfs_baci_2010_2025_v2")

nsa_geom_cent_CfS <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units_results.gpkg", # Read NSA center-of-mass centroid points
                                 layer = "baci_nsa_geom_cent_CfS")

nsa_geom_surface_CfS <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units_results.gpkg", # Read NSA point-on-surface centroid points
                                    layer = "baci_nsa_geom_surface_CfS")

nsa_med_area_square_fishnet_CfS <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units_results.gpkg", # Read square fishnet polygons
                                               layer = "baci_nsa_med_area_square_fishnet_CfS")

nsa_med_area_hex_fishnet_CfS <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units_results.gpkg", # Read hexagon fishnet polygons
                                            layer = "baci_nsa_med_area_hex_fishnet_CfS")

baci_nsa_2020_CfS <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units_results.gpkg", # Read NSA boundary polygons
                                 layer = "baci_nsa_2020_CfS")

nsa_025mi_buffer_CfS <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units_results.gpkg", # Read buffered NSA boundary polygons
                                    layer = "baci_nsa_025mi_buffer_CfS")

nsa_surface_area_buffer_CfS <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units_results.gpkg", # Read symmetrical-buffer point-on-surface polygons
                                           layer = "baci_nsa_surface_area_buffer_CfS")

nsa_geom_surface_isochrone_CfS <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units_results.gpkg", # Read isochrone point-on-surface polygons
                                              layer = "baci_nsa_geom_surface_isochrone_CfS") 

# ========================================================
# FUNCTIONS TO COMPUTE DISTANCES
# ========================================================

# For inwards aggregation, use the points to get the nearest cores
get_nearest_core <- function(points, cores){
  cfs_nearest_cores_vec <- sf::st_nearest_feature(points, # Get vector (length of points) containing the index of the nearest core
                                                  cores)
  
  cfs_w_nearest_core <- points |> 
    dplyr::mutate(nearest_core = cfs_nearest_cores_vec) # Create column containing the index of the nearest core computed previously
  
  return(cfs_w_nearest_core)
}

# Calculate the shift distance for each point over each method
calculate_shift_distance <- function(cfs, agg_units, join_field, method){
  if (!method %in% c("inwards", "outwards", "across")){
    stop("`method` must be one of 'inwards', 'outwards', or 'across'.") # Method must be one of the three discussed
  }
  
  if (method == 'across' | method == 'outwards') {
    polygon_pt_on_surface <- sf::st_point_on_surface(agg_units) # Convert polygon to point to compare the distance from the CfS
    
    dat <- cfs |> 
      st_join(agg_units) # Spatial join each CfS to its containing polygon
  }
  
  if (method == 'inwards'){
    polygon_pt_on_surface <- agg_units |>
      mutate(nearest_core = row_number()) # Create ID from row number (essentially, index)
    
    dat <- get_nearest_core(cfs, agg_units) # Relate each CfS to its nearest core point, like done before during aggregation
  }
  
  dat_out <- dat |>
         left_join(as.data.frame(polygon_pt_on_surface),                            # Table join between the CfS geom and the geom to which it was assigned
                   by = join_field, suffix = c("_cfs", "_unit")) |>                 # Create suffixes to tell geometries apart
         mutate(dist = as.numeric(st_distance(geom_cfs,                             
                                              st_sfc(geom_unit, crs = st_crs(cfs)), # Compute distances between the CfS and the spatial unit geometries
                                              by_element = TRUE))) |>               # Do not create a sparse matrix
         st_drop_geometry() |>                                                      # Tabular operation to speed up, geometry is no longer necessary
         group_by(ServiceRequestNum) |>                                             
         summarize(mean_dist = mean(dist)) |>                                       # In outwards, a point can match with more than one unit. Take the mean of distances to all related units
         ungroup()
  
  return(dat_out)
}

# ========================================================
# ITERATE OVER AGGREGATION METHODS
# ========================================================

# Create list of parameters for each method
params_list <- list(
  "NSA Polygons (across)" = list(units = baci_nsa_2020_CfS, field = "Name", type = "across"),                                            # Original NSAs
  "Hexagon Fishnet—Median Area (across)" = list(units = nsa_med_area_hex_fishnet_CfS, field = "grid_id", type = "across"),               # Hexagon grid
  "Square Fishnet—Median Area (across)" = list(units = nsa_med_area_square_fishnet_CfS, field = "grid_id", type = "across"),             # Square grid
  "NSA Polygons—0.25mi Buffer (outwards)" = list(units = nsa_025mi_buffer_CfS, field = "Name", type = "outwards"),                       # NSA polygon buffer
  "NSA Surface Points—Median Area Buffer (outwards)" = list(units = nsa_surface_area_buffer_CfS, field = "Name", type = "outwards"),     # NSA surface point buffer
  "NSA Surface Points—15-minute isochrone (outwards)" = list(units = nsa_geom_surface_isochrone_CfS, field = "Name", type = "outwards"), # NSA isochrones
  "NSA Surface Points (inwards)" = list(units = nsa_geom_surface_CfS, field = "nearest_core", type = "inwards"),                         # NSA surface points
  "NSA Geometric Centroid Points (inwards)" = list(units = nsa_geom_cent_CfS, field = "nearest_core", type = "inwards")                  # NSA center-of-mass points
)

# Iterate over params_list, applying calculate_shift_distance() under each aggregation method
dist_list <- lapply(names(params_list), function(nm) {
  p <- params_list[[nm]]    # Name of current method
  
  calculate_shift_distance( 
    cfs = all_cfs_cleaned,    # Cleaned CfS points for all aggregation techniques
    agg_units = p$units,      # Aggregation units
    join_field = p$field,     # Field to join on under the hood
    method = p$type           # Inwards, outwards, or across
  ) |>
    rename(!!nm := mean_dist) # Dynamic renaming for readability
})

all_methods_dist_df <- dist_list |> 
  purrr::reduce(full_join, by = "ServiceRequestNum") # Combine into one dataframe which contains all of the distances for all CfS across all methods

write.csv(all_methods_dist_df,
          "data/all_cfs_all_methods_distances.csv") # Write previous df to CSV to be ingested for computation of statistics
