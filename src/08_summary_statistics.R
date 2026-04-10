# ========================================================
# Author: Harrison DeFord
# Date: Mar 7, 2026
# Title: 08 - Aggregation Summary Statistics
#
# Description: This script will take the polygons created previously and
# calculate summary statistics of the CfS counts recorded per polygon. The results
# will be used to determine if there is variation between aggregation methods. If
# there are significant differences in patterns and statistics between methods,
# then the null hypothesis can be rejected -- decisions made early in analysis (or
# even before analysis) can lead to differing patterns.

# It begins by reading in the various result layers from scripts 6 and 7, which
# contain representations of the aggregation units with CfS counts aggregated by
# their respective methods. It then computes basic summary statistics, including
# the mean, min, max, and percentiles. These statistics can be compared to
# evaluate the differences (if any) between the aggregation methods.
# ========================================================

# ========================================================
# INSTALL AND LOAD LIBRARIES
# ========================================================

package_missing <- setdiff(c("sf", "dplyr", "sfdep"), installed.packages()) # Check for any missing required libraries

install.packages(package_missing) # Install missing libraries

library(sf)    # Simple Features for R
library(dplyr) # A Grammar of Data Manipulation
library(sfdep) # Spatial Dependence for Simple Features

set.seed(2819)

nsa_geom_cent_hulls <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
                                   layer = "baci_nsa_geom_cent_hulls_CfS")

nsa_geom_cent_CfS <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
                                 layer = "baci_nsa_geom_cent_CfS")

nsa_geom_surface_hulls <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
                                      layer = "baci_nsa_geom_com_hulls_CfS")

nsa_geom_surface_CfS <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
                                    layer = "baci_nsa_geom_surface_CfS")

nsa_med_area_square_fishnet_CfS <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
                                               layer = "baci_nsa_med_area_square_fishnet_CfS")

nsa_med_area_hex_fishnet_CfS <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
                                            layer = "baci_nsa_med_area_hex_fishnet_CfS")

baci_nsa_2020_CfS <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
                                 layer = "baci_nsa_2020_CfS")

nsa_025mi_buffer_CfS <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
                                    layer = "baci_nsa_025mi_buffer_CfS")

nsa_surface_area_buffer_CfS <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
                                           layer = "baci_nsa_surface_area_buffer_CfS")

nsa_geom_surface_isochrone_CfS <- sf::st_read(dsn = "data/baci_nsa_2020_novel_units_results.gpkg",
                                              layer = "baci_nsa_geom_surface_isochrone_CfS")


compute_spatial_stats <- function(sdf, count_col) {
  # Extract the vector of counts
  counts <- sdf[[count_col]]
  
  # 1. Basic Summary Stats
  n_val <- length(counts)
  total_val <- sum(counts)
  mu <- mean(counts, na.rm = TRUE)
  cv_val <- sd(counts, na.rm = TRUE) / mu
  min_val <- min(counts, na.rm = TRUE)
  max_val <- max(counts, na.rm = TRUE)
  
  quants <- quantile(counts, probs = c(0.10, 0.25, 0.5, 0.75, 0.90), na.rm = TRUE)
  
  # 2. Gini Coefficient (Inequality)
  gini_val <- sum(outer(counts, counts, FUN=function(x,y){abs(x-y)})) / (2 * length(counts) * sum(counts))
  
  # 3. Global Moran's I (Spatial Autocorrelation)
  # Create neighborhood list (Queen contiguity)
  nb <- sfdep::st_knn(sf::st_centroid(sdf), k = 6)
  
  # Convert to weights list
  # zero.policy = TRUE handles polygons with no neighbors
  wt <- sfdep::st_weights(nb)
  
  # Perform Moran's I test
  moran_res <- sfdep::global_moran_perm(counts, nb, wt, nsim = 999)
  print(moran_res)
  moran_i <- moran_res$statistic[1] # The I statistic
  p_val <- moran_res$p.value
  
  # Return as a tibble for easy row-binding
  tibble(
    n_units     = n_val,
    total_cfs   = total_val,
    mean_cfs    = mu,
    cv_cfs      = cv_val,
    gini_cfs    = gini_val,
    min_cfs     = min_val,
    p10_cfs     = unname(quants[1]),
    p25_cfs     = unname(quants[2]),
    median_cfs  = unname(quants[3]),
    p75_cfs     = unname(quants[4]),
    p90_cfs     = unname(quants[5]),
    max_cfs     = max_val,
    moran_i_cfs = moran_i,
    moran_p_cfs = p_val
  )
}

results_table <- lapply(
  list(
    "NSA Polygons (across)"                       = baci_nsa_2020_CfS,
    "Hexagon Fishnet—Median Area (across)"         = nsa_med_area_hex_fishnet_CfS,
    "Square Fishnet—Median Area (across)"      = nsa_med_area_square_fishnet_CfS,
    "NSA Polygons—0.25mi Buffer (outwards)"       = nsa_025mi_buffer_CfS,
    "NSA Surface Points—Median Area Buffer (outwards)"    = nsa_surface_area_buffer_CfS,
    "NSA Surface Points—15-minute isochrone (outwards)"           = nsa_geom_surface_isochrone_CfS,
    "NSA Surface Points (inwards)"        = nsa_geom_surface_CfS,
    "NSA Geometric Centroid Points (inwards)"       = nsa_geom_cent_CfS
  ),
  \(x) compute_spatial_stats(x, "CfS_Count")
) |> 
  bind_rows(.id = "Aggregation Method") # This creates the first column using the list names
