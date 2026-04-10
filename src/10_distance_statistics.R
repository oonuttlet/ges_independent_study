# ========================================================
# Author: Harrison DeFord
# Date: Apr 9, 2026
# Title: 10 - Sensitivity Analysis
#
# This script will use the previously calculated distances between each CfS
# and its assigned spatial unit to quantify the size of the effect of the various
# aggregation methods.
#
# It begins by reading in the CfS distance data from CSV, and the performs several
# tests on the distances and their distributions.
# ========================================================

# ========================================================
# INSTALL AND LOAD LIBRARIES
# ========================================================

package_missing <- setdiff(c("sf", "dplyr", "purrr"), installed.packages()) # Check for any missing required libraries

install.packages(package_missing) # Install missing libraries

library(sf)    # Simple Features for R
library(dplyr) # A Grammar of Data Manipulation
library(purrr) # Functional Programming Tools
compute_spatial_stats <- function(sdf, distance_col) {
  # Extract the vector of counts
  dists <- sdf[[distance_col]]
  
  # 1. Basic Summary Stats
  mu <- mean(dists, na.rm = TRUE)
  print("mu")
  cv_val <- sd(dists, na.rm = TRUE) / mu
  print("cv")
  min_val <- min(dists, na.rm = TRUE)
  print("min")
  max_val <- max(dists, na.rm = TRUE)
  print("max")
  
  quants <- quantile(dists, probs = c(0.10, 0.25, 0.5, 0.75, 0.90), na.rm = TRUE)
  print("quants")
  
  # Return as a tibble for easy row-binding
  tibble(
    mean_dist    = mu,
    cv_dist      = cv_val,
    min_dist     = min_val,
    p10_dist     = unname(quants[1]),
    p25_dist     = unname(quants[2]),
    median_dist  = unname(quants[3]),
    p75_dist     = unname(quants[4]),
    p90_dist     = unname(quants[5]),
    max_dist     = max_val,
  )
}