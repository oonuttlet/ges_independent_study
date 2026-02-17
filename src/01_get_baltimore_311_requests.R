library(arcgislayers)
library(sf)
library(tidyverse)

# get all 3-1-1 complaints from Baltimore REST
where_clause <- "SRType IN ('SW-Dirty Alley', 'SW-Dirty Street')"
# where_clause <- "SRType IN ('SW-Rat Rubout', 'HCD-Rodent')"
# 2025
cfs_2025 <- arc_read(r"(https://services1.arcgis.com/UWYHeuuJISiGmgXx/arcgis/rest/services/311_Customer_Service_Requests_2025/FeatureServer/0)",
                     where = where_clause) |> 
  st_drop_geometry() |> 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
  mutate(HashedRecord = "")
cfs_2024 <- arc_read(r"(https://services1.arcgis.com/UWYHeuuJISiGmgXx/arcgis/rest/services/311_Customer_Service_Requests_2024/FeatureServer/0)",
                     where = where_clause)
cfs_2023 <- arc_read(r"(https://services1.arcgis.com/UWYHeuuJISiGmgXx/arcgis/rest/services/311_Customer_Service_Requests_2023/FeatureServer/0)",
                     where = where_clause)
cfs_2010_2022 <- lapply(0:12, \(x) try(arc_read(paste0(r"(https://services1.arcgis.com/UWYHeuuJISiGmgXx/ArcGIS/rest/services/311_Customer_Service_Requests_Yearly/FeatureServer/)", x),
                                                   where = where_clause))) |> bind_rows()

all_cfs <- bind_rows(cfs_2025,
                     cfs_2024,
                     cfs_2023,
                     cfs_2010_2022)

cfs_sample <- sample_frac(all_cfs, 0.10) |>
  st_as_sf()

ggplot(cfs_sample) +
  geom_sf(mapping = aes(color = year(CreatedDate))) +
  theme_void()
