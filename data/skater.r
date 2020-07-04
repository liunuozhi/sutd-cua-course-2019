#####
##### START OF COPY
#####

library(spdep)
library(tidyverse)
library(glue)

# replace with your own project path (you can use `getwd()` in the Console)
project_path <- "D:/Workspace/SUTD-2.522-urban-analysis/01-project/liu-nuozhi"

resale <- readRDS(glue(project_path, "/data/resale_with_geom.rds"))

set.seed(13)
resale_sample <- sample_n(resale, 5000)

planning_areas <- st_read(glue(project_path, "/data/MP14_shp/MP14_PLNG_AREA_NO_SEA_PL.shp")) %>% 
  filter(!(OBJECTID == 49 | OBJECTID == 18)) # remove islands

hex_grid <- planning_areas %>%
  st_make_grid(st_bbox(.), square = FALSE, cellsize = 1500) %>% 
  st_sf() %>% 
  mutate(hex_id = row_number())

resale_hex <- st_join(resale_sample, hex_grid) %>% 
  st_set_geometry(NULL)

price_per_sqm <- resale_hex %>% 
  mutate(price_per_sqm = resale_price / floor_area_sqm) %>% 
  group_by(hex_id) %>% 
  summarise(price_per_sqm = mean(price_per_sqm)) %>% 
  left_join(hex_grid, .) %>% 
  #replace_na(list(floor_area_sqm = 999))
  filter(price_per_sqm > 0)

hex_sp <- as(price_per_sqm, 'Spatial')
hex_neighbors <- poly2nb(hex_sp)
hex_neighbors <- edit.nb(hex_neighbors, coordinates(hex_sp))

#####
##### END OF COPY
#####

saveRDS(hex_neighbors, glue(project_path, "/data/hex_neighbors_connect_floor_area_sqm.rds"))