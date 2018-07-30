
# Project Details ---------------------------------------------------------

# Project: Zambia GIS DQA Data
# Purpose: Validate the area calculations and randomly sample 3 polygons
# Date: 2018_06_07
# Authoer: Tim Essam, Ph.D. - GeoCenter


# Preliminaries -----------------------------------------------------------

library(tidyverse)
library(here)
library(sf)
library(units)
library(RColorBrewer)
library(ggmap)
library(knitr)

setwd(here("2018_CFP_Project_Areas_Shapefiles"))
geo_df <- st_read("project_area.shp", stringsAsFactors = FALSE)

# Calculate the area of the polgyons then summarize in a single column
geo_df <- geo_df %>% 
  mutate(area_gc_m = st_area(geometry),
         # Rename the polygon with a missing name
         name_gc = ifelse(OBJECTID == 0, "Shikabeta", Name),
         notes = ifelse(OBJECTID == 0, "extracted from M&E document provided by IP", NA),
         
         area_gc_ha = as.double(area_gc_m / 10000),
         tot_area = sum(area_gc_ha), 
         tot_area_bcp = sum(Hec),
  # Create centroids on the polygons for mapping w/ names
    lon = map_dbl(geometry, ~ st_centroid(.x)[[1]]),
    lat = map_dbl(geometry, ~ st_centroid(.x)[[2]])
  )

# Plot the results
# Call quartz to get results to plot o/wise get polygon edge error
quartz()
geo_df %>% 
  ggplot() +
  geom_sf(aes(fill = area_gc_ha)) +
  scale_fill_viridis_c(alpha = 0.5, 
                       begin = 0.5, 
                       end = .75, 
                       option = "A", 
                       direction = -1) +
  geom_text(aes(label = name_gc, 
                x = lon, 
                y = lat),
            color = "gray20",
            size = 5) +
  ggtitle("BCP AOI for Hectare Calculations")

# Save the output to a pdf for inclusion in the write up doc
ggsave("CFP_BCP_AOI.png", 
       plot = last_plot(),
       width = 10,
       height = 10, 
       units = c("in"))

# Draw a random sample of 3 areas to do follow up work on
# Setting seed so that sample is reproducible
set.seed(20180607)
sample_geo <- as_data_frame(sample_n(geo_df, 3))
glimpse(sample_geo)

sample_geo <- sample_geo %>% 
  select(-geometry, -area_gc_m) 


geo_df %>% 
  filter(name_gc %in% c("Luembe-Chikwasha", "Mnkhanya", "Name Missing")) %>% 
  mutate(
    lon = map_dbl(geometry, ~ st_centroid(.x)[[1]]),
    lat = map_dbl(geometry, ~ st_centroid(.x)[[2]])
  ) %>%
  ggplot() +
  geom_sf(data = geo_df, 
          colour = "white") +
  geom_sf(aes(fill = area_gc_ha)) +
  scale_fill_viridis_c(alpha = 0.5, 
                       begin = 0.5, 
                       end = .75, 
                       option = "A",
                       direction = -1) +
  geom_text(aes(label = name_gc, 
                x = lon, 
                y = lat),
            color = "gray20",
            size = 5) +
  theme(legend.position = "none") +
  ggtitle("Randomly selected polygons selected for documentation verification") 

ggsave("CFP_sampled_polygons.png",
  plot = last_plot(),
       width = 10,
       height = 10, 
       units = c("in"))

write_csv(sample_geo, here("sample_geo.csv"))
