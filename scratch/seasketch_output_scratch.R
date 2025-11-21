#this is just us testing out loading in seasketch output files
library("sf")
library("terra")


geo <- st_read(here::here("scratch/test_pfsh_polygons.json"))

unique(geo$response_id)
subset_geo <- geo %>%
  dplyr::filter(response_id == 12636)

# plot all polygons for that ID
plot(st_geometry(subset_geo))



###notes below on starting to get the actual extent we'll want for the raster, BUT, in the meantime for sake of a test raster, I think the box of coordinates from xmin = -132, ymin = 28, xmax = -116, ymax = 50 would do the trick 


#getting outer part of eez to use as outer extent for raster:
eez_shp <- st_read("scratch/USMaritimeLimitsAndBoundariesSHP/USMaritimeLimitsNBoundaries.shp")

unique(eez_shp$REGION)
pacific_eez <- eez_shp %>%
  dplyr::filter(REGION == "Pacific Coast")

eez_outer <- pacific_eez %>% 
  dplyr::filter(
    LEGAL_AUTH %in% c(
      "US/Mexico Maritime Boundary Treaty, 1970",
      "Presidential Proclamation No. 5030, March 1983",
      "Federal Register, Vol. 60, No. 163, August 23, 1995", 
      "US/Mexico Maritime Boundary Treaty, 1978"
    )
  )
plot(eez_outer)


#land polygons for inner extent can be found here: https://osmdata.openstreetmap.de/data/land-polygons.html



