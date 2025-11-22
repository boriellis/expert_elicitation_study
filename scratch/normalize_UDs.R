library(terra)
library(tidyverse)
library(tidyterra)

# Read shapefile
pfsh_geom <- vect(here::here("scratch/test_pfsh_polygons.json")) %>% 
  # Create UD (50/90) and SCOPE (best, broadest, narrowest) columns
  mutate(ud = as.numeric(str_sub(feature_name, 1, 2)),
         scope = str_extract(feature_name, "[^ ]+ ([^ ]+)", 1))

# Create a map of scopes across respondents
ggplot(pfsh_geom) +
  geom_spatvector(aes(color = ud), fill = NA, linewidth = 1.5) +
  facet_grid(response_id ~ scope) +
  scale_x_continuous(breaks = seq(-128, -120, by = 4)) +
  theme_classic(18)

# Save figure
ggsave(here::here("scratch/PFSH_polygons.png"),
       width = 8,
       height = 16,
       units = "in")

# Create rasters

# Transverse cylindrical equal-area projection
equal_area_proj <- "+proj=tcea +lon_0=-123.5742188 +datum=WGS84 +units=m +no_defs"
# Extent for the template (projected from input data)
template_ext <- ext(project(pfsh_geom, equal_area_proj))
# Create template
template <- rast(template_ext, 
                 res = 1e3, # 1 km cells
                 crs = equal_area_proj)
# Pick one respondent (12546) and scope (best)
pfsh_geom_12546_best <- filter(pfsh_geom, 
                               response_id == 12546,
                               scope == "best") %>% 
  project(equal_area_proj)

# Rasterize the polygons
pfsh_dist_12546_best <- rasterize(pfsh_geom_12546_best,
                                  template,
                                  field = "ud",
                                  fun = "min",
                                  background = 100)
plot(pfsh_dist_12546_best)

# Normalize the values so they all add up to 1
n_ud_level <- table(values(pfsh_dist_12546_best))
for (i in 1:3) {
  ud_level <- c(50, 95, 100)[i]
  total <- c(0.5, 0.45, 0.05)[i]
  pfsh_dist_12546_best[pfsh_dist_12546_best == ud_level] <- total / n_ud_level[i]
}
plot(pfsh_dist_12546_best)
global(pfsh_dist_12546_best, sum) # Adds to 1!





