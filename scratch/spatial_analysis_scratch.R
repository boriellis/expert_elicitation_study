#script to look at the spatial D-D, D-I, and I-I approaches


# get simplified actual maps first ----------------------------------------
#I'm being lazy and copying over the whole raster stack which I can call the individual species from - come back and clean this up to be for the 6 species later 

packages<- c("tidyverse", "sf", "terra", "tidyr", "tidyterra", "ggplot2", "dplyr", "purrr")
pacman::p_load(packages, character.only = TRUE); rm(packages)

source(here::here("R/prep_ref_UDs.R"))

#load in the original Leirness average distribution maps and associated CV maps
density_paths <- dir(here::here("data/raw_data/leirness_model_outputs/"),
                     pattern = "density.tif$",
                     full.names = TRUE)

densities <- map(density_paths, rast) %>% 
  rast()

#combine into annual rasters
annual_densities <- combine_seasons(densities)



# Part 3: make UDs --------------------------------------------------------

#normalize so that the cells sum to 1
annual_dens_norm <- normalize_annuals(annual_densities)

#bin cells into 50%, 95%, 100%
uds <- make_UDs(annual_dens_norm)


#check out the individual actuals
plot(uds[["ASSP_annual"]])
plot(uds[["BLKI_annual"]])
plot(uds[["CAAU_annual"]])
plot(uds[["LAAL_annual"]])
plot(uds[["NOFU_annual"]])
plot(uds[["POJA_annual"]])


CAAUss <- vect("data/raw_data/raw_survey_outputs/seasketch/cassin's_aukletplease_represent_.geojson.json")

plot(CAAUss) 

single_CAAU <- CAAUss %>% 
  filter(response_id == 15662) %>% 
  mutate(ud = as.numeric(CAAU_ud))

ggplot(single_CAAU, aes(fill = CAAU_ud)) + geom_spatvector()

single_CAAU_rast <- rasterize(project(single_CAAU, crs(uds[["CAAU_annual"]])),
          uds[["CAAU_annual"]], field = "ud", fun = "min") %>% 
  mask(uds[["CAAU_annual"]])

single_CAAU_ud <- uds[["CAAU_annual"]]
single_CAAU_ud[!is.na(single_CAAU_ud)] <- 0
single_CAAU_ud <- single_CAAU_ud + subst(single_CAAU_rast, NA, 0)

single_CAAU_ud <- single_CAAU_ud %>% 
  subst(50, 0.5 / global(single_CAAU_ud == 50, sum, na.rm = TRUE)) %>% 
  subst(95, 0.45 / global(single_CAAU_ud == 95, sum, na.rm = TRUE)) %>% 
  subst(100, 0.05 / global(single_CAAU_ud == 100, sum, na.rm = TRUE))
names(single_CAAU_ud) <- "ud_expert"

plot(single_CAAU_ud)

CAAU_ud_actual <- uds[["CAAU_annual"]] %>% 
  subst(1000, 0) %>% 
  subst(0.5, 0.5 / global(uds[["CAAU_annual"]] == 0.5, sum, na.rm = TRUE)) %>% 
  subst(0.95, 0.45 / global(uds[["CAAU_annual"]] == 0.95, sum, na.rm = TRUE)) %>% 
  subst(1.0, 0.05 / global(uds[["CAAU_annual"]] == 1.0, sum, na.rm = TRUE))

as.data.frame(uds[["CAAU_annual"]], xy = TRUE) %>% 
  rename(ud_actual = CAAU_annual) %>% 
  mutate(ud_actual = ud_actual / sum(ud_actual))

view(CAAUss)
