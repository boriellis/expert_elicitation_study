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
