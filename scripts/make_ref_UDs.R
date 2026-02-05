###############################################################################
# Map making script for annual maps for reference in expert elicitation process  ######################
# Code Author: Aspen Ellis (aaellis@ucsc.edu) ##
###############################################################################
#------------------------------------------------------------------------------

# Part 1: Load Packages -------------------------------------------------------

packages<- c("tidyverse", "sf", "terra", "tidyr", "tidyterra", "ggplot2", "dplyr")
pacman::p_load(packages, character.only = TRUE); rm(packages)


source(here::here("R/prep_ref_UDs.R"))


# Part 2: Load rasters ----------------------------------------------------

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

plot(annual_dens_norm[["SCMU-GUMU-CRMU_annual"]])

#bin cells into 50%, 95%, 100%
uds <- make_UDs(annual_dens_norm)

plot(uds[["SCMU-GUMU-CRMU_annual"]])






#OLD SCRATCH FROM LAST YEAR'S VERSION.

