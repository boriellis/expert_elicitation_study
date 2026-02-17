###############################################################################
# Map making script for annual maps for reference in expert elicitation process  ######################
# Code Author: Aspen Ellis (aaellis@ucsc.edu) ##
###############################################################################
#------------------------------------------------------------------------------

# Part 1: Load Packages -------------------------------------------------------

packages<- c("tidyverse", "sf", "terra", "tidyr", "tidyterra", "ggplot2", "dplyr", "purrr")
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

#bin cells into 50%, 95%, 100%
uds <- make_UDs(annual_dens_norm)

plot(uds[["SCMU-GUMU-CRMU_annual"]])

# pdf("~/Downloads/ud_50_95_testvers.pdf", width = 6.5, height = 9)
# for (i in 1:nlyr(uds)) {
#   plot(uds[[i]], main = names(uds)[i])
# }
# dev.off()

uds_fourclass <- make_UDs_fourclass(annual_dens_norm)

plot(uds_fourclass[["ANMU_annual"]])


# Part 4: make pngs -------------------------------------------------------

#loading in the states outlines and making the projection match the density data
states1 <- vect("data/map_extras/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
crds(states1, df = FALSE)

e <- ext(-130,-117, 30, 50)
west <- crop(states1, e)
plot(west)

crs <- "+proj=omerc +lat_0=39 +lonc=-125 +alpha=75 +gamma=75 +k=0.9996 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" #this is the coordinate system for the density data  
states <- project(west, crs)


#bin cells into 50%, 95%, 100%
uds <- make_UDs(annual_dens_norm)

uds_list <- lapply(seq_len(nlyr(uds)), function(i) uds[[i]])
names(uds_list) <- names(uds)


walk2(
  uds_list,
  names(uds_list),
  function(r, nm) {
    
    speciescode <- sub("_annual.*$", "", nm)
    
    ggsave(
      filename = file.path(
        "reports/images",
        paste0(speciescode, "_annual_ref_ud.png")
      ),
      plot = plot_ud(r, speciescode, states),
      width = 10.5,
      height = 8,
      dpi = 400,
      bg = "transparent"
    )
  }
)






#OLD

# 
# ud_cols <- c(
#   "0.5"  = "#D73027",  # red
#   "0.95" = "#FEE08B",  # yellow
#   "1"    = "#91BFDB"   # light blue
# )
# 
# ud_labs <- c(
#   "0.5"  = "50% core area",
#   "0.95" = "95% area"
# )
# 
# uds_list <- lapply(seq_len(nlyr(uds)), function(i) uds[[i]])
# names(uds_list) <- names(uds)
# 
# walk2(
#   uds_list,
#   names(uds_list),
#   function(r, nm) {
#     
#     speciescode <- sub("_annual.*$", "", nm)
#     
#     ggsave(
#       filename = file.path(
#         "reports/images",
#         paste0(speciescode, "_annual_ref_ud.png")
#       ),
#       plot = plot_ud(r, speciescode, states),
#       width = 10.5,
#       height = 8,
#       dpi = 400,
#       bg = "transparent"
#     )
#   }
# )
# 
# 
