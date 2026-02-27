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

plot(annual_densities[["PFSH_annual"]])


#bin cells into 50%, 95%, 100%
uds <- make_UDs(annual_dens_norm)


#loading in the states outlines and making the projection match the density data
states1 <- vect("data/map_extras/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
crds(states1, df = FALSE)

e <- ext(-130,-117, 30, 50)
west <- crop(states1, e)
plot(west)

crs <- "+proj=omerc +lat_0=39 +lonc=-125 +alpha=75 +gamma=75 +k=0.9996 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" #this is the coordinate system for the density data  
states <- project(west, crs)


#functions to plot the other two:

plot_ud_range_only <- function(r, species_code, states) {
  
  ggplot() +
    geom_spatraster(
      data = r,
      aes(fill = factor(after_stat(value))),
      na.rm = TRUE
    ) +
    geom_spatvector(
      data = states,
      color = "#ffffff",
      fill  = "#8290AB",
      linewidth = 0.3
    ) +
    scale_fill_manual(
      values = c(
        "0.5"  = "#91BFDB",  # red
        "0.95" = "#91BFDB",  # yellow
        "1"    = "#91BFDB",  # light blue
        "1000"    = "#E0E0E0"   # grey
      ),
      na.value = "transparent", 
      labels = c(
        "0.5"  = "50% core area",
        "0.95" = "95% area",
        "1"    = "Range limit"
      ),
      breaks = c("0.5", "0.95", "1"),
      drop = FALSE,
      name = "Distribution"
    ) +
    labs(
      title = paste0("Annual ", species_code, " distribution")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
      legend.title = element_text(size = 9),
      legend.text  = element_text(size = 8),
      axis.text = element_text(size = 10, color = "#000000"),
      axis.title   = element_blank(),
      panel.grid   = element_blank(),
      panel.background = element_rect(fill = "transparent"),
      plot.background  = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent")
    )
}


plot_ud_range_95 <- function(r, species_code, states) {
  
  ggplot() +
    geom_spatraster(
      data = r,
      aes(fill = factor(after_stat(value))),
      na.rm = TRUE
    ) +
    geom_spatvector(
      data = states,
      color = "#ffffff",
      fill  = "#8290AB",
      linewidth = 0.3
    ) +
    scale_fill_manual(
      values = c(
        "0.5"  = "#FEE08B",  # red
        "0.95" = "#FEE08B",  # yellow
        "1"    = "#91BFDB",  # light blue
        "1000"    = "#E0E0E0"   # grey
      ),
      na.value = "transparent", 
      labels = c(
        "0.5"  = "50% core area",
        "0.95" = "95% area",
        "1"    = "Range limit"
      ),
      breaks = c("0.5", "0.95", "1"),
      drop = FALSE,
      name = "Distribution"
    ) +
    labs(
      title = paste0("Annual ", species_code, " distribution")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
      legend.title = element_text(size = 9),
      legend.text  = element_text(size = 8),
      axis.text = element_text(size = 10, color = "#000000"),
      axis.title   = element_blank(),
      panel.grid   = element_blank(),
      panel.background = element_rect(fill = "transparent"),
      plot.background  = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent")
    )
}

COPE_annual = annual_densities[["COPE_annual"]]

p <- ggplot()+
  geom_spatraster(data = BVSH_annual, na.rm = TRUE)+
  geom_spatvector(data=states, color = "#ffffff", fill = "#8290AB")+
  scale_fill_continuous(na.value = "transparent") +  # Make NA values transparent
  theme(axis.text = element_text(size = 20, color = "#ffffff")) +
  theme_minimal()+
  labs(
    title = "Annual COPE distribution",
    fill = paste0("Density")
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),  # Center & style title
    legend.title = element_text(hjust = 0.5, size = 10) 
  )

p +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )


ggsave(here::here("scratch/COPE_rawmap.png"), plot = p, width = 10.5, height = 8, units = "in", dpi = 400, bg = "transparent")







  
  
  
plot_ud_range_only(
  r = uds[["PFSH_annual"]],
  species_code = "PFSH",
  states = states
)

plot_ud_range_95(
  r = uds[["PFSH_annual"]],
  species_code = "PFSH",
  states = states
)

plot_ud(
  r = uds[["PFSH_annual"]],
  species_code = "PFSH",
  states = states
)

