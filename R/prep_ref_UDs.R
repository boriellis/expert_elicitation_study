




combine_seasons <-  function(densities){
 layer_names <- names(densities) 
 species <- sub("_.*$", "", layer_names)
 idx <- split(seq_along(layer_names), species)
 combined <- lapply(idx, function(i) {
   terra::app(densities[[i]], sum, na.rm = TRUE)
 })
 out <- terra::rast(combined)
 
 # set layer names to species names
 names(out) <- paste0(names(idx), "_annual")
 return(out)
}
  



  
normalize_annuals <- function(annuals){
  # global sum for each layer
  layer_sums <- terra::global(annuals, "sum", na.rm = TRUE)[, 1]
  
  # divide each layer by its corresponding sum
  annual_norm <- annuals / layer_sums  
}




make_UDs <- function(norms) {
  uds <- lapply(seq_len(terra::nlyr(norms)), function(i) {
    r <- norms[[i]]
    presence_thr <- global(r, "max", na.rm = TRUE)[[1]] * 0.01
    r[r < presence_thr] <- 0 #set to NA not 0
    r <- r / global(r, "sum", na.rm = TRUE)[[1]]
    # extract and sort values
    vals <- terra::values(r, na.rm = TRUE)
    vals <- sort(vals, decreasing = TRUE)
    
    # cumulative sum
    cumul <- cumsum(vals)
    
    # thresholds
    thr50 <- vals[which(cumul >= 0.5)[1]]
    thr95 <- vals[which(cumul >= 0.95)[1]]
    # classify
    terra::classify(
      norms[[i]],
      rcl = cbind(
        c(-Inf, presence_thr, thr95, thr50),
        c(presence_thr, thr95, thr50, Inf),
        c(1000, 1.0, 0.95, 0.5)
      ),
      right = TRUE
    )
  })
  
  out <- terra::rast(uds)
  names(out) <- names(norms)
  
  return(out)
}

plot_ud <- function(r, species_code, states) {
  
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
        "0.5"  = "#D73027",  # red
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







#old three step UD code:


# make_UDs <- function(norms) {
#   uds <- lapply(seq_len(terra::nlyr(norms)), function(i) {
#     r <- norms[[i]]
#     r[r < global(r, "max", na.rm = TRUE)[[1]] * 0.01] <- 0 #set to NA not 0
#     r <- r / global(r, "sum", na.rm = TRUE)[[1]]
#     # extract and sort values
#     vals <- terra::values(r, na.rm = TRUE)
#     vals <- sort(vals, decreasing = TRUE)
#     
#     # cumulative sum
#     cumul <- cumsum(vals)
#     
#     # thresholds
#     thr50 <- vals[which(cumul >= 0.5)[1]]
#     thr95 <- vals[which(cumul >= 0.95)[1]]
#     # classify
#     terra::classify(
#       norms[[i]],
#       rcl = cbind(
#         c(-Inf, thr95, thr50),
#         c(thr95, thr50, Inf),
#         c(1.0, 0.95, 0.5)
#       )
#     )
#   })
#   
#   out <- terra::rast(uds)
#   names(out) <- names(norms)
#   
#   return(out)
# }
# 
# 
# 
# 
# plot_ud <- function(r, species_code, states,
#                     cols = ud_cols,
#                     labs = ud_labs) {
#   
#   ggplot() +
#     geom_spatraster(
#       data = r,
#       aes(fill = factor(after_stat(value))),
#       na.rm = TRUE
#     ) +
#     geom_spatvector(
#       data = states,
#       color = "#ffffff",
#       fill  = "#8290AB",
#       linewidth = 0.3
#     ) +
#     scale_fill_manual(
#       values = cols,
#       labels = labs,
#       breaks = c("0.5", "0.95"), 
#       na.value = "transparent", 
#       na.translate = FALSE,
#       drop   = FALSE,
#       name   = "Utilization distribution"
#     ) +
#     labs(
#       title = paste0("Annual ", species_code, " distribution")
#     ) +
#     theme_minimal() +
#     theme(
#       plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
#       legend.title = element_text(size = 9),
#       legend.text  = element_text(size = 8),
#       axis.text = element_text(size = 10, color = "#000000"),
#       axis.title   = element_blank(),
#       panel.grid   = element_blank(),
#       panel.background = element_rect(fill = "transparent"),
#       plot.background  = element_rect(fill = "transparent", color = NA),
#       legend.background = element_rect(fill = "transparent"),
#       legend.box.background = element_rect(fill = "transparent")
#     )
# }
