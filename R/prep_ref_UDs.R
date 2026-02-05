




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
      r,
      rcl = cbind(
        c(-Inf, thr95, thr50),
        c(thr95, thr50, Inf),
        c(1.0, 0.95, 0.5)
      )
    )
  })
  
  out <- terra::rast(uds)
  names(out) <- names(norms)
  
  return(out)
}


#probably will want to add a function here for plotting - scratch code for that is in the make_ref_UDs.R script
  
  
