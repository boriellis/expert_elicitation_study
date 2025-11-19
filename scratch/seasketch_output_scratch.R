#this is just us testing out loading in seasketch output files
library("sf")
library("terra")


geo <- st_read(here::here("scratch/tupu_best_estimate.geojson.json"))
bath <- rast(here::here("data/bathymetry_gebco.tif"))



plot(bath)
gdalinfo(bath)
res(bath)
datatype(bath)
minmax(bath)
range(values(bath), na.rm = TRUE)
