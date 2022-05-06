
## Set repeated commands specific to the project region
## This version is parameterised for the Barents sea

library(sf)

#EPSG <- rgdal::make_EPSG()
#EPSG2 <- dplyr::filter(EPSG, stringr::str_detect(note, "Brazil"))
crs <- 5530                                                              # Specify the map projection for the project

lims <- c(xmin = -44.25, xmax = -10, ymin = -14, ymax = 14)# Specify limits of plotting window, also used to clip data grids

zoom <- coord_sf(xlim = c(lims[["xmin"]], lims[["xmax"]]), ylim = c(lims[["ymin"]], lims[["ymax"]]), expand = FALSE) # Specify the plotting window for SF maps in this region

ggsave_map <- function(filename, plot) {
  ggsave(filename, plot, scale = 1, width = 12, height = 10, units = "cm", dpi = 500, bg = "white")
  
}                             # Set a new default for saving maps in the correct size
pre <- list(scale = 1, width = 12, height = 10, units = "cm", dpi = 500) # The same settings if you need to pass them to a function in MiMeMo.tools

SDepth <- 60
DDepth <- 600

#### bathymetry.5 MODEL DOMAIN ####

shape <- function(matrix) {
  
shape <-  matrix %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(Region = "St Peter and St Paul",.)
  st_crs(shape) <- st_crs(4326)                                        
  shape <- st_transform(shape, crs = crs)
  return(shape)
  
}                      # Convert a matrix of lat-lons to an sf polygon

#### expand polygon for sampling rivers ####

river_expansion <- matrix(c(13, 73,
                            0, 80,
                            0, 85,
                            63, 85,
                            73, 77,
                            30, 71,
                            13, 73),
                          ncol = 2, byrow = T) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(Region = "St Peter and St Paul",.)
st_crs(river_expansion) <- st_crs(4326)                                          
river_expansion <- st_transform(river_expansion, crs = crs)

