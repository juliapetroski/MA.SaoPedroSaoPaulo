
# Create an object defining the geographic extent of the model domain

#### Set up ####

rm(list=ls())                                                   

Packages <- c("tidyverse", "sf", "stars", "rnaturalearth", "raster")        # List handy packages
lapply(Packages, library, character.only = TRUE)                            # Load packages

source("./R scripts/@_Region file.R")                                       # Define project region 

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = crs)                                                   # Assign polar projection

rocks <- read.csv("./Data/Rocks.csv") %>%                                   # Import coordinates
  rbind(slice_head(.)) %>%                                                  # Close the polygon by repeating first point
  as.matrix() %>%                                                           # Change format for shape()
  shape() %>%                                                               # Convert to sf
  st_transform(crs) 

hotspot <- read.csv("./Data/Fishing hotspot.csv") %>%                       # Import coordinates
  rbind(slice_head(.)) %>%                                                  # Close the polygon by repeating first point
  as.matrix() %>%                                                           # Change format for shape()
  shape() %>%                                                               # Convert to sf
  st_transform(crs) %>% 
  mutate(Region = "Fishing Hotspot")

MPA <- read.csv("./Data/MPA.csv") %>%                                       # Import coordinates
  rbind(slice_head(.)) %>%                                                  # Close the polygon by repeating first point
  as.matrix() %>%                                                           # Change format for shape()
  shape() %>%                                                               # Convert to sf
  st_transform(crs) %>% 
  mutate(Region = "MPA")

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = bind_rows(MPA, rocks, hotspot), aes(colour = Region), fill = NA, size = 0.25) + 
  theme_minimal() +
  zoom +
  theme(legend.position = "bottom") +
  NULL

ggsave_map("./Figures/bathymetry/Case study start.png", last_plot())
