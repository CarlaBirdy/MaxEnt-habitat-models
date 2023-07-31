############################################################################################
#    Figure 3: High res view figure
#    By: Carla Archibald
#    Date: 01/03/23
############################################################################################

# 1. install libraries and get data directory#####
library(dplyr)      # Load the dplyr package for data manipulation
library(sp)         # Load sp for spatial data classes and methods
library(sf)         # Load sf for spatial data classes and methods
library(rgdal)      # Load rgdal for handling geographic data
library(tmap)       # Load tmap for thematic maps
library(stars)      # Load stars for raster data manipulation
library(raster)     # Load raster for raster data analysis
library(rasterVis)  # Load rasterVis for raster visualization
library(scales)     # Load scales for data transformation
library(tidyverse)  # Load tidyverse for data manipulation and visualization
library(ggspatial)  # Load ggspatial for spatial visualization with ggplot2
library(ggplot2)    # Load ggplot2 for data visualization
library(ggmap)      # Load ggmap for working with Google maps and ggplot2
library(ggpubr)     # Load ggpubr for combining ggplot2 plots
library(ggpattern)  # Load ggpattern for adding pattern fills in ggplot2

current_directory <- getwd()  # Get the current working directory, this should be the parent directory which contains all of the taxonomic folders
current_directory             # Print the current working directory path

# 2. read in data #####

# General summary using one species as an example
# current
# 4 future time periods

taxa <- "birds" # Set the taxa category to "birds" (you can change it to other categories like "amphibians," "reptiles," "mammals," or "plants")
species <- "Casuarius_casuarius" # Set the species folder to "Casuarius_casuarius" (you can choose other species folders)

# Define file paths
sp_wd <- paste0(current_directory,"/",taxa,"/",species)
out_wd <- current_directory

# Read shapefile state data
# Download the Australia state and territory shapefile: https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1259.0.30.001July%202011?OpenDocument
# Put the file in your currest directory
states <- st_read(paste0(current_directory,"STE11aAust.shp"))
states_gda94 <- st_set_crs(states, st_crs(4283))
states_gda94_sub <- states_gda94 %>%
  dplyr::filter(STATE_NAME == "Queensland")

# Read raster data for historic and future time periods
# Load the raster data for the historic baseline of a species at 5km resolution in Australia.
historic <- raster(paste0(sp_wd,"/",species,"_historic_baseline_1990_AUS_5km_EnviroSuit.tif"))

# Convert the raster data to stars format.
historic_subset_stars <- st_as_stars(historic)

# Define a new coordinate reference system (CRS) for the raster data as longlat with GRS80 ellipsoid.
new_crs <- st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

# Transform the historic raster data to the new CRS.
historic_subset_stars_proj <- st_transform(historic_subset_stars, new_crs)

# Load raster data for future time periods (2030, 2050, 2070, 2090) at 5km resolution in Australia.
future2030 <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp585_2030_AUS_5km_EnviroSuit.tif"))
future2050 <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp585_2050_AUS_5km_EnviroSuit.tif"))
future2070 <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp585_2070_AUS_5km_EnviroSuit.tif"))
future2090 <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp585_2090_AUS_5km_EnviroSuit.tif"))

# Convert the future raster data to stars format and transform them to the new CRS.
future2030_stars <- st_as_stars(future2030)
future2030_stars_proj <- st_transform(future2030_stars, new_crs)

future2050_stars <- st_as_stars(future2050)
future2050_stars_proj <- st_transform(future2050_stars, new_crs)

future2070_stars <- st_as_stars(future2070)
future2070_stars_proj <- st_transform(future2070_stars, new_crs)

future2090_stars <- st_as_stars(future2090)
future2090_stars_proj <- st_transform(future2090_stars, new_crs)

# 3. plot data #####

# Historical Suitability Plot
ggplot() +
  geom_stars(
    data = historic_subset_stars_proj,
    aes(fill = Casuarius_casuarius_historic_baseline_1990_AUS_5km_EnviroSuit),
    downsample = 2) +
  scale_fill_gradientn(name="Suitability",
                       limits = c(0, 100),
                       colors = c("white","#83edcb", "#3592a5","#073e55","#fe8901"),
                       values = c(0,0.25,0.5, 0.75,1), 
                       na.value = alpha("#c5d5e3", 0))+  
  geom_sf(data = states_gda94_sub, fill = alpha("white", 0), colour = "black") +
  theme_bw(base_size = 16) +
  scale_x_continuous(limits = c(141,151), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-9.5,-24), expand = c(0, 0)) +
  labs(x="Longitude", y="Latitude", color = "Legend") +
  theme(plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "#c5d5e3", colour="black"))+
  annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering)+
  annotation_scale(location = "bl", width_hint = 0.2, height_hint = 0.02, text_engine = "latex")
  
ggsave(paste0(current_directory,"figure_3a.png"), width=8.9, height=8.9, units="cm")

# 2030 Suitability RCP 8.5-SSP5 Suitability Plot
ggplot() +
  geom_stars(
    data = future2030_stars_proj,
    aes(fill = Casuarius_casuarius_GCM.Ensembles_ssp585_2030_AUS_5km_EnviroSuit_1),
    downsample = 2) +
  scale_fill_gradientn(name="Suitability",
                       limits = c(0, 100),
                       colors = c("white","#83edcb", "#3592a5","#073e55","#fe8901"),
                       values = c(0,0.25,0.5, 0.75,1), 
                       na.value = alpha("#c5d5e3", 0))+  
  geom_sf(data = states_gda94_sub, fill = alpha("white", 0), colour = "black")+
  theme_bw(base_size = 16) +
  scale_x_continuous(limits = c(141,151), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-9.5,-24), expand = c(0, 0)) +
  labs(x="Longitude", y="Latitude", color = "Legend") +
  theme(plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "#c5d5e3", colour="black"))

ggsave(paste0(current_directory,"figure_3b.png"), width=8.9, height=8.9, units="cm")

# 2090 Suitability RCP 8.5-SSP5 Suitability Plot

ggplot() +
  geom_stars(
    data = future2090_stars_proj,
    aes(fill = Casuarius_casuarius_GCM.Ensembles_ssp585_2090_AUS_5km_EnviroSuit_1),
    downsample = 2) +
  scale_fill_gradientn(name="Suitability",
                       limits = c(0, 100),
                       colors = c("white","#83edcb", "#3592a5","#073e55","#fe8901"),
                       values = c(0,0.25,0.5, 0.75,1), 
                       na.value = alpha("#c5d5e3", 0))+  
  geom_sf(data = states_gda94_sub, fill = alpha("white", 0), colour = "black")+
  theme_bw(base_size = 16) +
  scale_x_continuous(limits = c(141,151), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-9.5,-24), expand = c(0, 0)) +
  labs(x="Longitude", y="Latitude", color = "Legend") +
  theme(plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "#c5d5e3", colour="black"))

ggsave(paste0(current_directory,"figure_3c.png"), width=8.9, height=8.9, units="cm")

# End: Panel arrangement was done outside of R
