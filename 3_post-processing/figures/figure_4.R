############################################################################################
#    Figure 4: Species level summary for maxent models
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

# 2. set directories#####

current_directory <- getwd()  # Get the current working directory, this should be the parent directory which contains all of the taxonomic folders
current_directory             # Print the current working directory path

# select which species to map
taxa <- "mammals"# "amphibians" "birds", "reptiles", "mammals", "plants"
species <-  "Macropus_rufus" # species name

# set file paths
sp_wd <- paste0(current_directory,"/",taxa,"/",species)
out_wd <- current_directory
 
# 3. read in data #####

# Read raster data for the MAX prediction of future climate projections for different scenarios and time periods
future2090_ssp126_max <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp126_2090_AUS_5km_EnviroSuit_max.tif"))
names(future2090_ssp126_max) <- "RCP1-SSP2.6_Maximum"
future2090_ssp245_max <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp245_2090_AUS_5km_EnviroSuit_max.tif"))
names(future2090_ssp245_max) <- "RCP2-SSP4.5_Maximum"
future2090_ssp370_max <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp370_2090_AUS_5km_EnviroSuit_max.tif"))
names(future2090_ssp370_max) <- "RCP3-SSP7.0_Maximum"
future2090_ssp585_max <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp585_2090_AUS_5km_EnviroSuit_max.tif"))
names(future2090_ssp585_max) <- "RCP5-SSP8.5_Maximum"

# Read raster data for the MEAN prediction of future climate projections for different scenarios and time periods
future2090_ssp126_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp126_2090_AUS_5km_EnviroSuit.tif"))
names(future2090_ssp126_mean) <- "RCP1-SSP2.6_Mean"
future2090_ssp245_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp245_2090_AUS_5km_EnviroSuit.tif"))
names(future2090_ssp245_mean) <- "RCP2-SSP4.5_Mean"
future2090_ssp370_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp370_2090_AUS_5km_EnviroSuit.tif"))
names(future2090_ssp370_mean) <- "RCP3-SSP7.0_Mean"
future2090_ssp585_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp585_2090_AUS_5km_EnviroSuit.tif"))
names(future2090_ssp585_mean) <- "RCP5-SSP8.5_Mean"

# Read raster data for the MIN prediction of future climate projections for different scenarios and time periods
future2090_ssp126_min <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp126_2090_AUS_5km_EnviroSuit_min.tif"))
names(future2090_ssp126_min) <- "RCP1-SSP2.6_Minimum"
future2090_ssp245_min <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp245_2090_AUS_5km_EnviroSuit_min.tif"))
names(future2090_ssp245_min) <- "RCP2-SSP4.5_Minimum"
future2090_ssp370_min <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp370_2090_AUS_5km_EnviroSuit_min.tif"))
names(future2090_ssp370_min) <- "RCP3-SSP7.0_Minimum"
future2090_ssp585_min <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp585_2090_AUS_5km_EnviroSuit_min.tif"))
names(future2090_ssp585_min) <- "RCP5-SSP8.5_Minimum"

# Stack all the raster layers together
stackRas <- stack(future2090_ssp126_min,future2090_ssp245_min,future2090_ssp370_min,future2090_ssp585_min,
                  future2090_ssp126_mean,future2090_ssp245_mean,future2090_ssp370_mean,future2090_ssp585_mean,
                  future2090_ssp126_max,future2090_ssp245_max,future2090_ssp370_max,future2090_ssp585_max)

# Set the Coordinate Reference System (CRS) for the stacked raster
crs(stackRas) <- CRS("+init=epsg:4283")

# 4. data manipulation and mapping

# Convert the stacked raster to a long-format data frame, removing NA values
raster_long_df <-
  as.data.frame(stackRas, xy = TRUE) %>%
  na.omit()%>%
  pivot_longer(
    c(-x, -y),
    names_to = "map",
    values_to = "suitability")%>%
  dplyr::mutate(scenario = str_replace(map, "_.*", "")) %>%
  dplyr::mutate(projection = str_remove(map, ".*_"))
  
# Create a plot using ggplot to visualize the data
ggplot() +
  geom_raster(data = raster_long_df, aes(x = x, y = y, fill = suitability)) +
  facet_grid(projection ~ scenario, switch = "y") +
  coord_equal() +
  scale_fill_gradientn(name="Suitability",
                       limits = c(0, 100),
                       colors = c("white","#83edcb", "#3592a5","#073e55","#fe8901"),
                       values = c(0,0.25,0.5, 0.75,1))+  
  theme_bw() + 
  xlab("Longitude") +
  ylab("Latitude") +
  theme(plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "#c5d5e3", colour="black"))

#save out the data
ggsave(paste0(current_directory,"figure_4.png"), width=8.9, height=8.9, units="cm")

# End: Slight adjustments labels were done outside of R
