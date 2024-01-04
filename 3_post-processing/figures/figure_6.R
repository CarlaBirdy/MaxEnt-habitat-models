############################################################################################
#    Species level summary for maxent models
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

taxa <- "plants" # "mammals" # "amphibians" "birds", "reptiles", "mammals", "plants"
species <-  "Eucalyptus_pauciflora" # "Macropus_rufus" #"Notaden_bennettii" # "Notaden_melanoscaphus"# "Macropus_rufus" # "Pardalotus_punctatus" 

# set file paths
sp_wd <- paste0(current_directory,"/",taxa,"/",species)
out_wd <- current_directory

# 3. read in data #####

# Read raster data for historical data
historic <- raster(paste0(sp_wd,"/",species,"_historic_baseline_1990_AUS_5km_EnviroSuit.tif"))
names(historic) <- "Historical-Suitability"

# Read raster data for the 2030 prediction of future climate projections for different scenarios
future2030_ssp126_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp126_2030_AUS_5km_EnviroSuit.tif"))
names(future2030_ssp126_mean) <- "RCP1-SSP2.6_Mean"
future2030_ssp245_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp245_2030_AUS_5km_EnviroSuit.tif"))
names(future2030_ssp245_mean) <- "RCP2-SSP4.5_Mean"
future2030_ssp370_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp370_2030_AUS_5km_EnviroSuit.tif"))
names(future2030_ssp370_mean) <- "RCP3-SSP7.0_Mean"
future2030_ssp585_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp585_2030_AUS_5km_EnviroSuit.tif"))
names(future2030_ssp585_mean) <- "RCP5-SSP8.5_Mean"

# Read raster data for the 2050 prediction of future climate projections for different scenarios
future2050_ssp126_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp126_2050_AUS_5km_EnviroSuit.tif"))
names(future2050_ssp126_mean) <- "RCP1-SSP2.6_Mean"
future2050_ssp245_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp245_2050_AUS_5km_EnviroSuit.tif"))
names(future2050_ssp245_mean) <- "RCP2-SSP4.5_Mean"
future2050_ssp370_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp370_2050_AUS_5km_EnviroSuit.tif"))
names(future2050_ssp370_mean) <- "RCP3-SSP7.0_Mean"
future2050_ssp585_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp585_2050_AUS_5km_EnviroSuit.tif"))
names(future2050_ssp585_mean) <- "RCP5-SSP8.5_Mean"

# Read raster data for the 2070 prediction of future climate projections for different scenarios
future2070_ssp126_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp126_2070_AUS_5km_EnviroSuit.tif"))
names(future2070_ssp126_mean) <- "RCP1-SSP2.6_Mean"
future2070_ssp245_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp245_2070_AUS_5km_EnviroSuit.tif"))
names(future2070_ssp245_mean) <- "RCP2-SSP4.5_Mean"
future2070_ssp370_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp370_2070_AUS_5km_EnviroSuit.tif"))
names(future2070_ssp370_mean) <- "RCP3-SSP7.0_Mean"
future2070_ssp585_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp585_2070_AUS_5km_EnviroSuit.tif"))
names(future2070_ssp585_mean) <- "RCP5-SSP8.5_Mean"

# Read raster data for the 2090 prediction of future climate projections for different scenarios
future2090_ssp126_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp126_2090_AUS_5km_EnviroSuit.tif"))
names(future2090_ssp126_mean) <- "RCP1-SSP2.6_Mean"
future2090_ssp245_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp245_2090_AUS_5km_EnviroSuit.tif"))
names(future2090_ssp245_mean) <- "RCP2-SSP4.5_Mean"
future2090_ssp370_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp370_2090_AUS_5km_EnviroSuit.tif"))
names(future2090_ssp370_mean) <- "RCP3-SSP7.0_Mean"
future2090_ssp585_mean <- raster(paste0(sp_wd,"/",species,"_GCM-Ensembles_ssp585_2090_AUS_5km_EnviroSuit.tif"))
names(future2090_ssp585_mean) <- "RCP5-SSP8.5_Mean"

# --- Refugia calculations and mapping

# Calculate refugia between historic and 2030
refugia2030_ssp126_mean <- (future2030_ssp126_mean * historic)/100
names(refugia2030_ssp126_mean) <- "RCP1-SSP2.6_2030"
refugia2030_ssp245_mean <- (future2030_ssp245_mean * historic)/100
names(refugia2030_ssp245_mean) <- "RCP2-SSP4.5_2030"
refugia2030_ssp370_mean <- (future2030_ssp370_mean * historic)/100
names(refugia2030_ssp370_mean) <- "RCP3-SSP7.0_2030"
refugia2030_ssp585_mean <- (future2030_ssp585_mean * historic)/100
names(refugia2030_ssp585_mean) <- "RCP5-SSP8.5_2030"

# Calculate refugia between 2030 and 2050
refugia2050_ssp126_mean <- (future2050_ssp126_mean * future2030_ssp126_mean)/100
names(refugia2050_ssp126_mean) <- "RCP1-SSP2.6_2050"
refugia2050_ssp245_mean <- (future2050_ssp245_mean * future2030_ssp245_mean)/100
names(refugia2050_ssp245_mean) <- "RCP2-SSP4.5_2050"
refugia2050_ssp370_mean <- (future2050_ssp370_mean * future2030_ssp370_mean)/100
names(refugia2050_ssp370_mean) <- "RCP3-SSP7.0_2050"
refugia2050_ssp585_mean <- (future2050_ssp585_mean * future2030_ssp585_mean)/100
names(refugia2050_ssp585_mean) <- "RCP5-SSP8.5_2050"

# Calculate refugia between 2050 and 2070
refugia2070_ssp126_mean <- (future2070_ssp126_mean * future2050_ssp126_mean)/100
names(refugia2070_ssp126_mean) <- "RCP1-SSP2.6_2070"
refugia2070_ssp245_mean <- (future2070_ssp245_mean* future2050_ssp245_mean)/100
names(refugia2070_ssp245_mean) <- "RCP2-SSP4.5_2070"
refugia2070_ssp370_mean <- (future2070_ssp370_mean  * future2050_ssp370_mean)/100
names(refugia2070_ssp370_mean) <- "RCP3-SSP7.0_2070"
refugia2070_ssp585_mean <- (future2070_ssp585_mean * future2050_ssp585_mean)/100
names(refugia2070_ssp585_mean) <- "RCP5-SSP8.5_2070"

# Calculate refugia between 2070 and 2090
refugia2090_ssp126_mean <- (future2090_ssp126_mean * future2070_ssp126_mean)/100
names(refugia2090_ssp126_mean) <- "RCP1-SSP2.6_2090"
refugia2090_ssp245_mean <- (future2090_ssp245_mean* future2070_ssp245_mean)/100
names(refugia2090_ssp245_mean) <- "RCP2-SSP4.5_2090"
refugia2090_ssp370_mean <- (future2090_ssp370_mean  * future2070_ssp370_mean)/100
names(refugia2090_ssp370_mean) <- "RCP3-SSP7.0_2090"
refugia2090_ssp585_mean <- (future2090_ssp585_mean * future2070_ssp585_mean)/100
names(refugia2090_ssp585_mean) <- "RCP5-SSP8.5_2090"

# Stack the 2050 and 2090 the raster layers together
refugiaRas <- stack(refugia2050_ssp126_mean, refugia2050_ssp245_mean, refugia2050_ssp370_mean, refugia2050_ssp585_mean,
                    refugia2090_ssp126_mean, refugia2090_ssp245_mean, refugia2090_ssp370_mean, refugia2090_ssp585_mean)

# Set the Coordinate Reference System (CRS) for the stacked raster
crs(refugiaRas) <- CRS("+init=epsg:4283")

# 4. data manipulation and mapping

# Convert the stacked raster to a long-format data frame, removing NA values
raster_refugia_long_df <-
  as.data.frame(refugiaRas, xy = TRUE) %>%
  na.omit()%>%
  pivot_longer(
    c(-x, -y),
    names_to = "map",
    values_to = "refugia")%>%
  dplyr::mutate(scenario = str_replace(map, "_.*", "")) %>%
  dplyr::mutate(year = str_remove(map, ".*_")) %>%
  dplyr::mutate(year = str_remove(year, "(_[0-9]+)?\\..*$"))


# Create a plot using ggplot to visualize the data
ggplot() +
  geom_raster(data = raster_refugia_long_df, aes(x = x, y = y, fill = refugia)) +
  facet_grid(year~scenario , switch = "y") +
  scale_fill_gradientn(name="Refugia",
                       limits = c(0, 100),
                       colors = c("white","#83edcb","#073e55"), #"#009587","#009587"
                       values = c(0,0.5,1))+
  theme_bw(base_size = 16) +
  coord_cartesian(xlim = c(145,153), ylim = c(-40,-30))+
  xlab("Longitude") +
  ylab("Latitude") +
  theme(plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "#c5d5e3", colour="black"),
        legend.position = "right",
        legend.justification = "center",
        legend.box = "horizontal")

# save out the data
ggsave(paste0(current_directory,"figure_5a.png"), width=8.9, height=8.9, units="cm")

# --- Difference calculations and mapping

# Calculate the difference between historic and 2030
diff2030_ssp126_mean <- (future2030_ssp126_mean - historic)
names(diff2030_ssp126_mean) <- "RCP1-SSP2.6_2030"
diff2030_ssp245_mean <- (future2030_ssp245_mean - historic)
names(diff2030_ssp245_mean) <- "RCP2-SSP4.5_2030"
diff2030_ssp370_mean <- (future2030_ssp370_mean - historic)
names(diff2030_ssp370_mean) <- "RCP3-SSP7.0_2030"
diff2030_ssp585_mean <- (future2030_ssp585_mean - historic)
names(diff2030_ssp585_mean) <- "RCP5-SSP8.5_2030"

# Calculate the difference between historic and 2050
diff2050_ssp126_mean <- (future2050_ssp126_mean - historic)
names(diff2050_ssp126_mean) <- "RCP1-SSP2.6_2050"
diff2050_ssp245_mean <- (future2050_ssp245_mean - historic)
names(diff2050_ssp245_mean) <- "RCP2-SSP4.5_2050"
diff2050_ssp370_mean <- (future2050_ssp370_mean - historic)
names(diff2050_ssp370_mean) <- "RCP3-SSP7.0_2050"
diff2050_ssp585_mean <- (future2050_ssp585_mean - historic)
names(diff2050_ssp585_mean) <- "RCP5-SSP8.5_2050"

# Calculate the difference between historic and 2070
diff2070_ssp126_mean <- (future2070_ssp126_mean - historic)
names(diff2070_ssp126_mean) <- "RCP1-SSP2.6_2070"
diff2070_ssp245_mean <- (future2070_ssp245_mean - historic)
names(diff2070_ssp245_mean) <- "RCP2-SSP4.5_2070"
diff2070_ssp370_mean <- (future2070_ssp370_mean - historic)
names(diff2070_ssp370_mean) <- "RCP3-SSP7.0_2070"
diff2070_ssp585_mean <- (future2070_ssp585_mean - historic)
names(diff2070_ssp585_mean) <- "RCP5-SSP8.5_2070"

# Calculate the difference between historic and 2090
diff2090_ssp126_mean <- (future2090_ssp126_mean - historic)
names(diff2090_ssp126_mean) <- "RCP1-SSP2.6_2090"
diff2090_ssp245_mean <- (future2090_ssp245_mean - historic)
names(diff2090_ssp245_mean) <- "RCP2-SSP4.5_2090"
diff2090_ssp370_mean <- (future2090_ssp370_mean - historic)
names(diff2090_ssp370_mean) <- "RCP3-SSP7.0_2090"
diff2090_ssp585_mean <- (future2090_ssp585_mean - historic)
names(diff2090_ssp585_mean) <- "RCP5-SSP8.5_2090"

# Stack the 2050 and 2090 raster layers together
diffRas <- stack(diff2050_ssp126_mean, diff2050_ssp245_mean, diff2050_ssp370_mean, diff2050_ssp585_mean,
                 diff2090_ssp126_mean, diff2090_ssp245_mean, diff2090_ssp370_mean, diff2090_ssp585_mean)

# Set the Coordinate Reference System (CRS) for the stacked raster
crs(diffRas) <- CRS("+init=epsg:4283")

# Convert the stacked raster to a long-format data frame, removing NA values
raster_diff_long_df <-
  as.data.frame(diffRas, xy = TRUE) %>%
  na.omit()%>%
  pivot_longer(
    c(-x, -y),
    names_to = "map",
    values_to = "change")%>%
  dplyr::mutate(scenario = str_replace(map, "_.*", "")) %>%
  dplyr::mutate(year = str_remove(map, ".*_")) %>%
  dplyr::mutate(year = str_remove(year, "(_[0-9]+)?\\..*$"))


# Create a plot using ggplot to visualize the data
ggplot() +
  geom_raster(data = raster_diff_long_df, aes(x = x, y = y, fill = change)) +
  facet_grid(year~scenario , switch = "y") +
  scale_fill_gradient2(name="Change",
                       limits = c(-100, 100),
                       low = "#c71e1d",
                       mid = c("#FF6A00","white","#83edcb"),
                       high = "#073e55",
                       midpoint = 0) + # #25897c
  theme_bw(base_size = 16) + 
  coord_cartesian(xlim = c(145,153), ylim = c(-40,-30))+
  xlab("Longitude") +
  ylab("Latitude") +
  theme(plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "#c5d5e3", colour="black"),
        legend.position = "right",
        legend.justification = "center",
        legend.box = "horizontal")

# save out the data
ggsave(paste0(current_directory,"figure_5b.png"), width=8.9, height=8.9, units="cm")

# End: Slight adjustments labels were done outside of R
