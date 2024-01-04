############################################################################################
#    Calculate Boyce index for model validation
#    By: Carla Archibald
#    Date: 05/10/23
############################################################################################

# 1. install libraries #####
library(tidyverse)  # Collection of R packages for data manipulation and visualization
library(dplyr).    # Part of tidyverse, for data manipulation
library(ecospat).  # Functions for the support of spatial ecology analyses 
library(rgdal)     # for vector work; sp package should always load with rgdal 
library (raster)   # for metadata/attributes- vectors or rasters
library(sf)        # A package that provides simple features access for R

# species, select a species. 

spp <- "Arenophryne_rotunda"
taxa <- "amphibians"
  
wd <- # define your working directory path

# Bring in species historical projection data
hist_proj <- raster(paste0(wd, "/", taxa, "/", spp, "/", spp,"_historic_baseline_1990_AUS_5km_EnviroSuit.tif"))

# Bring in species occurrence point data
occur_point <- read.csv(paste0(wd, "/", taxa, "/", spp, "/occur.csv")) %>% 
               st_as_sf(coords=c("lon", "lat"), crs=4326)%>%
               dplyr::select(SPPCODE)%>%
               mutate(OCCUR = 1)

# boyce index
boyce_index <- ecospat.boyce(fit = hist_proj, occur_point, PEplot = TRUE)

F.ratio <- as.tibble(boyce_index$F.ratio)%>%
           rename("F.ratio (the predicted-to-expected ratio for each class-interval)"="value")%>%
           mutate(SPECIES = spp)%>%
           select(SPECIES,"F.ratio (the predicted-to-expected ratio for each class-interval)")
  
cor <- as.tibble(boyce_index$cor)%>%
  rename("Spearman.cor (the Boyce index value)"="value")%>%
  mutate(SPECIES = spp)%>%
  select(SPECIES,"Spearman.cor (the Boyce index value)")

write.csv(F.ratio, (paste0(wd, "/", taxa, "/", spp, "/boyce_index_fratio.csv", row.names=FALSE)))
write.csv(cor, (paste0(wd, "/", taxa, "/", spp, "/boyce_index_score.csv", row.names=FALSE)))

# End, I calculated this value for all species using a parallell processing, then I aggregated all species values to summarise in the manuscript.
