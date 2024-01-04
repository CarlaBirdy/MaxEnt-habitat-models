############################################################################################
#    Figure 7: Graph habitat suitability over time and under different scenarios
#    By: Carla Archibald
#    Date: 01/03/23
############################################################################################

# 1. Install and load required libraries and get working directory
library(tidyverse)  # Load the tidyverse collection of R packages for data manipulation and visualization
library(dplyr)      # Load dplyr, a part of the tidyverse, for data manipulation
library(ggplot2)    # Load ggplot2 for data visualization

current_directory <- getwd()  # Get the current working directory
current_directory <- "N:\\Data-Master\\Biodiversity\\Environmental-suitability\\"

# Load the csv data for the species
csv <- read_csv(paste0(current_directory,"\\Quality-weighted-area-summary-all-species_AUS.csv"))   %>%  # To make this graph you can use the species summary sheet, or the sheet with all species
  filter(`Projection type` %in% c("mean"), `Climate scenario`%in% c("ssp126", "ssp245", "ssp370", "ssp585"))%>%
  dplyr::mutate(`Proportional change rounded` = round(`Proportional change in climate space from 1990 to Year (%)`, digits = -1))%>%
  group_by(`Year`,`Climate scenario`,`Proportional change rounded`)%>%
  summarise(count = n())


# Create a bar plot to visualize the distribution of species count based on rounded AUC values
ssp_pal <- c("#2F8FCE", "#114A76", "#DF9239","#CB503A")

ggplot(data = csv, aes(x = `Proportional change rounded`,y=count, fill = `Climate scenario`)) +
  geom_bar(stat = "identity") +  
  geom_vline(xintercept = 0, linetype = "solid") +  
  facet_grid(Year ~ `Climate scenario`) +
  xlab("% change of habitat suitability") +
  ylab("Number of species models") +
  scale_fill_manual(values = ssp_pal)+
  xlim(c(-100, 200)) +  # Adjust the x-axis limits as needed
  ylim(c(0, 2000)) +
  theme_bw(base_size = 16) 


# End: Panel arrangement was done outside of R
