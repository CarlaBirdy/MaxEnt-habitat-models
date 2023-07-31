############################################################################################
#    Summarising AUC values for model validation of Maxent models
#    By: Carla Archibald
#    Date: 01/03/23
############################################################################################

# 1. Install and load required libraries and get working directory
library(tidyverse)  # Load the tidyverse collection of R packages for data manipulation and visualization
library(dplyr)      # Load dplyr, a part of the tidyverse, for data manipulation

current_directory <- getwd()  # Get the current working directory
current_directory             # Print the current working directory path

# 2. Read and process CSV files in the current directory #####

# Find all CSV files in the current directory (including sub-directories), this step can either pull the maxentResults.csv for each species, or tehre is a combines csv for each taxa in the GitHub
maxentFiles <- list.files(current_directory, pattern="*maxentResults*", recursive = TRUE, full.names = TRUE) 

# Combine data from all CSV files into a single data frame 'maxentResults'
maxentResults  <- maxentFiles %>%
  lapply(function(x) read_csv(x, show_col_types = FALSE)) %>% # Read each CSV file into a list
  bind_rows # Combine the list into a single data frame

# Summarize the AUC values by rounding to two decimal places and grouping by 'Training_AUC_2ROUND'
maxentResultsSum <- maxentResults%>%
  dplyr::select(Species,`Training AUC`)%>%                         # Select columns for Species and Training AUC
  dplyr::mutate(Training_AUC_2ROUND = round(`Training AUC`, 2))%>% # Round 'Training AUC' to two decimal places
  dplyr::arrange(desc(Training_AUC_2ROUND))%>%                     # Sort the data frame by descending rounded AUC values
  group_by(Training_AUC_2ROUND)%>%                                 # Group the data by rounded AUC values
  summarise(n = n(),                                               # Calculate the count of species in each group
            perc = (n/1355)*100)                                   # Calculate the percentage of species in each group

# Summarize AUC values by taxa and rounding to one decimal place
maxentResultsTaxaSum <- maxentResults%>%                           
  dplyr::select(Taxa, Species,`Training AUC`)%>%                   # Select columns for Taxa, Species, and Training AUC
  dplyr::mutate(Training_AUC_2ROUND = round(`Training AUC`, 1))%>% # Round 'Training AUC' to one decimal place
  dplyr::arrange(desc(Training_AUC_2ROUND))%>%                     # Sort the data frame by descending rounded AUC values
  group_by(Taxa,Training_AUC_2ROUND)%>%                            # Group the data by Taxa and rounded AUC values
  summarise(n = n(),                                               # Calculate the count of species in each group
            perc = (n/1355)*100)                                   # Calculate the percentage of species in each group

# Create a bar plot to visualize the distribution of species count based on rounded AUC values
ggplot(data=maxentResultsSum, aes(x=Training_AUC_2ROUND, y=n)) +
  geom_bar(stat="identity", fill = "#073e55")+        # Select any colour you like
  geom_vline(xintercept = 0.7, linetype = "dashed")+  # Add a vertical line at AUC threshold 0.7
  theme_bw(base_size = 16) +
  xlab("AUC values") +
  ylab("Number of species models")

# Save the bar plot as an image
ggsave(paste0(current_directory,"/auc_figure.png"), width=8.9, height=8.9, units="cm")
