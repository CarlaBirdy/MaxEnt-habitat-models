############################################################################################
#    Figure 2: Summarising variable selection statistics for model validation of Maxent models
#    By: Carla Archibald
#    Date: 01/03/23
############################################################################################

# Load required libraries
library(tidyverse)  # Collection of R packages for data manipulation and visualization
library(dplyr)     # Part of tidyverse, for data manipulation

# 1. Get the current working directory #####
current_directory <- getwd()  # Get the current working directory
current_directory  # Print the current working directory path

# 2. Read and process CSV files in the current directory #####

# Find all CSV files in the current directory (including sub-directories), this step can either pull the maxentResults.csv for each species, or tehre is a combines csv for each taxa in the GitHub
maxentFiles <- list.files(current_directory, pattern="*maxentResults*", recursive = TRUE, full.names = TRUE) 

# Combine all the CSV files into a single data frame
maxentDf  <- maxentFiles %>%
  lapply(function(x) read_csv(x, show_col_types = FALSE)) %>%  # Read each CSV file into a list
  bind_rows # Combine the list into a single data frame

# Select relevant columns from the combined data frame
maxentDfContributionSub <- maxentDf[,c(1,2,4:21)]
maxentDfImportanceSub <- maxentDf[, c(1, 2, 22:39)]

# 3. Summarize contribution data #####

# Convert data from wide to long format
maxentDfContributionSummary <- maxentDfContributionSub %>%
  tidyr::pivot_longer(!c(Taxa,Species), names_to = "Variable", values_to = "Contribution")%>%
  group_by(Variable)%>% 
  summarise(n = sum(Contribution, na.rm = TRUE),            # Convert data from wide to long format
            mean = mean(Contribution, na.rm = TRUE),        # Calculate the mean of contributions
            median = median(Contribution, na.rm = TRUE))%>% # Calculate the median of contributions
  arrange(desc(median)) # Sort the summary by descending median

print(maxentDfContributionSummary) # Print the summary table

# Save the contribution summary to a CSV file
write.csv(maxentDfContributionSummary,paste0(current_directory,"/Contribution-Summary.csv"))

# 4. Summarize importance data #####

# Convert data from wide to long format
maxentDfImportanceSummary <- maxentDfImportanceSub %>%
  tidyr::pivot_longer(!c(Taxa,Species), names_to = "Variable", values_to = "Importance")%>%
  group_by(Variable)%>% 
  summarise(n = sum(Importance, na.rm = TRUE),            # Calculate the total number of importances
            mean = mean(Importance, na.rm = TRUE),        # Calculate the mean of importances
            median = median(Importance, na.rm = TRUE))%>% # Calculate the median of importances
  arrange(desc(median)) # Sort the summary by descending median

print(maxentDfImportanceSummary) # Print the summary table

# Save the importance summary to a CSV file
write.csv(maxentDfImportanceSummary,paste0(current_directory,"/Importance-Summary.csv"))
