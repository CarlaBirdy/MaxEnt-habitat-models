############################################################################################
#    Summarising AUC amd BI values for model validation of Maxent models
#    By: Carla Archibald
#    Date: 01/03/23
############################################################################################

# 1. Install and load required libraries and get working directory
library(tidyverse)  # Load the tidyverse collection of R packages for data manipulation and visualization
library(dplyr)      # Load dplyr, a part of the tidyverse, for data manipulation

current_directory <- getwd()  # Get the current working directory
current_directory             # Print the current working directory path

# Read in files 
maxentFiles <- list.files(current_directory, pattern="*maxentResults*", recursive = TRUE, full.names = TRUE) # maxentResults file containing the AUC
boyceResults  <- read_csv(current_directory, pattern="*boyceDf_all.csv", recursive = TRUE, full.names = TRUE) # boyce index file  containing the BI for all species.

# 2. AUC Graph #####

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
  geom_bar(stat="identity", fill = "#2b9d8f")+        # Select any colour you like
  geom_vline(xintercept = 0.7, linetype = "dashed")+  # Add a vertical line at AUC threshold 0.7
  theme_bw(base_size = 16) +
  xlab("AUC values") +
  ylab("Number of species models")

# Save the bar plot as an image
ggsave(paste0(current_directory,"/auc_figure.png"), width=8.9, height=8.9, units="cm")

# 3. Boyce Index Graph #####

boyceResultsThreshold <- boyceResults%>%
  mutate(Threshold = case_when(
    `Spearman.cor (the Boyce index value)` > 0.5  ~ "BI > 0.5",
    `Spearman.cor (the Boyce index value)` < 0  ~ "BI < 0",
    TRUE ~ "Other"))%>%  # An optional default case
  group_by(Threshold)%>%
  summarise(n = n())

median(boyceResults$`Spearman.cor (the Boyce index value)`)

boyceResultsSum <- boyceResults%>%
  dplyr::select(SPECIES,`Spearman.cor (the Boyce index value)`)%>%
  dplyr::mutate(Training_BI_2ROUND = round(`Spearman.cor (the Boyce index value)`, 1))%>%
  dplyr::arrange(desc(Training_BI_2ROUND))%>%
  group_by(Training_BI_2ROUND)%>%
  summarise(n = n())

ggplot(data=boyceResultsSum, aes(x=Training_BI_2ROUND, y=n)) +
  geom_bar(stat="identity", fill = "#2b9d8f")+  # Choose colout, e.g. #2b9d8f  #073e55
  geom_vline(xintercept = 0.97, linetype = "dashed", colour = c("black"))+
  geom_vline(xintercept = 0.5, linetype = "solid")+
  theme_bw(base_size = 16) +
  xlab("Boyce Index values") +
  ylab("Number of species models")

# Save the bar plot as an image
ggsave(paste0(current_directory,"/bi_figure.png"), width=8.9, height=8.9, units="cm")

# 4. Plotting AUC against BI #####
boyceResults<- boyceResults%>%
  rename(Species=SPECIES)

joined_tibble <- left_join(boyceResults, maxentResults, by = "Species")%>%
  dplyr::select(Species, Taxa, `Spearman.cor (the Boyce index value)`,`Training AUC`)

# Create a new variable to represent the quadrant
joined_tibble <- joined_tibble %>%
  mutate(Quadrant = case_when(
    `Spearman.cor (the Boyce index value)` > 0.5 & `Training AUC` > 0.7 ~ "AUC > 0.7, BI > 0.5",
    `Spearman.cor (the Boyce index value)` > 0.5 & `Training AUC` < 0.7 ~ "AUC < 0.7, BI > 0.5",
    `Spearman.cor (the Boyce index value)` < 0.5 & `Training AUC` > 0.7 ~ "AUC > 0.7, BI < 0.5",
    `Spearman.cor (the Boyce index value)` < 0.5 & `Training AUC` < 0.7 ~ "AUC < 0.7, BI < 0.5",
    TRUE ~ "Other"  # An optional default case
  ))

joined_tibbleSum <- joined_tibble%>%
  group_by(Quadrant)%>%
  summarise(n = n())

custom_palette <- c("AUC > 0.7, BI > 0.5" = "#2b9d8f", 
                    "AUC < 0.7, BI > 0.5" = "#073e55", 
                    "AUC > 0.7, BI < 0.5" = "#073e55", 
                    "AUC < 0.7, BI < 0.5" = "#FFC300")

ggplot(joined_tibble, aes(x = `Training AUC`, y = `Spearman.cor (the Boyce index value)`, color = Quadrant)) +
  geom_point(stat = "identity") +
  geom_vline(xintercept = 0.7, linetype = "solid") +
  geom_hline(yintercept = 0.5, linetype = "solid") +
  geom_vline(xintercept = 0.97, linetype = "dashed") +
  geom_hline(yintercept = 0.97, linetype = "dashed") +
  xlab("AUC value") +
  ylab("Boyce Index value") +
  theme_bw(base_size = 16,) +
  theme(legend.position = "none")+
  scale_color_manual(values = custom_palette)  # Remove the legend

# Save the bar plot as an image
ggsave(paste0(current_directory,"/auc-bi_figure.png"), width=8.9, height=8.9, units="cm")
