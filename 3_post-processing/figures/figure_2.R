############################################################################################
#    Summarise occurrence points used in Maxent models
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

# 2. Data manipulation as needed 
maxentResults  <- maxentFiles %>%
  lapply(function(x) read_csv(x, col_types = cols_only(Species = col_character(), `#Training samples` = col_double()))) %>% 
  bind_rows

maxentResultsSum <- maxentResults%>%
  dplyr::select(Species,`#Training samples`)%>%
  dplyr::arrange(desc(`#Training samples`))%>%
  summarise(n = n(),
            perc = (n/1355)*100)

maxentResultsTaxaSum <- maxentResults %>%
  dplyr::select(Species,`#Training samples`)%>%
  dplyr::mutate(OCCUR_2ROUND = plyr::round_any(`#Training samples`, 10)) %>%
  dplyr::arrange(desc(OCCUR_2ROUND)) %>%
  select(OCCUR_2ROUND) %>%
  group_by(OCCUR_2ROUND) %>%
  dplyr::summarise(n = n(),
            perc = (n/10611)*100)

# 3. Occurrence Graph #####

ggplot(data=maxentResultsTaxaSum, aes(x=OCCUR_2ROUND, y=n)) +
  geom_bar(stat="identity", fill = "#073e55")+
  geom_vline(xintercept = 123, linetype = "dashed")+
  theme_bw(base_size = 24) +
  xlim(0,1000) +
  xlab("Occurrence points") +
  ylab("Number of species")

# Save the bar plot as an image
ggsave(paste0(current_directory,"/occur_figure.png"), width=8.9, height=8.9, units="cm")
