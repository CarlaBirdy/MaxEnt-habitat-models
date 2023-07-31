####################################################################################
#
# Graph the quality weighted area
#    By: Carla Archibald
#    Date: 14/1/2021
#    Method: - Calculate quality weighted area: For each species calculate the quality weighted area for each climate ensembal and time period
#            - Summarise quality weighted area: For each species quality weighted area value summarise for each climate ensembal and time period
#
####################################################################################

library(tidyr) # Easily Install and Load the 'Tidyverse'
library(readr)
library(dplyr)
library(stringr) # Simple, Consistent Wrappers for Common String Operations
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(viridis) # Default Color Maps from 'matplotlib' 
library(rasterVis)
library(RColorBrewer)
library(sf)
library(sp)

### 1. Set working dirs, load files

df <- list.files(path='N:/Planet-A/Data-Master/Biodiversity_priority_areas/Biodiversity/Species-summaries_20-year_snapshots-5km/', recursive = TRUE, full.names = TRUE, pattern = "Climate-niche-summary-total-*") %>% 
                lapply(read_csv) %>% 
                bind_rows
write.csv(df,"N:/Planet-A/Data-Master/Biodiversity_priority_areas/Biodiversity/Species-summaries_20-year_snapshots-5km/Climate-niche-summary-all-species_AUS.csv")
df <- read_csv("N:/Planet-A/Data-Master/Biodiversity_priority_areas/Biodiversity/Species-summaries_20-year_snapshots-5km/Climate-niche-summary-all-species_AUS.csv")

df_state <- list.files(path='N:/Planet-A/Data-Master/Biodiversity_priority_areas/Biodiversity/Species-summaries_20-year_snapshots-5km/', recursive = TRUE, full.names = TRUE, pattern = "Climate-niche-summary-by-state-*") %>% 
  lapply(read_csv) %>% 
  bind_rows
write.csv(df_state,"N:/Planet-A/Data-Master/Biodiversity_priority_areas/Biodiversity/Species-summaries_20-year_snapshots-5km/Climate-niche-summary-all-species-state_AUS.csv")

df_landuse <- list.files(path='N:/Planet-A/Data-Master/Biodiversity_priority_areas/Biodiversity/Species-summaries_20-year_snapshots-5km/', recursive = TRUE, full.names = TRUE, pattern = "Climate-niche-summary-by-land-use-*") %>% 
  lapply(read_csv) %>% 
  bind_rows
write.csv(df_landuse,"N:/Data-Master/Biodiversity_priority_areas/Biodiversity/Species-summaries_20-year_snapshots-5km/Climate-niche-summary-all-species-landuse_AUS.csv")

#df <- read_csv("N:/Planet-A/Data-Master/Biodiversity_priority_areas/Biodiversity/Species-summaries_20-year_snapshots-5km/Climate-niche-summary-all-species_AUS.csv")
df <- read_csv("N:/LUF-Modelling/Species-Distribution-Modelling/Data/Temporal_data/Quality-weighted-area-summary-all-species_AUS.csv")
View(head(df))
df_colnames <- colnames(df)

sspColPal <- c('#2f8fce','#114a76','#df9239','#cb503a')
pd <- position_dodge(3) # move them .05 to the left and right

## Climate space summaries

#--- T1: Climate suitability patterns through time
t1 <- df %>%
      dplyr::select("Taxon","Climate Scenario","Species","1990-2030 Proportional change in climate space (mean [%]","1990-2050 Proportional change in climate space (mean [%]","1990-2070 Proportional change in climate space (mean [%]","1990-2090 Proportional change in climate space (mean [%]")%>%
      dplyr::group_by(`Climate Scenario`,`Taxon`) %>%
      dplyr::summarize(n_species = n(),
                       n_2030_30 = sum(`1990-2030 Proportional change in climate space (mean [%]` < 70),
                       p_2030_30= (n_2030_30 / n_species)*100,
                       n_2050_30 = sum(`1990-2050 Proportional change in climate space (mean [%]` < 70),
                       p_2050_30= (n_2050_30 / n_species)*100,
                       n_2070_30 = sum(`1990-2070 Proportional change in climate space (mean [%]` < 70),
                       p_2070_30= (n_2070_30 / n_species)*100,
                       n_2090_30 = sum(`1990-2090 Proportional change in climate space (mean [%]` < 70),
                       p_2090_30= (n_2090_30 / n_species)*100,
                       n_2030_50 = sum(`1990-2030 Proportional change in climate space (mean [%]`< 50),
                       p_2030_50= (n_2030_50 / n_species)*100,
                       n_2050_50 = sum(`1990-2050 Proportional change in climate space (mean [%]`< 50),
                       p_2050_50= (n_2050_50 / n_species)*100,
                       n_2070_50 = sum(`1990-2070 Proportional change in climate space (mean [%]`< 50),
                       p_2070_50= (n_2070_50 / n_species)*100,
                       n_2090_50 = sum(`1990-2090 Proportional change in climate space (mean [%]`< 50),
                       p_2090_50= (n_2090_50 / n_species)*100,
                       n_2030_50 = sum(`1990-2030 Proportional change in climate space (mean [%]`< 20),
                       p_2030_50= (n_2030_50 / n_species)*100,
                       n_2050_50 = sum(`1990-2050 Proportional change in climate space (mean [%]`< 20),
                       p_2050_50= (n_2050_50 / n_species)*100,
                       n_2070_50 = sum(`1990-2070 Proportional change in climate space (mean [%]`< 20),
                       p_2070_50= (n_2070_50 / n_species)*100,
                       n_2090_50 = sum(`1990-2090 Proportional change in climate space (mean [%]`< 20),
                       p_2090_50= (n_2090_50 / n_species)*100) 


write.csv(t1, file="N:/Planet-A/Data-Master/Biodiversity_priority_areas/Biodiversity/Species-summaries_20-year_snapshots-5km/Climate-niche-summary-loss-bins_AUS.csv")
write.csv(t1_total, file="N:/Planet-A/Data-Master/Biodiversity_priority_areas/Biodiversity/Species-summaries_20-year_snapshots-5km/Climate-niche-summary-loss-bins-all_AUS.csv")

t3<-df %>%
  dplyr::select("Taxon","Climate Scenario","Species","Historical climate space [quality weighted km2]" ,df_colnames[9],df_colnames[10],df_colnames[11],df_colnames[12])%>%
  dplyr::group_by(`Climate Scenario`,`Taxon`,`Species`) %>%
  dplyr::summarize(Condition_2090 = ((`2090 Future quality weighted climate space (mean) [km2]`/`Historical quality weighted climate space [km2]`)*100))

#--- T2: Time period changes in climate suitability patterns through time
t2 <- df %>%
  dplyr::select("Taxon","Climate Scenario","Species",df_colnames[4],df_colnames[9],df_colnames[10],df_colnames[11],df_colnames[12])%>%
  dplyr::mutate("change_1990to2030" = if_else(((((`2030 Future quality weighted climate space (mean) [km2]`-`Historical quality weighted climate space [km2]`)/`Historical quality weighted climate space [km2]`)*100))<0,
                                          ((((`2030 Future quality weighted climate space (mean) [km2]`-`Historical quality weighted climate space [km2]`)/`Historical quality weighted climate space [km2]`)*100)),
                                               (((`2030 Future quality weighted climate space (mean) [km2]`-`2030 Future quality weighted climate space (mean) [km2]`)/`Historical quality weighted climate space [km2]`)*100)*-1)) %>%
  dplyr::mutate("change_2030to2050" = if_else(((((`2050 Future quality weighted climate space (mean) [km2]`-`2030 Future quality weighted climate space (mean) [km2]`)/`2030 Future quality weighted climate space (mean) [km2]`)*100))<0,
                                              ((((`2050 Future quality weighted climate space (mean) [km2]`-`2030 Future quality weighted climate space (mean) [km2]`)/`2030 Future quality weighted climate space (mean) [km2]`)*100)),
                                               (((`2050 Future quality weighted climate space (mean) [km2]`-`2030 Future quality weighted climate space (mean) [km2]`)/`2030 Future quality weighted climate space (mean) [km2]`)*100))) %>%
  dplyr::mutate("change_2050to2070" = if_else(((((`2070 Future quality weighted climate space (mean) [km2]`-`2050 Future quality weighted climate space (mean) [km2]`)/`2050 Future quality weighted climate space (mean) [km2]`)*100))<0,
                                              ((((`2070 Future quality weighted climate space (mean) [km2]`-`2050 Future quality weighted climate space (mean) [km2]`)/`2050 Future quality weighted climate space (mean) [km2]`)*100)),
                                               (((`2070 Future quality weighted climate space (mean) [km2]`-`2050 Future quality weighted climate space (mean) [km2]`)/`2050 Future quality weighted climate space (mean) [km2]`)*100))) %>% 
  dplyr::mutate("change_2070to2090" = if_else(((((`2090 Future quality weighted climate space (mean) [km2]`-`2070 Future quality weighted climate space (mean) [km2]`)/`2070 Future quality weighted climate space (mean) [km2]`)*100))<0,
                                              ((((`2090 Future quality weighted climate space (mean) [km2]`-`2070 Future quality weighted climate space (mean) [km2]`)/`2070 Future quality weighted climate space (mean) [km2]`)*100)),
                                               (((`2090 Future quality weighted climate space (mean) [km2]`-`2070 Future quality weighted climate space (mean) [km2]`)/`2070 Future quality weighted climate space (mean) [km2]`)*100))) %>%
  dplyr::select("Taxon","Climate Scenario","change_1990to2030","change_2030to2050","change_2050to2070","change_2070to2090") %>%
  dplyr::group_by(`Climate Scenario`,`Taxon`)%>%
  dplyr::summarise("avg_change_1990to2030"=median(`change_1990to2030`),
                   "avg_change_2030to2050"=median(`change_2030to2050`),
                   "avg_change_2050to2070"=median(`change_2050to2070`),
                   "avg_change_2070to2090"=median(`change_2070to2090`))%>%
  pivot_longer(!c("Taxon","Climate Scenario"), names_to = "timeperiod", values_to = "change")%>%
  pivot_wider(names_from = Taxon, values_from = change)

write.csv(t2, file="N:/Planet-A/Data-Master/Biodiversity_priority_areas/Biodiversity/Species-summaries_20-year_snapshots-5km/Climate-niche-summary-timeperiod-difference_AUS.csv")

#--- G1: Cumulative Histgram
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

df_cumhist <- df %>%
  dplyr::select(`Climate scenario`,Year, `Proportional change in climate space from 1990 to Year (%)`)%>%
  mutate(rounded = round_any(`Proportional change in climate space from 1990 to Year (%)`, 10)) %>%
  group_by(`Climate scenario`,Year,rounded) %>%
  summarize(count = n())%>%
  filter(!Year %in% c("1990"))

 
colour<- c(rep("red",20), rep("orange",30), rep("yellow",30))

df2 <- data.frame(`Climate Scenario` = rep("SSP 1 - RCP 2.6",5), Taxon = c("amphibians","birds","mammals","plants","reptiles"), mean_coverted = c(40,80,30,1600, 70), count = c(99,99,99,99,99))
sspColPalHist <- c('#2f8fce','#114a76','#df9239','#cb503a')

ggplot(df_cumhist, aes(x = rounded, y = count, fill=`Climate scenario`, color=`Climate scenario`)) +
  geom_histogram(stat = "identity") +
  labs(x = "Rounded Value", y = "Count") +
  facet_grid(Year~`Climate scenario`)+
  xlim(-100,200)+
  geom_vline(xintercept=0, color = "black")+
  scale_fill_manual(values=sspColPalHist,name = "Climate scenario", labels = c("SSP 1 - RCP 2.6", "SSP 2 - RCP 4.5","SSP 3 - RCP 7.0","SSP 5 - RCP 8.5"))+
  scale_colour_manual(values=sspColPalHist,name = "Climate scenario",guide="none")+	
  xlab("% change of habitat suitability") + 
  ylab("Number of species")+
  theme_bw() 

ggplot(df_cumhist)+
  geom_histogram(aes(x=mean_coverted, fill=`Climate Scenario order`, color=`Climate Scenario order`),position="identity", alpha=0.25, bins = 30)+
  facet_wrap(~Taxon, scales="free")+
  xlim(-100,200)+
  geom_vline(xintercept=0, color = "black")+
  scale_fill_manual(values=sspColPalHist,name = "Climate scenario")+
  scale_colour_manual(values=sspColPalHist,name = "Climate scenario")+	
  #geom_vline(xintercept=-30, color = "black")+
  #geom_vline(xintercept=-50, color = "black")+
  #geom_vline(xintercept=-80, color = "black")+
  #scale_x_discrete(limits=-100:-30)+
  xlab("Year") + 
  ylab("Change in climate space (%)")+
  theme_bw()

#--- G1: Climate suitability patterns through time


df %>%
  dplyr::select("Taxon","Climate Scenario",`1990-2030 Proportional change in climate space (mean [%]`,`1990-2050 Proportional change in climate space (mean [%]`,`1990-2070 Proportional change in climate space (mean [%]`,`1990-2090 Proportional change in climate space (mean [%]`)%>%
  pivot_longer(!c("Taxon","Climate Scenario"), names_to = "timeperiod", values_to = "mean")%>%
  dplyr::mutate(year = str_sub(timeperiod,6,9))%>%
  dplyr::mutate(mean_coverted = mean-100)%>%
  ggplot(., aes(year, mean_coverted, fill = factor(`Climate Scenario`))) + 
  geom_hline(yintercept=0, color = "black")+
  #geom_hline(yintercept=-30, linetype="dashed", color = "#f7c600")+
  #geom_hline(yintercept=-50, linetype="dashed", color = "#f6781d")+
  #geom_hline(yintercept=-80, linetype="dashed", color = "red")+
  #geom_hline(yintercept=-100, linetype="dashed", color = "grey")+
  geom_boxplot()+ #outlier.shape = NA
  ylim(-100,200)+
  facet_wrap(~Taxon, scales="free_y")+
  scale_fill_manual(values=sspColPal,name = "Climate scenario")+	
  xlab("Year") + 
  ylab("Change in climate space (%)")+
  theme_bw()


df_box <- df %>%
  dplyr::select(Taxon, `Climate Scenario`,`1990-2030 Proportional change in climate space (mean [%]`,`1990-2050 Proportional change in climate space (mean [%]`,`1990-2070 Proportional change in climate space (mean [%]`,`1990-2090 Proportional change in climate space (mean [%]`)%>%
  pivot_longer(!c(Taxon, `Climate Scenario`), names_to = "timeperiod", values_to = "mean")%>%
  dplyr::mutate(year = str_sub(timeperiod,6,9))%>%
  dplyr::mutate(mean_coverted = mean-100,
                "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp585" ,replacement = "SSP 5 - RCP 8.5"),
                "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp370" ,replacement = "SSP 3 - RCP 7.0"),
                "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp245" ,replacement = "SSP 2 - RCP 4.5"),
                "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp126" ,replacement = "SSP 1 - RCP 2.6"))%>%
  filter(`year` %in% c("2090"))


df %>%
  dplyr::select("Taxon","Climate Scenario",`1990-2030 Proportional change in climate space (mean [%]`,`1990-2050 Proportional change in climate space (mean [%]`,`1990-2070 Proportional change in climate space (mean [%]`,`1990-2090 Proportional change in climate space (mean [%]`)%>%
  pivot_longer(!c("Taxon","Climate Scenario"), names_to = "timeperiod", values_to = "mean")%>%
  dplyr::mutate(year = str_sub(timeperiod,6,9))%>%
  dplyr::mutate(mean_coverted = mean-100)%>%
  dplyr::mutate(mean_coverted = mean-100,
                "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp585" ,replacement = "SSP 5 - RCP 8.5"),
                "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp370" ,replacement = "SSP 3 - RCP 7.0"),
                "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp245" ,replacement = "SSP 2 - RCP 4.5"),
                "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp126" ,replacement = "SSP 1 - RCP 2.6"))%>%
  ggplot(., aes(year, mean_coverted, fill = factor(`Climate Scenario`))) + 
  geom_hline(yintercept=0, color = "black")+
  #geom_hline(yintercept=-30, linetype="dashed", color = "#f7c600")+
  #geom_hline(yintercept=-50, linetype="dashed", color = "#f6781d")+
  #geom_hline(yintercept=-80, linetype="dashed", color = "red")+
  #geom_hline(yintercept=-100, linetype="dashed", color = "grey")+
  geom_boxplot()+ #outlier.shape = NA
  ylim(-100,200)+
  facet_wrap(~Taxon, scales="free_y")+
  scale_fill_manual(values=sspColPal,name = "Climate scenario")+	
  xlab("Year") + 
  ylab("Change in climate space (%)")+
  theme_bw()

df %>%
  dplyr::select("Taxon","Climate Scenario",`1990-2030 Proportional change in climate space (max) [%]`,`1990-2050 Proportional change in climate space (max) [%]`,`1990-2070 Proportional change in climate space (max) [%]`,`1990-2090 Proportional change in climate space (max) [%]`)%>%
  pivot_longer(!c("Taxon","Climate Scenario"), names_to = "timeperiod", values_to = "max")%>%
  dplyr::mutate(year = str_sub(timeperiod,6,9))%>%
  dplyr::mutate(max_coverted = max-100)%>%
  ggplot(., aes(year, max_coverted, fill = factor(`Climate Scenario`))) + 
  geom_hline(yintercept=0, color = "black")+
  #geom_hline(yintercept=-30, linetype="dashed", color = "#f7c600")+
  #geom_hline(yintercept=-50, linetype="dashed", color = "#f6781d")+
  #geom_hline(yintercept=-80, linetype="dashed", color = "red")+
  #geom_hline(yintercept=-100, linetype="dashed", color = "grey")+
  geom_boxplot()+ #outlier.shape = NA
  ylim(-100,200)+
  facet_wrap(~Taxon, scales="free_y")+
  scale_fill_manual(values=sspColPal,name = "Climate scenario")+	
  xlab("Year") + 
  ylab("Change in climate space (%)")+
  theme_bw()

## Climate suitability patterns through space
#--- Q3: 1990-2100 Proportion of historical climate niche overlapping with climate refugia (mean) [%]
df %>%
  dplyr::select("Taxon","Climate Scenario",`1990-2100 Proportional climate space that is refugia (mean) [%]`)%>%
  pivot_longer(!c("Taxon","Climate Scenario"), names_to = "timeperiod", values_to = "mean")%>%
  dplyr::mutate(year = str_sub(timeperiod,1,4))%>%
  ggplot(., aes(year,mean, fill = factor(`Climate Scenario`))) + 
  geom_boxplot()+ #outlier.shape = NA
  facet_wrap(~Taxon)+
  scale_fill_manual(values=sspColPal,name = "Climate scenario")+	
  xlab("Climate scenario") + 
  ylab("Proportion of overlapping climate space")+
  theme_bw()+
  theme(axis.text.x=element_blank())

df %>%
  dplyr::select("Taxon","Climate Scenario",`1990-2100 Proportional climate space that is refugia (mean) [%]`)%>%
  pivot_longer(!c("Taxon","Climate Scenario"), names_to = "timeperiod", values_to = "mean")%>%
  dplyr::mutate(year = str_sub(timeperiod,1,4))%>%
  dplyr::mutate(mean_coverted = mean-100,
                "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp585" ,replacement = "SSP 5 - RCP 8.5"),
                "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp370" ,replacement = "SSP 3 - RCP 7.0"),
                "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp245" ,replacement = "SSP 2 - RCP 4.5"),
                "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp126" ,replacement = "SSP 1 - RCP 2.6"))%>%
  ggplot(., aes(year,mean, fill = factor(`Climate Scenario`))) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+ #outlier.shape = NA
  facet_wrap(~Taxon)+
  scale_fill_manual(values=sspColPal,name = "Climate scenario")+	
  xlab("Climate scenario") + 
  ylab("Proportion of overlapping climate space")+
  theme_bw()+
  theme(axis.text.x=element_blank())

                   
df_median <- df %>%
  dplyr::select(`Taxon`,`Climate Scenario`,`1990-2100 Proportional climate space that is refugia (mean) [%]`)%>%
  pivot_longer(!c(`Taxon`,`Climate Scenario`), names_to = "timeperiod", values_to = "Refigua") %>%
  dplyr::select(`Taxon`,`Climate Scenario`, `Refigua`)%>%
  dplyr::group_by(`Taxon`,`Climate Scenario`)%>%
  dplyr::summarise(Median = median(`Refigua`, na.rm=TRUE))                  

# 1990-2100 Proportion of historical climate niche not overlapping with climate refugia (mean) [%]

View(df %>%
  dplyr::select("Taxon","Climate Scenario",`1990-2100 Proportional climate space that increases in quality (mean) [%]`))

df_diff <- df %>%
       dplyr::select("Taxon","Climate Scenario",`1990-2100 Proportional climate space that increases in quality (mean) [%]`,`1990-2100 Proportional climate space that decreases in quality (mean) [%]`)%>%
       dplyr::group_by(`Climate Scenario`,`Taxon`) %>%
       dplyr::summarise(#n_species = n(),
                        median_increase  = median(`1990-2100 Proportional climate space that increases in quality (mean) [%]`, na.rm=TRUE),
                        #mean_increase  = mean(`1990-2100 Proportional climate space that increases in quality (mean) [%]`, na.rm=TRUE),
                        #sd_increase  = sd(`1990-2100 Proportional climate space that increases in quality (mean) [%]`, na.rm=TRUE),
                        median_decrease  = median(`1990-2100 Proportional climate space that decreases in quality (mean) [%]`, na.rm=TRUE))%>%
                        #mean_decrease  = mean(`1990-2100 Proportional climate space that decreases in quality (mean) [%]`, na.rm=TRUE),
                        #sd_decrease  = sd(`1990-2100 Proportional climate space that decreases in quality (mean) [%]`, na.rm=TRUE))%>%
  pivot_longer(!c("Taxon","Climate Scenario"), names_to = "changetype", values_to = "mean")%>%
  dplyr::mutate(year = 1990)

df_diff %>%
  dplyr::mutate(mean_graph = ifelse(`changetype` == "median_decrease", `mean` * -1, `mean`)) %>%
  dplyr::mutate(mean_coverted = mean-100,
                "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp585" ,replacement = "SSP 5 - RCP 8.5"),
                "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp370" ,replacement = "SSP 3 - RCP 7.0"),
                "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp245" ,replacement = "SSP 2 - RCP 4.5"),
                "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp126" ,replacement = "SSP 1 - RCP 2.6"))%>%
  ggplot(.) + 
    geom_col(aes(x = , `Climate Scenario`, y = mean_graph, fill = factor(`Climate Scenario`)), 
             stat = "identity", position = "dodge")+
    facet_wrap(~Taxon)+
    scale_fill_manual(values=sspColPal,name = "Climate scenario")+ 
    geom_hline(yintercept = 0, colour = "black")+	
  xlab("Climate scenario") + 
  ylab("Chaneg in cells quality")+
  theme_bw()+
  theme(axis.text.x=element_blank())



df_diff_median <- df_diff %>%
  dplyr::select(`Taxon`,`Climate Scenario`,`changetype`,`mean`)%>%
  dplyr::group_by(`Taxon`,`Climate Scenario`,`changetype`)%>%
  dplyr::summarise(Median = median(`mean`, na.rm=TRUE))   


df_diff %>%
  ggplot(.) + 
  geom_col(aes(x = `changetype`, y = mean, fill = factor(`Climate Scenario`)), 
           stat = "identity", position = "dodge")+
  facet_wrap(~Taxon)+
  scale_fill_manual(values=sspColPal,name = "Climate scenario")+ 
  geom_hline(yintercept = 0, colour = "black")+	
  xlab("Climate scenario") + 
  ylab("Chaneg in cells quality")+
  theme_bw()+
  theme(axis.text.x=element_blank())


df %>%
  dplyr::select("Taxon","Climate Scenario",`1990-2100 Proportional climate space that increases in quality (mean) [%]`)%>%
  pivot_longer(!c("Taxon","Climate Scenario"), names_to = "timeperiod", values_to = "mean")%>%
  dplyr::mutate(year = str_sub(timeperiod,1,4))%>%
  ggplot(., aes(year,mean, fill = factor(`Climate Scenario`))) + 
  geom_boxplot()+ #outlier.shape = NA
  facet_wrap(~Taxon)+
  scale_fill_manual(values=sspColPal,name = "Climate scenario")+	
  xlab("Climate scenario") + 
  ylab("Proportion of cells that increase in quality")+
  theme_bw()+
  theme(axis.text.x=element_blank())

df %>%
  dplyr::select("Taxon","Climate Scenario",`1990-2100 Proportional climate space that increases in quality (mean) [%]`)%>%
  pivot_longer(!c("Taxon","Climate Scenario"), names_to = "timeperiod", values_to = "mean")%>%
  dplyr::mutate(year = str_sub(timeperiod,1,4)) %>%
  dplyr::mutate(mean_coverted = mean-100,
                                                               "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp585" ,replacement = "SSP 5 - RCP 8.5"),
                                                               "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp370" ,replacement = "SSP 3 - RCP 7.0"),
                                                               "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp245" ,replacement = "SSP 2 - RCP 4.5"),
                                                               "Climate Scenario" = str_replace_all(`Climate Scenario`, pattern = "ssp126" ,replacement = "SSP 1 - RCP 2.6"))%>%
  ggplot(., aes(year,mean, fill = factor(`Climate Scenario`))) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+ #outlier.shape = NA
  facet_wrap(~Taxon)+
  scale_fill_manual(values=sspColPal,name = "Climate scenario")+	
  xlab("Climate scenario") + 
  ylab("Proportion of cells that increase in quality")+
  theme_bw()+
  theme(axis.text.x=element_blank())


df_median <- df %>%
  dplyr::select(`Taxon`,`Climate Scenario`,`1990-2100 Proportional climate space that is refugia (mean) [%]`)%>%
  pivot_longer(!c(`Taxon`,`Climate Scenario`), names_to = "timeperiod", values_to = "Refigua") %>%
  dplyr::select(`Taxon`,`Climate Scenario`, `Refigua`)%>%
  dplyr::group_by(`Taxon`,`Climate Scenario`)%>%
  dplyr::summarise(Median = median(`Refigua`, na.rm=TRUE))   

#--- Q6: 1990-2100 Proportional worsening in climate space (mean) [%]


View(df %>%
       dplyr::select("Taxon","Climate Scenario",`1990-2100 Proportional climate space that increases in quality (mean) [%]`)%>%
       dplyr::group_by(`Climate Scenario`,`Taxon`) %>%
       dplyr::summarise(n_species = n(),
                        median = median(`1990-2100 Proportional climate space that increases in quality (mean) [%]`, na.rm=TRUE),
                        mean = mean(`1990-2100 Proportional climate space that increases in quality (mean) [%]`, na.rm=TRUE),
                        sd = sd(`1990-2100 Proportional climate space that increases in quality (mean) [%]`, na.rm=TRUE)))

df %>%
  dplyr::select("Taxon","Climate Scenario",`1990-2100 Proportional climate space that decreases in quality (mean) [%]`)%>%
  pivot_longer(!c("Taxon","Climate Scenario"), names_to = "timeperiod", values_to = "mean")%>%
  dplyr::mutate(year = str_sub(timeperiod,1,4))%>%
  ggplot(., aes(year,mean, fill = factor(`Climate Scenario`))) + 
  geom_boxplot()+ #outlier.shape = NA
  facet_wrap(~Taxon)+
  scale_fill_manual(values=sspColPal, name = "Climate scenario")+	
  xlab("Climate scenario") + 
  ylab("Proportional of cells that decrease in quality")+
  theme_bw()+
  theme(axis.text.x=element_blank())

# 1990-2100 Proportional improvement in climate space (mean) [%]
View(df %>%
       dplyr::select("Taxon","Climate Scenario",`1990-2100 Proportional climate space that decreases in quality (mean) [%]`)%>%
       dplyr::group_by(`Climate Scenario`,`Taxon`) %>%
       dplyr::summarise(n_species = n(),
                        median = median(`1990-2100 Proportional climate space that decreases in quality (mean) [%]`, na.rm=TRUE),
                        mean = mean(`1990-2100 Proportional climate space that decreases in quality (mean) [%]`, na.rm=TRUE),
                        sd = sd(`1990-2100 Proportional climate space that decreases in quality (mean) [%]`, na.rm=TRUE)))

df %>%
  dplyr::select("Taxon","Climate Scenario",df_colnames[29])%>%
  pivot_longer(!c("Taxon","Climate Scenario"), names_to = "timeperiod", values_to = "mean")%>%
  dplyr::mutate(year = str_sub(timeperiod,1,4))%>%
  ggplot(., aes(year,mean, fill = factor(`Climate Scenario`))) + 
  geom_boxplot()+ #outlier.shape = NA
  facet_wrap(~Taxon)+
  ylim(0,100)+
  scale_fill_manual(values=sspColPal,name = "Climate scenario")+	
  xlab("Climate scenario") + 
  ylab("Proportional improvement in climate space")+
  theme_bw()+
  theme(axis.text.x=element_blank())



# STATE SUMMARIES
df_state %>%
  dplyr::select("Taxon","STATE_NAME","Climate Scenario",`1990-2030 Proportional change in climate space (mean [%]`,`1990-2050 Proportional change in climate space (mean [%]`,`1990-2070 Proportional change in climate space (mean [%]`,`1990-2090 Proportional change in climate space (mean [%]`)%>%
  dplyr::filter(`Climate Scenario` %in% c("ssp245"))%>%
  pivot_longer(!c("Taxon","STATE_NAME","Climate Scenario"), names_to = "timeperiod", values_to = "mean")%>%
  dplyr::mutate(year = str_sub(timeperiod,6,9))%>%
  dplyr::mutate(mean_coverted = mean-100)%>%
  ggplot(., aes(year, mean_coverted, fill = factor(STATE_NAME))) + 
  geom_hline(yintercept=0, color = "black")+
  #geom_hline(yintercept=-30, linetype="dashed", color = "#f7c600")+
  #geom_hline(yintercept=-50, linetype="dashed", color = "#f6781d")+
  #geom_hline(yintercept=-80, linetype="dashed", color = "red")+
  #geom_hline(yintercept=-100, linetype="dashed", color = "grey")+
  geom_boxplot()+ #outlier.shape = NA
  ylim(-100,200)+
  facet_wrap(~Taxon, scales="free_y")+
  #scale_fill_manual(values=sspColPal,name = "Climate scenario")+	
  xlab("Year") + 
  ylab("Change in climate space (%)")+
  theme_bw()

# STATE SUMMARIES DISTRIBUTIONS
stateSum <- df_state %>%
  dplyr::select(Species, Taxon, STATE_NAME,`Climate Scenario`,`2030 Distribution of climate space across States & Territories (mean [%]`,`2050 Distribution of climate space across States & Territories (mean [%]`,`2070 Distribution of climate space across States & Territories (mean [%]`,`2070 Distribution of climate space across States & Territories (mean [%]`,`2090 Distribution of climate space across States & Territories (mean [%]`)%>%
  pivot_longer(!c(Species, Taxon,STATE_NAME,`Climate Scenario`), names_to = "Timeperiod", values_to = "Percentage")%>%
  dplyr::group_by(Species,Taxon,`Climate Scenario`,Timeperiod)%>%
  summarise(sum(`Percentage`))


test <- df_state %>%
  dplyr::select(Species, Taxon, STATE_NAME,`Climate Scenario`,`2030 Distribution of climate space across States & Territories (mean [%]`,`2050 Distribution of climate space across States & Territories (mean [%]`,`2070 Distribution of climate space across States & Territories (mean [%]`,`2070 Distribution of climate space across States & Territories (mean [%]`,`2090 Distribution of climate space across States & Territories (mean [%]`)%>%
  pivot_longer(!c(Species, Taxon,STATE_NAME,`Climate Scenario`), names_to = "Timeperiod", values_to = "Percentage")%>%
  dplyr::group_by(Taxon,`Climate Scenario`,STATE_NAME,Timeperiod)%>%
  summarise(sum = sum(`Percentage`, na.rm=T),
            n = n()) %>%
  dplyr::mutate(perc = sum/n)%>%
  dplyr::mutate(year = str_sub(Timeperiod, 1,4))


df_state %>%
  dplyr::select(Species, Taxon, STATE_NAME,`Climate Scenario`,`2030 Distribution of climate space across States & Territories (mean [%]`,`2050 Distribution of climate space across States & Territories (mean [%]`,`2070 Distribution of climate space across States & Territories (mean [%]`,`2070 Distribution of climate space across States & Territories (mean [%]`,`2090 Distribution of climate space across States & Territories (mean [%]`)%>%
  pivot_longer(!c(Species, Taxon,STATE_NAME,`Climate Scenario`), names_to = "Timeperiod", values_to = "Percentage")%>%
  dplyr::group_by(Taxon,`Climate Scenario`,STATE_NAME,Timeperiod)%>%
  summarise(sum = sum(`Percentage`, na.rm=T),
            n = n()) %>%
  dplyr::mutate(perc = sum/n)%>%
  dplyr::mutate(year = str_sub(Timeperiod, 1,4))%>%
  ggplot(.) + 
  geom_col(aes(x = reorder(year, -perc), y = perc, fill = factor(`STATE_NAME`), 
           stat = "identity", position = "stack"))+
  scale_fill_viridis_d()+
  # geom_bar()+ #outlier.shape = NA
  facet_wrap(Taxon~`Climate Scenario`, ncol=4)+
  #scale_fill_manual(values=sspColPal,name = "Climate scenario")+	
  xlab("Year") + 
  ylab("Distribution of climate space across State & Territory")+
  theme_bw()



df_landuse %>%
  dplyr::select(Species, Taxon, LU_NAME,`Climate Scenario`,`2030 Distribution of climate space across NLUM 2010 land use types (mean [%]`,`2050 Distribution of climate space across NLUM 2010 land use types (mean [%]`,`2070 Distribution of climate space across NLUM 2010 land use types (mean [%]`,`2070 Distribution of climate space across NLUM 2010 land use types (mean [%]`,`2090 Distribution of climate space across NLUM 2010 land use types (mean [%]`)%>%
  pivot_longer(!c(Species, Taxon,LU_NAME,`Climate Scenario`), names_to = "Timeperiod", values_to = "Percentage")%>%
  dplyr::group_by(Taxon,`Climate Scenario`,LU_NAME,Timeperiod)%>%
  summarise(sum = sum(`Percentage`, na.rm=T),
            n = n()) %>%
  dplyr::mutate(perc = sum/n)%>%
  dplyr::mutate(year = str_sub(Timeperiod, 1,4))%>%
  ggplot(.) + 
  geom_col(aes(x = reorder(year, -perc), y = perc, fill = factor(`LU_NAME`), 
               stat = "identity", position = "stack"))+
  scale_fill_viridis_d()+
  # geom_bar()+ #outlier.shape = NA
  facet_wrap(Taxon~`Climate Scenario`, ncol=4)+
  #scale_fill_manual(values=sspColPal,name = "Climate scenario")+	
  xlab("Year") + 
  ylab("Distribution of climate space across Land Use Classes")+
  theme_bw()

df_state %>%
  dplyr::select("Species","Taxon","STATE_NAME","Climate Scenario",`2030 Distribution of climate space across States & Territories (mean [%]`,`2050 Distribution of climate space across States & Territories (mean [%]`,`2070 Distribution of climate space across States & Territories (mean [%]`,`2070 Distribution of climate space across States & Territories (mean [%]`,`2090 Distribution of climate space across States & Territories (mean [%]`)%>%
  dplyr::filter(`Climate Scenario` %in% c("ssp245"))%>%
  pivot_longer(!c("Taxon","STATE_NAME","Climate Scenario"), names_to = "timeperiod", values_to = "mean")%>%
  dplyr::mutate(year = str_sub(timeperiod,6,9))%>%
  dplyr::mutate(mean_coverted = mean)%>%
  ggplot(., aes(year, mean_coverted, fill = factor(STATE_NAME))) + 
  geom_hline(yintercept=0, color = "black")+
  #geom_hline(yintercept=-30, linetype="dashed", color = "#f7c600")+
  #geom_hline(yintercept=-50, linetype="dashed", color = "#f6781d")+
  #geom_hline(yintercept=-80, linetype="dashed", color = "red")+
  #geom_hline(yintercept=-100, linetype="dashed", color = "grey")+
  geom_boxplot()+ #outlier.shape = NA
  facet_wrap(~Taxon, scales="free_y")+
  #scale_fill_manual(values=sspColPal,name = "Climate scenario")+	
  xlab("Year") + 
  ylab("Change in climate space (%)")+
  theme_bw()



df_state %>%
  dplyr::select(`Taxon`,`STATE_NAME`,`Climate Scenario`,`1990-2100 Distribution of climate refugia across States & Territories (max) [%]`)%>%
  group_by(`Taxon`,`STATE_NAME`,`Climate Scenario`)%>%
  dplyr::summarise(median = median(`1990-2100 Distribution of climate refugia across States & Territories (max) [%]`[`1990-2100 Distribution of climate refugia across States & Territories (max) [%]`>0], na.rm=TRUE)) %>%
  dplyr::filter(`Climate Scenario` %in% c("ssp245"))%>%
  dplyr::mutate(year = 1990)%>%
  ggplot(., aes(median, fill = factor(`STATE_NAME`))) + 
  geom_col(aes(x = `Climate Scenario`, y = median), 
           stat = "identity", position = "stack")+
 # geom_bar()+ #outlier.shape = NA
  facet_wrap(~Taxon)+
  #scale_fill_manual(values=sspColPal,name = "Climate scenario")+	
  xlab("Climate scenario") + 
  ylab("Proportion of overlapping climate space")+
  theme_bw()+
  theme(axis.text.x=element_blank())

