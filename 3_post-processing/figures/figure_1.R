############################################################################################
#    Figure 1: Create workflow for Maxent modelling procedure
#    By: Carla Archibald
#    Date: 01/03/23
############################################################################################

# 1. install libraries #####
library(tidyverse)
library(dplyr)
library(DiagrammeR)

# 2. Workflow schematic, create a graph object, and define nodes dataframe #####

grViz("digraph{
 
      graph[rankdir = LR]
  
      node[shape = rectangle, style = filled]
  
      node[fillcolor = '#fe8901', margin = 0.2]
      A[label = 'Data set']
      B[label = 'Lambda file']

      subgraph cluster_0 {
        graph[shape = rectangle]
        style = rounded
        bgcolor = '#b9b9b9'
    
        label = 'MaxEnt modelling procedure'
        node[shape = rectangle, fillcolor = '#f2f2f2', margin = 0.25]
        C[label = 'Model projections']
        D[label = 'Model fitting']
        E[label = 'Model validation']
        L[label = 'Ensemble averaging']

      }
  
      subgraph cluster_1 {
        graph[shape = rectangle]
        style = rounded
        bgcolor = '#3592a5'
    
        label = 'Variable selection'
        node[shape = rectangle, fillcolor = '#e3f2f5', margin = 0.25]
        F[label = 'Statistical importance']
        G[label = 'Ecologial importance']

      }
  
      subgraph cluster_2 {
        graph[shape = rectangle]
        style = rounded
        bgcolor = '#009587'
    
        label = 'Occurance points'
        node[shape = rectangle, fillcolor = '#e5f4f3', margin = 0.25]
        H[label = 'ALA point data']
        I[label = 'Background point data']
      }
  
      subgraph cluster_3 {
         graph[shape = rectangle]
         style = rounded
         bgcolor = '#009587'
    
         label = 'Environmental variables'
         node[shape = rectangle, fillcolor = '#e5f4f3', margin = 0.25]
         J[label = 'WorldClim bioclimatic data']
         K[label = 'Soil and Landscape Grid data']
      }
      
      
      edge[color = black, arrowhead = vee, arrowsize = 1.25]
      C -> A
      E -> C
      D -> E
      D -> F
      D -> G
      H -> D
      I -> D
      J -> D
      K -> D
      F -> C
      G -> C
      D -> B
      B -> C
      C -> L
      L -> A

      
      }")

# End