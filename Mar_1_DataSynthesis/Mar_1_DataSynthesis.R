############ EFFICIENT AND BEAUTIFUL DATA SYNTHESIS ################

library(tidyverse)
library(ggthemes)
install.packages("ggalt") # for custom map projections
library(ggalt)
library(ggrepel)  # for annotations
library(viridis)  # for nice colours
library(broom)  # for cleaning up models
# devtools::install_github("wilkox/treemapify")
library(treemapify)  # for making area graphs
library(wesanderson)  # for nice colours

lter <- read.csv(here::here("Mar_1_DataSynthesis/CC", "lter.csv"))
niwot <- read.csv(here::here("Mar_1_DataSynthesis/CC", "niwot_plant_exp.csv"))

north_america <- map_data("world", region = c("USA", "Canada"))
north_america <- north_america[!(north_america$subregion %in% "Hawaii"), ]

(map1 <- ggplot()+
    geom_map(map = north_america, data = north_america,
             aes(long, lat, map_id = region), colour = "lightgray",
             fill = "lightgray", size = 0.3) +
    geom_point(data = lter, aes(x = long, y = lat, fill = ele), 
               alpha = 0.8, size = 4, colour = "gray", shape = 21))