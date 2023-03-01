############ EFFICIENT AND BEAUTIFUL DATA SYNTHESIS ################

library(tidyverse)
library(ggthemes)
library(ggalt) # for custom map projections
library(ggrepel)  # for annotations
library(viridis)  # for nice colours
library(broom)  # for cleaning up models
# devtools::install_github("wilkox/treemapify")
library(treemapify)  # for making area graphs
library(wesanderson)  # for nice colours

lter <- read.csv(here::here("Mar_1_DataSynthesis/CC", "lter.csv"))
niwot <- read.csv(here::here("Mar_1_DataSynthesis/CC", "niwot_plant_exp.csv"))
bird_pops <- read.csv(here::here("Mar_1_DataSynthesis/CC", "bird_pops.csv"))
bird_traits <- read.csv(here::here("Mar_1_DataSynthesis/CC", "elton_birds.csv"))

north_america <- map_data("world", region = c("USA", "Canada"))
north_america <- north_america[!(north_america$subregion %in% "Hawaii"), ]

(map1 <- ggplot()+
    geom_map(map = north_america, data = north_america,
             aes(long, lat, map_id = region), colour = "lightgray",
             fill = "lightgray", size = 0.3) +
    geom_point(data = lter, aes(x = long, y = lat, fill = ele), 
               alpha = 0.8, size = 4, colour = "gray", shape = 21))

(map2 <- ggplot()+
    geom_map(map = north_america, data = north_america,
             aes(map_id = region, y=lat, x=long), colour = "lightgray",
             fill = "lightgray", size = 0.3) +
    geom_point(data = lter, aes(x = long, y = lat, fill = ele), 
               alpha = 0.8, size = 4, colour = "gray", shape = 21) +
    coord_cartesian(ylim = c(25,80), xlim = c(-175, -50))) +
    theme_minimal()
#Try using geom_polygon instead

(map3 <- ggplot()+
    geom_map(map = north_america, data = north_america,
             aes(map_id = region), colour = "lightgray",
             fill = "lightgray", size = 0.3) +
    geom_point(data = lter, aes(x = long, y = lat, fill = ele), 
               alpha = 0.8, size = 4, colour = "gray", shape = 21) +
    coord_cartesian(ylim = c(25,80), xlim = c(-175, -50)) +
  theme_minimal() +
    geom_label_repel(data = lter, aes(x = long, y = lat, label = site), 
                     box.padding = 1, size = 4, nudge_x = 1, nudge_y = 1))

#subset out specific sites
#subset out sites with elevation greater than 2000
(map4 <- ggplot()+
    geom_map(map = north_america, data = north_america,
             aes(map_id = region), colour = "lightgray",
             fill = "lightgray", size = 0.3) +
    geom_point(data = lter, aes(x = long, y = lat, fill = ele), 
               alpha = 0.8, size = 4, colour = "gray", shape = 21) +
    coord_cartesian(ylim = c(25,80), xlim = c(-175, -50)) +
    theme_minimal() +
    geom_label_repel(data = subset(lter, ele > 2000), aes(x = long, y = lat, label = site), 
                     box.padding = 1, size = 4, nudge_x = 1, nudge_y = 1))

#make this map nicer :)
(map5 <- ggplot()+
    geom_map(map = north_america, data = north_america,
             aes(map_id = region), colour = "lightgray",
             fill = "lightgray", size = 0.3) +
    geom_point(data = lter, aes(x = long, y = lat, fill = ele), 
               alpha = 0.8, size = 4, colour = "gray", shape = 21) +
    coord_cartesian(ylim = c(25,80), xlim = c(-175, -50)) +
    theme_minimal() +
    geom_label_repel(data = subset(lter, ele > 2000), aes(x = long, y = lat, label = site), 
                     box.padding = 1, size = 4, nudge_x = 1, nudge_y = 1) +
    labs(fill = "elevation (m)")+
    annotate("text", x = -150, y = 35, colour = "#553C7F", label = "Example \ntext")+
    scale_fill_viridis(option = "magma"))

#Making violin plots (or raincloud plots?) mixed with boxplots
theme_niwot <- function(){
  theme_bw() +
    theme(text = element_text(family = "Helvetica Light"),
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
          plot.title = element_text(size = 18, vjust = 1, hjust = 0),
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.position = c(0.95, 0.15),
          legend.key = element_blank(),
          legend.background = element_rect(color = "black",
                                           fill = "transparent",
                                           size = 2, linetype = "blank"))
}

#Calculate and plot richness
niwot_richness <- niwot %>% 
  group_by(plot_num, year) %>% 
  mutate(richness=length(unique(USDA_Scientific_Name))) %>% 
  ungroup()

#Wider = more data points within that richness band
(niwot1 <- ggplot(niwot_richness, aes(x = fert, y = richness))+
    geom_violin())

(niwot2 <- ggplot(niwot_richness, aes(x = fert, y = richness))+
    geom_violin(aes(fill = fert, colour = fert), alpha = 0.5)+
    theme_niwot())

#add a boxplot
(niwot3 <- ggplot(niwot_richness, aes(x = fert, y = richness))+
    geom_violin(aes(fill = fert, colour = fert), alpha = 0.5)+
    theme_niwot()+
    geom_boxplot(aes(colour = fert), width = 0.2))

#add a jitter
(niwot4 <- ggplot(niwot_richness, aes(x = fert, y = richness))+
    geom_violin(aes(fill = fert, colour = fert), alpha = 0.5)+
    theme_niwot()+
    geom_jitter(aes(colour = fert), position = position_jitter(0.1), alpha = 0.3))

#add jitter and the boxplot
#Reorder levels to descending richness levels, half a violin 
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

(niwot5 <- ggplot(niwot_richness, aes(x = reorder(fert, desc(richness)), y = richness, fill = fert))+
    geom_flat_violin(alpha = 0.8, position = position_nudge(x = 0.2, y = 0))+
    theme_niwot()+
    geom_point(aes (y = richness, colour = fert), position = position_jitter(width = 0.15),
               size = 1, alpha = 0.1)+
    geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) +
    labs(y = "species richness", x = NULL) +
    guides(fill = FALSE, colour = FALSE)+
    scale_fill_manual(values = c("#5A4A6F", "#E47250",  "#EBB261", "#9D5A6C")) +
    scale_colour_manual(values = c("#5A4A6F", "#E47250",  "#EBB261", "#9D5A6C")))

#We are going to flip the plot (switches whats on the x axis with whats on the y axis)
#Easiest to make the plot the default way and then flip it at the end
(niwot6 <- ggplot(niwot_richness, aes(x = reorder(fert, desc(richness)), y = richness, fill = fert))+
    geom_flat_violin(alpha = 0.8, position = position_nudge(x = 0.2, y = 0))+
    theme_niwot()+
    geom_point(aes (y = richness, colour = fert), position = position_jitter(width = 0.15),
               size = 1, alpha = 0.1)+
    geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) +
    labs(y = "species richness", x = NULL) +
    guides(fill = FALSE, colour = FALSE)+
    scale_fill_manual(values = c("#5A4A6F", "#E47250",  "#EBB261", "#9D5A6C")) +
    scale_colour_manual(values = c("#5A4A6F", "#E47250",  "#EBB261", "#9D5A6C"))+
    coord_flip())

