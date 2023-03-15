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

#Let's make a histogram
obs.niwot <- niwot %>% 
  group_by(USDA_Scientific_Name) %>% 
  tally() %>% 
  arrange(desc(n))

carex <- niwot %>% 
  filter(str_detect(USDA_Scientific_Name, "Carex"))

(hist1 <- ggplot(carex, aes(x=hits))+
    geom_histogram())

(hist2 <- ggplot(carex, aes(x=hits))+
    geom_histogram(alpha=0.6, breaks = seq(0,100,by=3), fill="palegreen4")+ #create a list of numbers from 0 to 100 by 3 
    theme_niwot())

(hist3 <- ggplot(carex, aes(x=hits))+
    geom_histogram(alpha=0.6, breaks = seq(0,100,by=3), fill="palegreen4")+ #create a list of numbers from 0 to 100 by 3 
    theme_niwot()+
    scale_y_continuous(limits = c(0,100), expand = expand_scale(mult = c(0,0.1)))) #change the empty space around the bars and move count away from the axis

(hist4 <- ggplot(carex, aes(x=hits))+
    geom_histogram(alpha=0.6, breaks = seq(0,100,by=3), fill="palegreen4", colour="black")+ #create a list of numbers from 0 to 100 by 3 
    theme_niwot()+
    scale_y_continuous(limits = c(0,100), expand = expand_scale(mult = c(0,0.1)))) #change the empty space around the bars and move count away from the axis

h <- hist(carex$hits, breaks = seq(0,100,by = 3), plot = FALSE)
d1 <- data.frame(x=h$breaks, y=c(h$counts, NA))
d2 <- rbind(c(0,0),d1)

(hist5 <- ggplot(carex, aes(x=hits))+
    geom_histogram(alpha=0.6, breaks = seq(0,100,by=3), fill="palegreen4")+ #create a list of numbers from 0 to 100 by 3 
    theme_niwot()+
    scale_y_continuous(limits = c(0,100), expand = expand_scale(mult = c(0,0.1)))+
    geom_step(data=d2, aes(x=x, y=y), stat="identity", colour = "palegreen4")) 

(hist5 <- ggplot(carex, aes(x=hits))+
    geom_histogram(alpha=0.6, breaks = seq(0,100,by=3), fill="palegreen4")+ #create a list of numbers from 0 to 100 by 3 
    theme_niwot()+
    scale_y_continuous(limits = c(0,100), expand = expand_scale(mult = c(0,0.1)))+
    geom_step(data=d2, aes(x=x, y=y), stat="identity", colour = "palegreen4")+
    geom_vline(xintercept = mean(carex$hits), linetype="dotted", colour = "palegreen4", size = 1)+
    annotate("text", x=50, y=50, label= "mean :)")+
    geom_curve(aes(x=50, y=60, xend=mean(carex$hits)+2, yend=60), 
               arrow=arrow(length = unit(0.07,"inch")), size=0.7, colour = "grey30", curvature = 0.3))
##Sections 4: cleaning data
bird_pops_long <- gather(data = bird_pops, key = "year", value = "pop", 27:71)

#Examine the tidy data frame
head(bird_pops_long)
# Get rid of the X in front of years
# *** parse_number() from the readr package in the tidyverse ***
bird_pops_long$year <- parse_number(bird_pops_long$year)
# Create new column with genus and species together
bird_pops_long$species.name <- paste(bird_pops_long$Genus, bird_pops_long$Species, sep = " ")
# *** piping from from dplyr
bird_pops_long <- bird_pops_long %>%
  # Remove duplicate rows
  # *** distinct() function from dplyr
  distinct() %>%
  # remove NAs in the population column
  # *** filter() function from dplyr
  filter(is.finite(pop)) %>%
  # Group rows so that each group is one population
  # *** group_by() function from dplyr
  group_by(id) %>%
  # Make some calculations
  # *** mutate() function from dplyr
  mutate(maxyear = max(year), minyear = min(year),
         # Calculate duration
         duration = maxyear - minyear,
         # Scale population trend data
         scalepop = (pop - min(pop))/(max(pop) - min(pop))) %>%
  # Keep populations with >5 years worth of data and calculate length of monitoring
  filter(is.finite(scalepop),
         length(unique(year)) > 5) %>%
  # Remove any groupings you've greated in the pipe
  ungroup()

head(bird_pops_long)

# Data extraction ----
aus_pops <- bird_pops_long %>%
  filter(Country.list == "Australia")

# Calculate population change for each forest population
# 4331 models in one go!
# Using a pipe
aus_models <- aus_pops %>%
  # Group by the key variables that we want to iterate over
  # note that if we only include e.g. id (the population id), then we only get the
  # id column in the model summary, not e.g. duration, latitude, class...
  group_by(Decimal.Latitude, Decimal.Longitude, Class,
           species.name, id, duration, minyear, maxyear,
           system, Common.Name) %>%
  # Create a linear model for each group
  # Extract model coefficients using tidy() from the
  # *** tidy() function from the broom package ***
  do(broom::tidy(lm(scalepop ~ year, .))) %>%
  # Filter out slopes and remove intercept values
  filter(term == "year") %>%
  # Get rid of the column term as we don't need it any more
  #  *** select() function from dplyr in the tidyverse ***
  dplyr::select(-term) %>%
  # Remove any groupings you've greated in the pipe
  ungroup()

head(aus_models)
# Check out the model data frame

##Section 5: bird trait data
bird_traits <- bird_traits %>% 
  rename(species.name = Scientific)

bird_diet <- bird_traits %>% 
  select(species.name, Diet.5Cat) %>% 
  distinct() %>% 
  rename(diet = Diet.5Cat)

#Maintain all the data in the australian dataset (leftjoin) and the bird_diet gets added
bird_models_traits <- left_join(aus_models, bird_diet, by = "species.name") %>%
  drop_na()
head(bird_models_traits)

#Create a plot from the data
(trends_diet <- ggplot(bird_models_traits, aes(x=diet, y=estimate, colour=diet))+
    geom_jitter(size=3, alpha =0.3, width=0.2))

# Sorting the whole data frame by the mean trends
bird_models_traits <- bird_models_traits %>%
  group_by(diet) %>%
  mutate(mean_trend = mean(estimate)) %>%
  ungroup() %>%
  mutate(diet = fct_reorder(diet, -mean_trend))

# Calculating mean trends per diet categories
diet_means <- bird_models_traits %>% group_by(diet) %>%
  summarise(mean_trend = mean(estimate)) %>%
  arrange(mean_trend)

(trends_diet <- ggplot() +
    geom_jitter(data = bird_models_traits, aes(x = diet, y = estimate, colour = diet),
                size = 3, alpha = 0.3, width = 0.2) +
    geom_segment(data = diet_means,aes(x = diet, xend = diet,
                                       y = mean(bird_models_traits$estimate),
                                       yend = mean_trend), size = 0.8) +
    geom_point(data = diet_means, aes(x = diet, y = mean_trend,
                                      fill = diet), size = 5,
               colour = "grey30", shape = 21) +
    geom_hline(yintercept = mean(bird_models_traits$estimate),
               size = 0.8, colour = "grey30") +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey30") +
    coord_flip() +
    theme_clean() +
    scale_colour_manual(values = wes_palette("Cavalcanti1")) +
    scale_fill_manual(values = wes_palette("Cavalcanti1")) +
    scale_y_continuous(limits = c(-0.23, 0.23),
                       breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
                       labels = c("-0.2", "-0.1", "0", "0.1", "0.2")) +
    scale_x_discrete(labels = c("Carnivore", "Fruigivore", "Omnivore", "Insectivore", "Herbivore")) +
    labs(x = NULL, y = "\nPopulation trend") +
    guides(colour = FALSE, fill = FALSE))

#new plot
diet_sum <- bird_models_traits %>% group_by(diet) %>%
  tally()

(diet_bar <- ggplot(diet_sum, aes(x = diet, y = n,
                                  colour = diet,
                                  fill = diet)) +
    geom_bar(stat = "identity") +
    scale_colour_manual(values = wes_palette("Cavalcanti1")) +
    scale_fill_manual(values = wes_palette("Cavalcanti1")) +
    guides(colour = FALSE))

(diet_area <- ggplot(diet_sum, aes(area = n, fill = diet, label = n,
                                   subgroup = diet)) +
    geom_treemap() +
    geom_treemap_subgroup_border(colour = "white", size = 1) +
    geom_treemap_text(colour = "white", place = "center", reflow = T) +
    scale_colour_manual(values = wes_palette("Cavalcanti1")) +
    scale_fill_manual(values = wes_palette("Cavalcanti1")) +
    guides(fill = FALSE))  # this removes the colour legend
# later on we will combine multiple plots so there is no need for the legend
# to be in twice

# To display the legend, just remove the guides() line:
(diet_area <- ggplot(diet_sum, aes(area = n, fill = diet, label = n,
                                   subgroup = diet)) +
    geom_treemap() +
    geom_treemap_subgroup_border(colour = "white", size = 1) +
    geom_treemap_text(colour = "white", place = "center", reflow = T) +
    scale_colour_manual(values = wes_palette("Cavalcanti1")) +
    scale_fill_manual(values = wes_palette("Cavalcanti1")))

ggsave(diet_area, filename = "diet_area.png",
       height = 5, width = 8)