library(here)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(sjPlot)
library(ggeffects)


plants <- read.csv(here::here("Feb_8_ModelDesign/CC", "toolik_plants.csv"))
 #<- read.csv(here::here("Feb_8_ModelDesign/CC", ".csv"))


str(plants)

plants <- plants %>%
  mutate(across(c(Site, Block, Plot), as.factor))
str(plants)

unique(plants$Site)
plants %>%
  group_by(Site) %>%
  summarize(nblock = length(unique(Block)))

plants %>%
  group_by(Block) %>%
  summarize(nplot = length(unique(Plot)))

unique(plants$Year)
unique(plants$Species)

plants <- plants %>%
  filter(!Species %in% c("Woody cover", "Tube",
                         "Hole", "Vole trail",
                         "removed", "vole turds",
                         "Mushrooms", "Water",
                         "Caribou poop", "Rocks",
                         "mushroom", "caribou poop",
                         "animal litter", "vole poop",
                         "Vole poop", "Unk?"))

# determine plant species richness for different nested combinations
plant_rich <- plants %>% 
  group_by(Year, Site, Block, Plot) %>%
  mutate(richness = length(unique(Species))) %>%
  ungroup()

(hist_rich <- ggplot(plant_rich, aes(x = richness)) +
    geom_histogram())

(hist_cover <- ggplot(plant_rich, aes(x = Relative.Cover)) +
    geom_histogram())

#modelling

plant_model <- lm(richness ~ I(Year - 2007), data = plant_rich)
summary(plant_model)

plot(plant_model)

#Hierarchical model 

plant_model2 <- lmer(richness ~ I(Year - 2007) + (1|Site),
                     data = plant_rich)
summary(plant_model2)

# To be continued ... #

#using models with nested random effects 
plant_model3 <- lmer(richness ~ I(Year - 2007) + (1|Site/Block),
                     data = plant_rich)
summary(plant_model3)

plant_model4 <- lmer(richness ~ I(Year - 2007) + (1|Site/Block/Plot),
                     data = plant_rich)
summary(plant_model4)
plot(plant_model4)

#plotting the variance of the random effects (coefficient plots)
(re_effects <- plot_model(plant_model4, type = "re", show.values = TRUE))
#plotting the fixed effects 
(fix_effects <- plot_model(plant_model4, show.values = TRUE))

rs_plant_model <- lmer(richness ~ Mean.Temp + (Mean.Temp|Site/Block/Plot) + (1|Year),
                       data = plant_rich)
summary(rs_plant_model)
(re_effects <- plot_model(rs_plant_model, type = "re", show.values = TRUE))
(fix_effects <- plot_model(rs_plant_model, show.values = TRUE))

#using model to pull out relationship and plot 
ggpredict(rs_plant_model, terms = c("Mean.Temp")) %>%
  plot()

ggpredict(rs_plant_model, terms = c("Mean.Temp", "Site"), type = "re") %>%
  plot() #you can make the same plot in ggplot (below), allows for more customization

ggpredict(rs_plant_model, terms = c("Mean.Temp", "Site"), type = "re") %>%
  ggplot(., aes(x = x, y = predicted, colour = group)) +
  stat_smooth(method = "lm", se = FALSE) +
  theme_classic()


