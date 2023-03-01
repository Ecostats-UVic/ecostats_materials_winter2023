library(here)
library(ggplot2)
library(tidyr)
library(dplyr)
trees <- read.csv(here::here("Jan_25_DataManipulation2/CC", "trees.csv"))

trees.group <- group_by(trees, CommonName)
trees.summary <- summarize(trees.group, count = length(CommonName))
trees.summary2 <- tally(trees.group)

#Pipes
#you can type the pipe out or use a shortcut (command, shift, m (mac) or control, shift, m on windows)
trees.summary3 <- trees %>% 
  group_by(CommonName) %>% 
  tally()

#summarize all function - will summarize all columns in a dataframe (if they are numeric)
sum.all <- summarize_all(trees, mean)

#case when function

vector <- c(23, 472, 12, 45)
ifelse(vector <50, "a", "b")

vector2 <- c("What am I?", "a", "b", "c", "d")
case_when(vector2 == "What am I?" ~ "I am the walrus", 
          vector2 %in% c("a", "b")~ "goo",
          vector2 == "c" ~ "ga",
          vector2 == "d" ~ "joob")

unique(trees$LatinName)

trees.genus <- trees %>% 
  mutate(Genus = case_when(
    grepl("Acer", LatinName) ~ "Acer",
    grepl("Fraxinus", LatinName) ~ "Fraxinus",
    grepl("Sorbus", LatinName) ~ "Sorbus",
    grepl("Betula", LatinName) ~ "Betula",
    grepl("Populus", LatinName) ~ "Populus",
    grepl("Laburnum", LatinName) ~ "Laburnum",
    grepl("Aesculus", LatinName) ~ "Aesculus",
    grepl("Fagus", LatinName) ~ "Fagus",
    grepl("Prunus", LatinName) ~ "Prunus",
    grepl("Pinus", LatinName) ~ "Pinus",
    grepl("Sambucus", LatinName) ~ "Sambucus",
    grepl("Crataegus", LatinName) ~ "Crataegus",
    grepl("Ilex", LatinName) ~ "Ilex",
    grepl("Quercus", LatinName) ~ "Quercus",
    grepl("Larix", LatinName) ~ "Larix",
    grepl("Salix", LatinName) ~ "Salix",
    grepl("Alnus", LatinName) ~ "Alnus"
  )) # if you have a lot of species you can use the stingr function to take everything before a space or _ (or after) rather than typing them all out 

#separate function 
trees.genus2 <- trees %>% 
  separate(LatinName, c("Genus", "Species", "Extra"), sep = " ", remove = FALSE) %>% #added the extra column after an error message (two entries have subspecies)
  select(-Species) #-species means remove the species line (i.e., select everything but species)

#Advanced piping 

trees.five <- trees.genus %>%
  filter(Genus %in% c("Acer", "Fraxinus", "Salix", "Aesculus", "Pinus"))

(map.all <- ggplot(trees.five) +
    geom_point(aes(x = Easting, y = Northing, size = Height, colour =Genus), alpha = 0.5) +
    theme_classic())


#you can do the same as above but with a pipe 
trees.plots <- trees.five %>% 
  group_by(Genus) %>% 
  do(plots = 
       ggplot(data = .) +
       geom_point(aes(x = Easting, y = Northing, size = Height), alpha = 0.5)+
       labs(title = paste("Map of", .$Genus, "at Craigmillar Castle", sep = " "))+
       theme_classic())
trees.plots$plots  #to call on the plots

#saving the plots 
trees.plots %>% 
  do(., 
     ggsave(.$plots, filename = here::here("Jan_25_DataManipulation2", paste("map-", .$Genus, ".png", sep = "")), 
            device = "png", height = 12, width = 16,
            units = "cm"))
  
  
