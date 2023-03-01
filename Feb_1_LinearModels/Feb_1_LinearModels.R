library(here)
library(ggplot2)
library(tidyr)
library(dplyr)
library(DHARMa)

weevil_damage <- read.csv(here::here("Feb_1_LinearModels/CC", "Weevil_damage.csv"))
LPI <- read.csv(here::here("Feb_1_LinearModels/CC", "shagLPI.csv"))

weevil_damage$block <- as.factor(weevil_damage$block)

ggplot(weevil_damage, aes(x = block, y = total_damage)) +
  geom_boxplot()

#once you look at your data you can then start to model 
#start by producing a histogram to look at the distribution of your data

hist(weevil_damage$total_damage)

#linear model
model1 <- lm(total_damage ~ block, data = weevil_damage)
plot(model1) #distribution wasn't great so going to try a different one 

model2 <- glm(total_damage ~ block, data = weevil_damage, 
              family = poisson(link = "log"))
plot(model2) #this model still doesn't fit well, but we'll use it to carry on 

summary(model2)

#### PART 2 ####
LPI$year <- as.numeric(LPI$year)
str(LPI)

(LPIhist <- ggplot(LPI, aes(x = pop)) +
    geom_histogram())

LPImodel <- glm(pop ~ year, family = poisson(link = "log"), data = LPI)
plot(LPImodel)
summary(LPImodel)

#trying out the pakage DHARMa
model_resid <- simulateResiduals(LPImodel)
plot(model_resid) #turns out its a shit fit :)


#trying a binomial model
weevil_model <- glm(damage_T_F ~ block, family = binomial, data = weevil_damage)

plot(weevil_model)
summary(weevil_model)


