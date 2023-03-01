library(here)
library(ggplot2)
library(tidyr)
library(dplyr)
elongation<- read.csv(here::here("Jan_18_Datamanipulation/CC", "EmpetrumElongation.csv"))

#The new R studio update will colour your hex codes 
"#000000"

head(elongation)
str(elongation)

#indexing in different ways:
elongation$Indiv

#pulling out row 2, column 5 
elongation[2,5]

elongation[6,]

elongation[6,]$Indiv

#subsetting
elongation[elongation$Indiv == 603,]

elongation[elongation$Zone != 6,]

elongation[elongation$Zone != 6 & elongation$Zone > 3,]

elongation[elongation$Indiv %in% c(300:400),]

seq(300, 400, 10)
rep(c(1,2),10)

#changing variable names and values in a data frame 

names(elongation)

names(elongation)[1] <- "zone"
names(elongation)

elongation[1,4] <- 4.10
head(elongation)

elongation[elongation$Indiv == 373, ]$X2008 <- 5.7

#creating a factor 

elongation$zone <- as.factor(elongation$zone)
str(elongation)

#changing factor levels (useful for plotting!)
levels(elongation$zone)
#levels(elongation$zone) <- c("A", "B", "C", "D", "E", "F")
levels(elongation$zone)

#changing formats of data (to long version)

elongation_long <- gather(elongation, Year, Length, c(X2007, X2008, X2009, X2010, X2011, X2012))

elongation_wide <- spread(elongation_long, Year, Length)

#using the pivot dplyr function will produce the same outcome 
elongation_longP <- pivot_longer(elongation, cols=c("X2007", "X2008", "X2009", "X2010", "X2011", "X2012"), 
                                 names_to = "Year", values_to = "Length")

boxplot(Length ~ Year, data = elongation_long,
        xlab = "Year", ylab = "Elongation (cm)",
        main = "Annual growth of Empetrum hermaphroditum")

#using the dplyr package 

#renaming 

elongation_long <- rename(elongation_long, ZONE = zone, ID = Indiv)
names(elongation_long)

#filter and select functions
elong_sub <- dplyr::filter(elongation_long, ZONE %in% c("B", "C"))

#both of the lines below do the same thing (remove the ZONE column)
elong_select <- dplyr::select(elongation_long, ID, Year, Length)
elong_select <- dplyr::select(elongation_long, -ZONE)

#can also do this with base R, but you can't do multiple at once
elongation_long[, -1]

#you can change the order the columns appear in by selecting them in a different order 
elongation_noZ <- dplyr::select(elongation_long, YEAR = Year, Growth = Length,ShrubID = ID)

#mutate function 
elong_total <- mutate(elongation, Total.Growth = X2007 + X2008 + X2009 + X2010 + X2011 +X2012)

#group_by function
elong_group <- group_by(elongation_long, ID)

#summarize function 
summary1 <- summarize(elong_group, TotalGrowth = sum(Length))
summary2 <- summarize(elongation_long, TotalGrowth = sum(Length))
summary3 <- summarize(elong_group, TotalGrowth = sum(Length), 
                      MeanGrowth = mean(Length), stdGrowth = sd(Length))



treatments<- read.csv(here::here("Jan_18_Datamanipulation/CC", "EmpetrumTreatments.csv"), header = TRUE, sep = ";")
head(treatments)
head(elongation_long)

#using join function 

treatments$Zone <- as.factor(treatments$Zone)
experiment <- left_join(elongation_long, treatments, by = c("Indiv" = "Indiv", "zone" = "Zone"))

#you can also use the merge function 

experiment2 <- merge(elongation_long, treatments, by.x = c("zone", "Indiv"), by.y = c("Zone", "Indiv"))

boxplot(Length ~ Treatment, data = experiment2)
