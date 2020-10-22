#import dataset
chaff <- read.table("raw_data/chaff.txt", header = TRUE)

#tidy data
library(tidyverse)
chaff2 <- gather(chaff, key = sex, mass)
  
#summarise data in a new dataframe
chaff_sum <- chaff2 %>% 
  group_by(sex) %>% 
  summarise(mean = mean(mass),
            n = length(mass),
            std= sd(mass),
            se = std/sqrt(n))

#analyse data using an anova
anova <- aov(mass ~ sex, data = chaff2)
summary(anova)
#there is a significant difference in mass between sexes

#create a box and whisker figure from chaff2 dataset
library(ggplot2)
chaff2 %>% ggplot(aes(x = sex, y = mass)) +
  geom_boxplot()







