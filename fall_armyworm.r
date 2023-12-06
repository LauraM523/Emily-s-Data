
#Calculating percent growth change. Looking at proportional change realive to starting point rather than absolute difference.


raw <- read.csv("C:/Users/laura/Github/Emily-Data/fallarmyworm_rawdata - r_analysis.csv")
View(raw)



library(ggplot2)
library(dplyr)
library(tidyverse)
library


worm <- aov(raw$percent_growth_rate_worm~raw$Treatment)
summary(worm)


#check assumptions

par(mfrow = c(2,2))
plot(worm)

fligner.test(raw$percent_growth_rate_worm~raw$Treatment)
#data presents equal variances


shapiro.test(resid(worm))
#residuals have normal distribution

hist(resid(worm))



TukeyHSD(worm)
#no significance 

raw$Treatment <- factor(raw$Treatment, levels = c("control", "risk", "pheromone", "hexane"))

ggplot(raw,aes(x=Treatment, y = percent_growth_rate_worm , fill = Treatment)) + 
  geom_bar(stat="summary", position=position_dodge(), fun.y="mean") + 
  geom_errorbar(stat="summary", fun.data="mean_se", width=0.2)+
  ylab("Percent Growth Rate (mg)") + xlab("Treatment")

ggplot(raw,aes(x=Treatment, y = percent_growth_rate_worm , fill = Treatment))+
  geom_boxplot()


ggplot(raw,aes(x=Treatment, y = percent_growth_rate_worm , fill = Treatment))+
  geom_violin()




pheromone<-subset(raw, Treatment %in% c('hexane', 'pheromone'))

risk<- subset(raw, Treatment %in% c('risk', 'control'))



t.test(pheromone$percent_growth_rate_worm~ pheromone$Treatment)

ggplot(pheromone,aes(x=Treatment, y = percent_growth_rate_worm)) + 
  geom_bar(stat="summary", position=position_dodge(), fun.y="mean", fill= "red") + 
  geom_errorbar(stat="summary", fun.data="mean_se", width=0.2)+
  ylab("Percent Growth Rate (mg)") + xlab("Treatment")





t.test(risk$percent_growth_rate_worm~risk$Treatment)

ggplot(risk,aes(x=Treatment, y = percent_growth_rate_worm)) + 
  geom_bar(stat="summary", position=position_dodge(), fun.y="mean", fill= "blue") + 
  geom_errorbar(stat="summary", fun.data="mean_se", width=0.2)+
  ylab("Percent Growth Rate (mg)") + xlab("Treatment")

ggplot(risk,aes(x=Treatment, y = percent_growth_rate_worm)) + 
  geom_boxplot()

