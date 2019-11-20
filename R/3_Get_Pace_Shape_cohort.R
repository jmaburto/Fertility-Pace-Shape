# Jose Manuel: the code for calculating shape and pace of reproduction is in the development-development version of 
# the package Rage which is under development by myself, Owen, Patrick Barks and others... to install it from one of 
# my development branches using the devtools package use
# devtools::install_github("iainmstott/Rage@pr51")
# (I hope to sort out getting it pulled into the main repo soon!)
# The functions you want are shape_rep and pace_rep. I note that if you're interested in the survival
# versions at all, these are shape_surv and pace_surv.
# I've attached the Code and Data for our Figure 3, inlcuding an R script for the individual shape_rep and pace_rep functions. 

#devtools::install_github("jonesor/Rage")
#install.packages("remotes") # smaller and quicker to install than devtools
#remotes::install_github("iainmstott/Rage@pr51")

library(Rage)
library(data.table)
library(ggplot2)
library(viridis)

# set your working directory
setwd("C:/Users/jmaburto.SAM/Documents/GitHub/Fertility-Pace-Shape")

# load fertility data
load('Data/HFD_Data.RData')

HFD <- merge(HFD.cohort,CCF.cohort, by = c('country','cohort'))

# calculate asfr
HFD[, asfr := births/exposure]

# take out non-completed cohort
HFD <- HFD[, select.cohort:= ifelse(is.na(sum(asfr)),'no','yes'), by = list(country,cohort)]
HFD <- HFD[select.cohort == 'yes']

# we have consistent informatin with completed cohorts from age 12 to age 55
HFD[, list(min.age = min(age), max.age = max(age)), by = list(country,cohort)]

# take min and max reproductive ages
ages <- HFD[, list(min.age = min(age[which(births > 0)]), max.age = max(age[which(births>0)])), by = list(country,cohort)]

range(ages$min.age)

range(ages$max.age)

m <- HFD[country == 'AUT' & cohort == 1939,]

m[,list((min(age[which(births > 0)]):max(age[which(births>0)])),
        asfr[((min(age[which(births > 0)]):(max(age[which(births>0)]+1)))-min(age[which(births > 0)])+1)[-1]])]


#get pace and shape
Pace_Shape_HFD <- HFD[,list(shape = shape_rep(rep = data.frame(cbind(x =(min(age[which(births > 0)]):max(age[which(births>0)]))), 
                                                                     mx = asfr[((min(age[which(births > 0)]):(max(age[which(births>0)]+1)))-min(age[which(births > 0)])+1)[-1]],
                                                               xmin = min(age[which(births>0)]), 
                                                               xmax = max(age[which(births>0)]),
                                                               fertTable = F)), 
                            pace = pace_rep(rep = data.frame(cbind(x =(min(age[which(births > 0)]):max(age[which(births>0)]))), 
                                                                    mx = asfr[((min(age[which(births > 0)]):(max(age[which(births>0)]+1)))-min(age[which(births > 0)])+1)[-1]],
                                                              xmin = min(age[which(births>0)]), 
                                                              xmax = max(age[which(births>0)]),
                                                              fertTable = F)),
                             ccf = sum(asfr)), 
                      by = list(country,cohort)]

Pace_Shape_HFD <- merge(Pace_Shape_HFD,MAB.cohort, by = c('country','cohort'))

# make preliminary figure

Fig1 <- ggplot(data = Pace_Shape_HFD, aes(x = pace, y = shape,color=cohort,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  scale_x_continuous(expression(Pace))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous(expression(Shape))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Pace and Shape of human fertility, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig1

Fig.country <- ggplot(data = Pace_Shape_HFD, aes(x = pace, y = shape,color=cohort, size = ccf)) +
  geom_point(alpha=I(1/1),shape=16,show.legend = T)+
  facet_wrap(~country)+
  scale_x_continuous(expression(Pace))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous(expression(Shape))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Pace and Shape of human fertility, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig.country


pdf(file="Figures/Figs_Pace_Shape_CohortV3.pdf",width=15,height=13,pointsize=4)
Fig1
Fig.country
dev.off()




#with mab

Fig2 <- ggplot(data = Pace_Shape_HFD, aes(x = cmab, y = shape,color=cohort)) +
  geom_point(alpha=I(1/1),shape=16,show.legend = T)+
  scale_x_continuous(expression(MAB))+
  scale_color_viridis(discrete=F,option = 'C') +
  scale_y_continuous(expression(Shape))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('MAB and Shape of human fertility, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig2


Fig2.country <- ggplot(data = Pace_Shape_HFD, aes(x = cmab, y = shape,color=cohort)) +
  geom_point(alpha=I(1/1),shape=16,show.legend = T)+
  facet_wrap(~country)+
  scale_x_continuous(expression(MAB))+
  scale_color_viridis(discrete=F,option = 'C') +
  scale_y_continuous(expression(Shape))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('MAB and Shape of human fertility, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig2.country


HFD[,list(min(cohort),max(cohort)), by = list(country)]
