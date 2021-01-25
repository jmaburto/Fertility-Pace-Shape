#devtools::install_github("jonesor/Rage")
#install.packages("remotes") # smaller and quicker to install than devtools
#remotes::install_github("iainmstott/Rage@pr51")

library(Rage)
library(data.table)
library(ggplot2)
library(viridis)

# load fertility data
load('Data/HFD_Data.RData')

HFD <- merge(HFD.cohort.asfr.parity, HFD.cohort.birth.parity, by = c('country','cohort','age'))

# take out non-completed cohort
HFD <- HFD[, select.cohort:= ifelse(is.na(sum(total.births)),'no','yes'), by = list(country,cohort)]
HFD <- HFD[select.cohort == 'yes']

# we have consistent informatin with completed cohorts from age 12 to age 55
HFD[, list(min.age = min(age), max.age = max(age)), by = list(country,cohort)]


# load some useful functions
source('R/Functions.R')

#Age <- 12:55
#bx <- HFD[country %in% 'USA' & cohort %in% 1961]$total.births

# Pace_shape 
Pace_Shape_HFD <- HFD[,list(shape.Total = birth.shape(Age = age,bx = total.births), 
                            shape.b1 = birth.shape(Age = age,bx = birth1),
                            shape.b2 = birth.shape(Age = age,bx = birth2),
                            shape.b3 = birth.shape(Age = age,bx = birth3),
                            shape.b4 = birth.shape(Age = age,bx = birth4),
                            shape.b5 = birth.shape(Age = age,bx = birth5p),
                            pace.Total = birth.pace(Age = age,bx = total.births), 
                            pace.b1 = birth.pace(Age = age,bx = birth1),
                            pace.b2 = birth.pace(Age = age,bx = birth2),
                            pace.b3 = birth.pace(Age = age,bx = birth3),
                            pace.b4 = birth.pace(Age = age,bx = birth4),
                            pace.b5 = birth.pace(Age = age,bx = birth5p),
                            quantum.Total = sum(total.births),
                            quantum.1 = sum(birth1),
                            quantum.2 = sum(birth2),
                            quantum.3 = sum(birth3),
                            quantum.4 = sum(birth4),
                            quantum.5 = sum(birth5p),
                            ccf = sum(asfr)), 
                      by = list(country,cohort)]


########### by country


# make preliminary figure
Fig.total.country <- ggplot(data = Pace_Shape_HFD, aes(x = pace.Total, y = shape.Total,color=cohort,size = quantum.Total)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  facet_wrap(~country)+
  scale_x_continuous(expression(Pace))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous(expression(Shape))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Pace and Shape of human fertility total, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig.total.country


Fig.birth1 <- ggplot(data = Pace_Shape_HFD, aes(x = pace.b1, y = shape.b1,color=cohort,size = quantum.1)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  facet_wrap(~country)+
  scale_x_continuous(expression(Pace))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous(expression(Shape))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Pace and Shape of human fertility b1, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
Fig.birth1


Fig.birth2 <- ggplot(data = Pace_Shape_HFD, aes(x = pace.b2, y = shape.b2,color=cohort,size = quantum.2)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  facet_wrap(~country)+
  scale_x_continuous(expression(Pace))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous(expression(Shape))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Pace and Shape of human fertility b2, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
Fig.birth2


Fig.birth3 <- ggplot(data = Pace_Shape_HFD, aes(x = pace.b3, y = shape.b3,color=cohort,size = quantum.3)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  facet_wrap(~country)+
  scale_x_continuous(expression(Pace))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous(expression(Shape))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Pace and Shape of human fertility b3, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
Fig.birth3


Fig.birth4 <- ggplot(data = Pace_Shape_HFD, aes(x = pace.b4, y = shape.b4,color=cohort,size = quantum.4))+
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  facet_wrap(~country)+
  scale_x_continuous(expression(Pace))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous(expression(Shape))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Pace and Shape of human fertility b4, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
Fig.birth4


Fig.birth5 <- ggplot(data = Pace_Shape_HFD, aes(x = pace.b5, y = shape.b5,color=cohort,size = quantum.5)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  facet_wrap(~country)+
  scale_x_continuous(expression(Pace))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous(expression(Shape))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Pace and Shape of human fertility b5, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
Fig.birth5



pdf(file="Figures/Figs_Pace_Shape_ERC_AB.pdf",width=15,height=13,pointsize=4)
Fig.total.country
Fig.birth1
Fig.birth2
Fig.birth3
Fig.birth4
Fig.birth5
dev.off()

