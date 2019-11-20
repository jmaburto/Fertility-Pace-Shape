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

HFD <- HFD.period

# calculate asfr
HFD[, asfr := births/exposure]

#get pace and shape
Pace_Shape_HFD <- HFD[,list( shape = shape_rep(rep = data.frame(cbind(x = age, mx = asfr),xmin = 12, xmax = 55,fertTable = F)), 
                             pace = pace_rep(rep = data.frame(cbind(x = age, mx = asfr),xmin = 12, xmax = 55,fertTable = F)),
                             tfr = sum(asfr)), 
                      by = list(country,year)]

# make preliminary figure

Fig1 <- ggplot(data = Pace_Shape_HFD, aes(x = pace, y = shape,color=tfr)) +
  geom_point(alpha=I(1/1),shape=16,show.legend = T)+
  geom_smooth(data=Pace_Shape_HFD,aes(x = pace, y = shape), method = "lm", se=FALSE,col="black",size=1,lty=2,na.rm = T) + # if I want a linear one
  #facet_wrap(~country)+
  scale_x_continuous(expression(Pace))+
  scale_color_viridis(discrete=F,option = 'C') +
  scale_y_continuous(expression(Shape))+
  theme(legend.key.height=unit(2,"line"))+
  #theme(legend.position = c(0.15, 0.85))+
  ggtitle('Pace and Shape of human fertility',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
#eliminates background, gridlines, and chart border
# theme(plot.background = element_blank()
#       ,panel.grid.major = element_blank()
#       ,panel.grid.minor = element_blank()
#       ,panel.border = element_blank()
#       ,legend.key = element_blank()
#       ,axis.line.x = element_line(color="black", size = .5)
#       ,axis.line.y = element_line(color="black", size = .5))
previous_theme <- theme_set(theme_bw())

Fig1


Fig1.years <- ggplot(data = Pace_Shape_HFD, aes(x = pace, y = shape,color=year)) +
  geom_point(alpha=I(1/1),shape=16,show.legend = T)+
  geom_smooth(data=Pace_Shape_HFD,aes(x = pace, y = shape), method = "lm", se=FALSE,col="black",size=1,lty=2,na.rm = T) + # if I want a linear one
  facet_wrap(~country)+
  scale_x_continuous(expression(Pace))+
  scale_color_viridis(discrete=F,option = 'C') +
  scale_y_continuous(expression(Shape))+
  theme(legend.key.height=unit(2,"line"))+
  #theme(legend.position = c(0.15, 0.85))+
  ggtitle('Pace and Shape of human fertility',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
  #eliminates background, gridlines, and chart border
  # theme(plot.background = element_blank()
  #       ,panel.grid.major = element_blank()
  #       ,panel.grid.minor = element_blank()
  #       ,panel.border = element_blank()
  #       ,legend.key = element_blank()
  #       ,axis.line.x = element_line(color="black", size = .5)
  #       ,axis.line.y = element_line(color="black", size = .5))
previous_theme <- theme_set(theme_bw())

Fig1.years

Fig2.shape <- ggplot(data = Pace_Shape_HFD, aes(x = year, y = shape)) +
  geom_point(alpha=I(1/1),shape=16,show.legend = T)+
  #geom_smooth(data=Pace_Shape_HFD,aes(x = pace, y = shape), method = "lm", se=FALSE,col="black",size=1,lty=2,na.rm = T) + # if I want a linear one
  facet_wrap(~country)+
  scale_x_continuous(expression(Year))+
  scale_color_viridis(discrete=F,option = 'C') +
  scale_y_continuous(expression(Shape))+
  theme(legend.key.height=unit(2,"line"))+
  #theme(legend.position = c(0.15, 0.85))+
  ggtitle('Pace and Shape of human fertility',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
#eliminates background, gridlines, and chart border
# theme(plot.background = element_blank()
#       ,panel.grid.major = element_blank()
#       ,panel.grid.minor = element_blank()
#       ,panel.border = element_blank()
#       ,legend.key = element_blank()
#       ,axis.line.x = element_line(color="black", size = .5)
#       ,axis.line.y = element_line(color="black", size = .5))
previous_theme <- theme_set(theme_bw())

Fig2.shape


Fig3.pace <- ggplot(data = Pace_Shape_HFD, aes(x = year, y = pace)) +
  geom_point(alpha=I(1/1),shape=16,show.legend = T)+
  #geom_smooth(data=Pace_Shape_HFD,aes(x = pace, y = shape), method = "lm", se=FALSE,col="black",size=1,lty=2,na.rm = T) + # if I want a linear one
  facet_wrap(~country)+
  scale_x_continuous(expression(Year))+
  scale_color_viridis(discrete=F,option = 'C') +
  scale_y_continuous(expression(Pace))+
  theme(legend.key.height=unit(2,"line"))+
  #theme(legend.position = c(0.15, 0.85))+
  ggtitle('Pace and Shape of human fertility',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
#eliminates background, gridlines, and chart border
# theme(plot.background = element_blank()
#       ,panel.grid.major = element_blank()
#       ,panel.grid.minor = element_blank()
#       ,panel.border = element_blank()
#       ,legend.key = element_blank()
#       ,axis.line.x = element_line(color="black", size = .5)
#       ,axis.line.y = element_line(color="black", size = .5))
previous_theme <- theme_set(theme_bw())

Fig3.pace

pdf(file="Figures/Figs_Pace_Shape_rep.pdf",width=15,height=13,pointsize=4)
Fig1.tfr
Fig1.years
Fig2.shape
Fig3.pace
Fig1
#Fig2.MC
dev.off()
