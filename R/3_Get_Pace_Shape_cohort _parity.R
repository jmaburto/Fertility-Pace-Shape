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


Pace_Shape_HFD <- HFD[,list(shape.Total = shape_rep(rep = data.frame(cbind(x =(min(age[which(total.births > 0)]):max(age[which(total.births>0)]))), 
                                                               mx = asfr[((min(age[which(total.births > 0)]):(max(age[which(total.births>0)]+1)))-min(age[which(total.births > 0)])+1)[-1]],
                                                               xmin = min(age[which(total.births>0)]), 
                                                               xmax = max(age[which(total.births>0)]),
                                                               fertTable = F)), 
                            pace.Total = pace_rep(rep = data.frame(cbind(x =(min(age[which(total.births > 0)]):max(age[which(total.births>0)]))), 
                                                             mx = asfr[((min(age[which(total.births > 0)]):(max(age[which(total.births>0)]+1)))-min(age[which(total.births > 0)])+1)[-1]],
                                                             xmin = min(age[which(total.births>0)]), 
                                                             xmax = max(age[which(total.births>0)]),
                                                             fertTable = F)),
                            shape.B1 = shape_rep(rep = data.frame(cbind(x =(min(age[which(birth1 > 0)]):max(age[which(birth1>0)]))), 
                                                                     mx = asfr1[((min(age[which(birth1 > 0)]):(max(age[which(birth1>0)]+1)))-min(age[which(birth1 > 0)])+1)[-1]],
                                                                     xmin = min(age[which(birth1>0)]), 
                                                                     xmax = max(age[which(birth1>0)]),
                                                                     fertTable = F)), 
                            pace.B1 = pace_rep(rep = data.frame(cbind(x =(min(age[which(birth1 > 0)]):max(age[which(birth1>0)]))), 
                                                                   mx = asfr1[((min(age[which(birth1 > 0)]):(max(age[which(birth1>0)]+1)))-min(age[which(birth1 > 0)])+1)[-1]],
                                                                   xmin = min(age[which(birth1>0)]), 
                                                                   xmax = max(age[which(birth1>0)]),
                                                                   fertTable = F)),
                            shape.B2 = shape_rep(rep = data.frame(cbind(x =(min(age[which(birth2 > 0)]):max(age[which(birth2>0)]))), 
                                                                  mx = asfr2[((min(age[which(birth2 > 0)]):(max(age[which(birth2>0)]+1)))-min(age[which(birth2 > 0)])+1)[-1]],
                                                                  xmin = min(age[which(birth2>0)]), 
                                                                  xmax = max(age[which(birth2>0)]),
                                                                  fertTable = F)), 
                            pace.B2 = pace_rep(rep = data.frame(cbind(x =(min(age[which(birth2 > 0)]):max(age[which(birth2>0)]))), 
                                                                mx = asfr2[((min(age[which(birth2 > 0)]):(max(age[which(birth2>0)]+1)))-min(age[which(birth2 > 0)])+1)[-1]],
                                                                xmin = min(age[which(birth2>0)]), 
                                                                xmax = max(age[which(birth2>0)]),
                                                                fertTable = F)),
                            shape.B3 = shape_rep(rep = data.frame(cbind(x =(min(age[which(birth3 > 0)]):max(age[which(birth3>0)]))), 
                                                                  mx = asfr3[((min(age[which(birth3 > 0)]):(max(age[which(birth3>0)]+1)))-min(age[which(birth3 > 0)])+1)[-1]],
                                                                  xmin = min(age[which(birth3>0)]), 
                                                                  xmax = max(age[which(birth3>0)]),
                                                                  fertTable = F)), 
                            pace.B3 = pace_rep(rep = data.frame(cbind(x =(min(age[which(birth3 > 0)]):max(age[which(birth3>0)]))), 
                                                                mx = asfr3[((min(age[which(birth3 > 0)]):(max(age[which(birth3>0)]+1)))-min(age[which(birth3 > 0)])+1)[-1]],
                                                                xmin = min(age[which(birth3>0)]), 
                                                                xmax = max(age[which(birth3>0)]),
                                                                fertTable = F)),
                            shape.B4 = shape_rep(rep = data.frame(cbind(x =(min(age[which(birth4 > 0)]):max(age[which(birth4>0)]))), 
                                                                  mx = asfr4[((min(age[which(birth4 > 0)]):(max(age[which(birth4>0)]+1)))-min(age[which(birth4 > 0)])+1)[-1]],
                                                                  xmin = min(age[which(birth4>0)]), 
                                                                  xmax = max(age[which(birth4>0)]),
                                                                  fertTable = F)), 
                            pace.B4 = pace_rep(rep = data.frame(cbind(x =(min(age[which(birth4 > 0)]):max(age[which(birth4>0)]))), 
                                                                mx = asfr4[((min(age[which(birth4 > 0)]):(max(age[which(birth4>0)]+1)))-min(age[which(birth4 > 0)])+1)[-1]],
                                                                xmin = min(age[which(birth4>0)]), 
                                                                xmax = max(age[which(birth4>0)]),
                                                                fertTable = F)),
                            ccf = sum(asfr)), 
                      by = list(country,cohort)]


range(c(Pace_Shape_HFD$pace.Total,Pace_Shape_HFD$pace.B1,Pace_Shape_HFD$pace.B2,Pace_Shape_HFD$pace.B3,Pace_Shape_HFD$pace.B4))

range(c(Pace_Shape_HFD$shape.Total,Pace_Shape_HFD$shape.B1,Pace_Shape_HFD$shape.B2,Pace_Shape_HFD$shape.B3,Pace_Shape_HFD$shape.B4))


# make preliminary figure
Fig.total <- ggplot(data = Pace_Shape_HFD, aes(x = pace.Total, y = shape.Total,color=cohort,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  scale_x_continuous(expression(Pace),limits = c(7.6,16.6))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Pace and Shape of human fertility, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig.total


Fig.b1 <- ggplot(data = Pace_Shape_HFD, aes(x = pace.B1, y = shape.B1,color=cohort,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  scale_x_continuous(expression(Pace),limits = c(7.6,16.6))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Pace and Shape of human fertility of birth 1, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig.b1


Fig.b2 <- ggplot(data = Pace_Shape_HFD, aes(x = pace.B2, y = shape.B2,color=cohort,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  scale_x_continuous(expression(Pace),limits = c(7.6,16.6))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Pace and Shape of human fertility of birth 2, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig.b2


Fig.b3 <- ggplot(data = Pace_Shape_HFD, aes(x = pace.B3, y = shape.B3,color=cohort,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  scale_x_continuous(expression(Pace),limits = c(7.6,16.6))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Pace and Shape of human fertility of birth 3, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig.b3


Fig.b4 <- ggplot(data = Pace_Shape_HFD, aes(x = pace.B4, y = shape.B4,color=cohort,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  scale_x_continuous(expression(Pace),limits = c(7.6,16.6))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Pace and Shape of human fertility of birth 4, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig.b4

########### by country


# make preliminary figure
Fig.total.country <- ggplot(data = Pace_Shape_HFD, aes(x = pace.Total, y = shape.Total,color=cohort,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  facet_wrap(~country)+
  scale_x_continuous(expression(Pace),limits = c(7.6,16.6))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Pace and Shape of human fertility, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig.total.country


Fig.b1.country <- ggplot(data = Pace_Shape_HFD, aes(x = pace.B1, y = shape.B1,color=cohort,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  facet_wrap(~country)+
  scale_x_continuous(expression(Pace),limits = c(7.6,16.6))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Pace and Shape of human fertility of birth 1, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig.b1.country


Fig.b2.country <- ggplot(data = Pace_Shape_HFD, aes(x = pace.B2, y = shape.B2,color=cohort,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  facet_wrap(~country)+
  scale_x_continuous(expression(Pace),limits = c(7.6,16.6))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Pace and Shape of human fertility of birth 2, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig.b2.country


Fig.b3.country <- ggplot(data = Pace_Shape_HFD, aes(x = pace.B3, y = shape.B3,color=cohort,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  facet_wrap(~country)+
  scale_x_continuous(expression(Pace),limits = c(7.6,16.6))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Pace and Shape of human fertility of birth 3, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig.b3.country


Fig.b4.country <- ggplot(data = Pace_Shape_HFD, aes(x = pace.B4, y = shape.B4,color=cohort,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  facet_wrap(~country)+
  scale_x_continuous(expression(Pace),limits = c(7.6,16.6))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Pace and Shape of human fertility of birth 4, cohort',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig.b4.country





pdf(file="Figures/Figs_Pace_Shape_CohortV3_parity.pdf",width=15,height=13,pointsize=4)
Fig.total
Fig.b1
Fig.b2
Fig.b3
Fig.b4
Fig.total.country
Fig.b1.country
Fig.b2.country
Fig.b3.country
Fig.b4.country
dev.off()




#table about data
HFD[, list(min(cohort),max(cohort)), by = list(country)]
