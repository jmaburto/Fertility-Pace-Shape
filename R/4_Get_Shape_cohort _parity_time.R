#devtools::install_github("jonesor/Rage")
#install.packages("remotes") # smaller and quicker to install than devtools
#remotes::install_github("iainmstott/Rage@pr51")
library(Rage)
library(data.table)
library(ggplot2)
library(viridis)

#?getDTthreads

# set your working directory
setwd("C:/Users/jmaburto.SAM/Documents/GitHub/Fertility-Pace-Shape")

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
Fig.total.shape <- ggplot(data = Pace_Shape_HFD, aes(x = cohort, y = shape.Total,color=country,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  geom_text(data = Pace_Shape_HFD[,list(cohort = max(cohort),
                                        shape.Total = shape.Total[which.max(cohort)],
                                        ccf = ccf[which.max(cohort)]),
                                  by = list(country)],
            aes(x=cohort + 2,y = shape.Total,label = country,color=country),
            show.legend = F,angle = 0, size = 5)+
  guides(color=FALSE)+
  scale_x_continuous(expression(Cohort),limits = c(1920,1965))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_discrete(name = 'Country')+
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  ggtitle('Shape of human fertility, cohort (Total)',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig.total.shape

Fig.shape1 <- ggplot(data = Pace_Shape_HFD, aes(x = cohort, y = shape.B1,color=country,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  geom_text(data = Pace_Shape_HFD[,list(cohort = max(cohort),
                                        shape.B1 = shape.B1[which.max(cohort)],
                                        ccf = ccf[which.max(cohort)]),
                                  by = list(country)],
            aes(x=cohort + 2,y = shape.B1,label = country,color=country),
            show.legend = F,angle = 0, size = 5)+
  guides(color=FALSE)+
  scale_x_continuous(expression(Cohort),limits = c(1920,1965))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_discrete(name = 'Country')+
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  ggtitle('Shape of human fertility, cohort (birth 1)',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig.shape1

Fig.shape2 <- ggplot(data = Pace_Shape_HFD, aes(x = cohort, y = shape.B2,color=country,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  geom_text(data = Pace_Shape_HFD[,list(cohort = max(cohort),
                                        shape.B2 = shape.B2[which.max(cohort)],
                                        ccf = ccf[which.max(cohort)]),
                                  by = list(country)],
            aes(x=cohort + 2,y = shape.B2,label = country,color=country),
            show.legend = F,angle = 0, size = 5)+
  guides(color=FALSE)+
  scale_x_continuous(expression(Cohort),limits = c(1920,1965))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_discrete(name = 'Country')+
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  ggtitle('Shape of human fertility, cohort (birth 2)',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig.shape2

Fig.shape3 <- ggplot(data = Pace_Shape_HFD, aes(x = cohort, y = shape.B3,color=country,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  geom_text(data = Pace_Shape_HFD[,list(cohort = max(cohort),
                                        shape.B3 = shape.B3[which.max(cohort)],
                                        ccf = ccf[which.max(cohort)]),
                                  by = list(country)],
            aes(x=cohort + 2,y = shape.B3,label = country,color=country),
            show.legend = F,angle = 0, size = 5)+
  guides(color=FALSE)+
  scale_x_continuous(expression(Cohort),limits = c(1920,1965))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_discrete(name = 'Country')+
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  ggtitle('Shape of human fertility, cohort (birth 3)',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig.shape3

Fig.shape4 <- ggplot(data = Pace_Shape_HFD, aes(x = cohort, y = shape.B4,color=country,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  geom_text(data = Pace_Shape_HFD[,list(cohort = max(cohort),
                                        shape.B4 = shape.B4[which.max(cohort)],
                                        ccf = ccf[which.max(cohort)]),
                                  by = list(country)],
            aes(x=cohort + 2,y = shape.B4,label = country,color=country),
            show.legend = F,angle = 0, size = 5)+
  guides(color=FALSE)+
  scale_x_continuous(expression(Cohort),limits = c(1920,1965))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_discrete(name = 'Country')+
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  ggtitle('Shape of human fertility, cohort (birth 4)',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig.shape4

pdf(file="Figures/Figs_Shape_CohortV4_parity.pdf",width=15,height=13,pointsize=4)
Fig.total.shape
Fig.shape1
Fig.shape2
Fig.shape3
Fig.shape4
dev.off()



# make preliminary figure by country
Fig.total.shape.country <- ggplot(data = Pace_Shape_HFD, aes(x = cohort, y = shape.Total,color=country,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  guides(color=FALSE)+
  facet_wrap(~country)+
  scale_x_continuous(expression(Cohort),limits = c(1920,1965))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_discrete(name = 'Country')+
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  ggtitle('Shape of human fertility, cohort (Total)',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15), legend.position = c(.8,.1))
previous_theme <- theme_set(theme_bw())
Fig.total.shape.country

Fig.shape1.country <- ggplot(data = Pace_Shape_HFD, aes(x = cohort, y = shape.B1,color=country,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  guides(color=FALSE)+
  facet_wrap(~country)+
  scale_x_continuous(expression(Cohort),limits = c(1920,1965))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_discrete(name = 'Country')+
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  ggtitle('Shape of human fertility, cohort (birth 1)',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15), legend.position = c(.8,.1))
previous_theme <- theme_set(theme_bw())
Fig.shape1.country

Fig.shape2.country <- ggplot(data = Pace_Shape_HFD, aes(x = cohort, y = shape.B2,color=country,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  guides(color=FALSE)+
  facet_wrap(~country)+
  scale_x_continuous(expression(Cohort),limits = c(1920,1965))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_discrete(name = 'Country')+
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  ggtitle('Shape of human fertility, cohort (birth 2)',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15), legend.position = c(.8,.1))
previous_theme <- theme_set(theme_bw())
Fig.shape2.country

Fig.shape3.country <- ggplot(data = Pace_Shape_HFD, aes(x = cohort, y = shape.B3,color=country,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  guides(color=FALSE)+
  facet_wrap(~country)+
  scale_x_continuous(expression(Cohort),limits = c(1920,1965))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_discrete(name = 'Country')+
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  ggtitle('Shape of human fertility, cohort (birth 3)',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15), legend.position = c(.8,.1))
previous_theme <- theme_set(theme_bw())
Fig.shape3.country

Fig.shape4.country <- ggplot(data = Pace_Shape_HFD, aes(x = cohort, y = shape.B4,color=country,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  guides(color=FALSE)+
  facet_wrap(~country)+
  scale_x_continuous(expression(Cohort),limits = c(1920,1965))+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_discrete(name = 'Country')+
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  ggtitle('Shape of human fertility, cohort (birth 4)',subtitle = 'Data from HFD')+
  theme(text = element_text(size = 15), legend.position = c(.8,.1))
previous_theme <- theme_set(theme_bw())
Fig.shape4.country

pdf(file="Figures/Figs_Shape_CohortV4_parity_country.pdf",width=15,height=13,pointsize=4)
Fig.total.shape.country
Fig.shape1.country
Fig.shape2.country
Fig.shape3.country
Fig.shape4.country
dev.off()