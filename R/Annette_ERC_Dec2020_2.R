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
gdata::keep(Pace_Shape_HFD,sure = T)

#New indicators for cohort + 15
Pace_Shape_HFD[,cohort.15 := cohort + 15]

load('Data/HMD_Data.RData')

HMDLT[,ed.approx := sum(ex*dx/100000), by = .(Year,Sex,PopName)]

mort <- HMDLT[Age == 0 & Sex == 'f',c('Year','PopName','ex','ed.approx')]

colnames(mort) <- c('cohort.15','country','ex','ed')

fert <- Pace_Shape_HFD[,c('country','cohort','cohort.15','shape.Total','pace.Total','ccf')]

#merge fertility with mortality 

fert.mort <- merge(fert,mort,by = c('cohort.15','country'))


# make preliminary figure
Fig.1.total <- ggplot(data = fert.mort, aes(x = ex, y = pace.Total,color=country,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  #facet_wrap(~country)+
  scale_x_continuous('Life expectancy')+
  scale_size_continuous(name = 'Cumulative fertility')+
  #scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous('Birth expectancy')+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Fig 1 Total')+
  theme(text = element_text(size = 15))
previous_theme <- theme_set(theme_bw())
Fig.1.total

Fig.1.total.country <- ggplot(data = fert.mort, aes(x = ex, y = pace.Total,color=cohort,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  facet_wrap(~country)+
  scale_x_continuous('Life expectancy')+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous('Birth expectancy')+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Fig 1 Countries')+
  theme(text = element_text(size = 15))
Fig.1.total.country


### fig 2
# make preliminary figure
Fig.2.total <- ggplot(data = fert.mort, aes(x = ex, y = shape.Total,color=country,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  #facet_wrap(~country)+
  scale_x_continuous('Life expectancy')+
  scale_size_continuous(name = 'Cumulative fertility')+
  #scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous('Birth-entropy')+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Fig 2 Total')+
  theme(text = element_text(size = 15))
Fig.2.total

Fig.2.total.country <- ggplot(data = fert.mort, aes(x = ex, y = shape.Total,color=cohort,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  facet_wrap(~country)+
  scale_x_continuous('Life expectancy')+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous('Birth-entropy')+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Fig 2 Countries')+
  theme(text = element_text(size = 15))
Fig.2.total.country

### fig 3
# make preliminary figure
Fig.3.total <- ggplot(data = fert.mort, aes(x = ed/ex, y = pace.Total,color=country,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  #facet_wrap(~country)+
  scale_x_continuous('Lifetable entropy')+
  scale_size_continuous(name = 'Cumulative fertility')+
  #scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous('Birth expectancy')+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Fig 3 Total')+
  theme(text = element_text(size = 15))
Fig.3.total

Fig.3.total.country <- ggplot(data = fert.mort, aes(x = ed/ex, y = pace.Total,color=cohort,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  facet_wrap(~country)+
  scale_x_continuous('Lifetable entropy')+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous('Birth expectancy')+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Fig 3 Countries')+
  theme(text = element_text(size = 15))
Fig.3.total.country


### fig 4
# make preliminary figure
Fig.4.total <- ggplot(data = fert.mort, aes(x = ed/ex, y = shape.Total,color=country,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  #facet_wrap(~country)+
  scale_x_continuous('Lifetable entropy')+
  scale_size_continuous(name = 'Cumulative fertility')+
  #scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous('Birth-entropy')+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Fig 4 Total')+
  theme(text = element_text(size = 15))
Fig.4.total

Fig.4.total.country <- ggplot(data = fert.mort, aes(x = ed/ex, y = shape.Total,color=cohort,size = ccf)) +
  geom_point(alpha=I(1/1.5),shape=16,show.legend = T)+
  facet_wrap(~country)+
  scale_x_continuous('Lifetable entropy')+
  scale_size_continuous(name = 'Cumulative fertility')+
  scale_color_viridis(discrete=F,option = 'C',name = c('Cohort')) +
  scale_y_continuous('Birth-entropy')+
  theme(legend.key.height=unit(2,"line"))+
  ggtitle('Fig 4 Countries')+
  theme(text = element_text(size = 15))
Fig.4.total.country



pdf(file="Figures/Figs_Pace_Shape_ERC_AB_2.pdf",width=15,height=13,pointsize=4)
Fig.1.total
Fig.1.total.country
Fig.2.total
Fig.2.total.country 
Fig.3.total
Fig.3.total.country 
Fig.4.total
Fig.4.total.country 
dev.off()

