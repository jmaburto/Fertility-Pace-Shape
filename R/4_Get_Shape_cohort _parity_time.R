#devtools::install_github("jonesor/Rage")
#install.packages("remotes") # smaller and quicker to install than devtools
#remotes::install_github("iainmstott/Rage@pr51")
library(Rage)
library(data.table)
library(ggplot2)
library(viridis)
library(colorspace)

#?getDTthreads

# load fertility data
load('Data/HFD_Data.RData')

HFD <- merge(HFD.cohort.asfr.parity, HFD.cohort.birth.parity, by = c('country','cohort','age'))

# take out non-completed cohort
HFD <- HFD[, select.cohort:= ifelse(is.na(sum(total.births)),'no','yes'), by = list(country,cohort)]
HFD <- HFD[select.cohort == 'yes']

# we have consistent informatin with completed cohorts from age 12 to age 55
HFD[, list(min.age = min(age), max.age = max(age)), by = list(country,cohort)]

HFD[,region:= ifelse(country %in% c('BGR','BLR','CZE','HUN','SVK'),'Central Europe',
                     ifelse(country %in% c('CAN','USA'),'North America',
                            ifelse(country %in% c('DNK','EST','NLD','NOR','SWE'),'Northern Europe',
                                   ifelse(country %in% c('JPN'),'Asia',
                                          ifelse(country %in% c('PRT'),'Southern Europe',
                                                 ifelse(country %in% c('LTU','POL','RUS','UKR'),'Eastern Europe','No region'))))))]

unique(HFD$region)

#how many births
b1 <- 1

Pace_Shape_HFD <- HFD[,list(shape.Total = shape_rep(rep = data.frame(cbind(x =(min(age[which(total.births > b1)]):max(age[which(total.births>b1)]))), 
                                                               mx = asfr[((min(age[which(total.births > b1)]):(max(age[which(total.births>b1)]+1)))-min(age[which(total.births > b1)])+1)[-1]],
                                                               xmin = min(age[which(total.births>b1)]), 
                                                               xmax = max(age[which(total.births>b1)]),
                                                               fertTable = F)), 
                            pace.Total = pace_rep(rep = data.frame(cbind(x =(min(age[which(total.births > b1)]):max(age[which(total.births>b1)]))), 
                                                             mx = asfr[((min(age[which(total.births > b1)]):(max(age[which(total.births>b1)]+1)))-min(age[which(total.births > b1)])+1)[-1]],
                                                             xmin = min(age[which(total.births>b1)]), 
                                                             xmax = max(age[which(total.births>b1)]),
                                                             fertTable = F)),
                            shape.B1 = shape_rep(rep = data.frame(cbind(x =(min(age[which(birth1 > b1)]):max(age[which(birth1>b1)]))), 
                                                                     mx = asfr1[((min(age[which(birth1 > b1)]):(max(age[which(birth1>b1)]+1)))-min(age[which(birth1 > b1)])+1)[-1]],
                                                                     xmin = min(age[which(birth1>b1)]), 
                                                                     xmax = max(age[which(birth1>b1)]),
                                                                     fertTable = F)), 
                            pace.B1 = pace_rep(rep = data.frame(cbind(x =(min(age[which(birth1 > b1)]):max(age[which(birth1>b1)]))), 
                                                                   mx = asfr1[((min(age[which(birth1 > b1)]):(max(age[which(birth1>b1)]+1)))-min(age[which(birth1 > b1)])+1)[-1]],
                                                                   xmin = min(age[which(birth1>b1)]), 
                                                                   xmax = max(age[which(birth1>b1)]),
                                                                   fertTable = F)),
                            shape.B2 = shape_rep(rep = data.frame(cbind(x =(min(age[which(birth2 > b1)]):max(age[which(birth2>b1)]))), 
                                                                  mx = asfr2[((min(age[which(birth2 > b1)]):(max(age[which(birth2>b1)]+1)))-min(age[which(birth2 > b1)])+1)[-1]],
                                                                  xmin = min(age[which(birth2>b1)]), 
                                                                  xmax = max(age[which(birth2>b1)]),
                                                                  fertTable = F)), 
                            pace.B2 = pace_rep(rep = data.frame(cbind(x =(min(age[which(birth2 > b1)]):max(age[which(birth2>b1)]))), 
                                                                mx = asfr2[((min(age[which(birth2 > b1)]):(max(age[which(birth2>b1)]+1)))-min(age[which(birth2 > b1)])+1)[-1]],
                                                                xmin = min(age[which(birth2>b1)]), 
                                                                xmax = max(age[which(birth2>b1)]),
                                                                fertTable = F)),
                            shape.B3 = shape_rep(rep = data.frame(cbind(x =(min(age[which(birth3 > b1)]):max(age[which(birth3>b1)]))), 
                                                                  mx = asfr3[((min(age[which(birth3 > b1)]):(max(age[which(birth3>b1)]+1)))-min(age[which(birth3 > b1)])+1)[-1]],
                                                                  xmin = min(age[which(birth3>b1)]), 
                                                                  xmax = max(age[which(birth3>b1)]),
                                                                  fertTable = F)), 
                            pace.B3 = pace_rep(rep = data.frame(cbind(x =(min(age[which(birth3 > b1)]):max(age[which(birth3>b1)]))), 
                                                                mx = asfr3[((min(age[which(birth3 > b1)]):(max(age[which(birth3>b1)]+1)))-min(age[which(birth3 > b1)])+1)[-1]],
                                                                xmin = min(age[which(birth3>b1)]), 
                                                                xmax = max(age[which(birth3>b1)]),
                                                                fertTable = F)),
                            shape.B4 = shape_rep(rep = data.frame(cbind(x =(min(age[which(birth4 > b1)]):max(age[which(birth4>b1)]))), 
                                                                  mx = asfr4[((min(age[which(birth4 > b1)]):(max(age[which(birth4>b1)]+1)))-min(age[which(birth4 > b1)])+1)[-1]],
                                                                  xmin = min(age[which(birth4>b1)]), 
                                                                  xmax = max(age[which(birth4>b1)]),
                                                                  fertTable = F)), 
                            pace.B4 = pace_rep(rep = data.frame(cbind(x =(min(age[which(birth4 > b1)]):max(age[which(birth4>b1)]))), 
                                                                mx = asfr4[((min(age[which(birth4 > b1)]):(max(age[which(birth4>b1)]+1)))-min(age[which(birth4 > b1)])+1)[-1]],
                                                                xmin = min(age[which(birth4>b1)]), 
                                                                xmax = max(age[which(birth4>b1)]),
                                                                fertTable = F)),
                            alphaT = min(age[which(total.births>b1)]),
                            alpha1 = min(age[which(birth1>b1)]),
                            alpha2 = min(age[which(birth2>b1)]),
                            alpha3 = min(age[which(birth3>b1)]),
                            alpha4 = min(age[which(birth4>b1)]),
                            tau.total = max(age[which(total.births>b1)]) - min(age[which(total.births>b1)]),
                            tau1 = max(age[which(birth1>b1)]) - min(age[which(birth1>b1)]),
                            tau2 = max(age[which(birth2>b1)]) - min(age[which(birth2>b1)]),
                            tau3 = max(age[which(birth3>b1)]) - min(age[which(birth3>b1)]),
                            tau4 = max(age[which(birth4>b1)]) - min(age[which(birth4>b1)]),
                            ccf = sum(asfr)), 
                      by = list(region,country,cohort)]


range(c(Pace_Shape_HFD$pace.Total,Pace_Shape_HFD$pace.B1,Pace_Shape_HFD$pace.B2,Pace_Shape_HFD$pace.B3,Pace_Shape_HFD$pace.B4))

range(c(Pace_Shape_HFD$shape.Total,Pace_Shape_HFD$shape.B1,Pace_Shape_HFD$shape.B2,Pace_Shape_HFD$shape.B3,Pace_Shape_HFD$shape.B4))

Pace_Shape_HFD$cohort.birth.T <- Pace_Shape_HFD$cohort + Pace_Shape_HFD$alphaT

Pace_Shape_HFD$cohort.birth.1 <- Pace_Shape_HFD$cohort + Pace_Shape_HFD$alpha1

Pace_Shape_HFD$cohort.birth.2 <- Pace_Shape_HFD$cohort + Pace_Shape_HFD$alpha2

Pace_Shape_HFD$cohort.birth.3 <- Pace_Shape_HFD$cohort + Pace_Shape_HFD$alpha3

Pace_Shape_HFD$cohort.birth.4 <- Pace_Shape_HFD$cohort + Pace_Shape_HFD$alpha4


#### adjust pace to read as age
Pace_Shape_HFD$age.pace.T <- Pace_Shape_HFD$pace.Total + Pace_Shape_HFD$alphaT

Pace_Shape_HFD$age.pace.1 <- Pace_Shape_HFD$pace.B1 + Pace_Shape_HFD$alpha1

Pace_Shape_HFD$age.pace.2 <- Pace_Shape_HFD$pace.B2 + Pace_Shape_HFD$alpha2

Pace_Shape_HFD$age.pace.3 <- Pace_Shape_HFD$pace.B3 + Pace_Shape_HFD$alpha3

Pace_Shape_HFD$age.pace.4 <- Pace_Shape_HFD$pace.B4 + Pace_Shape_HFD$alpha4
  


range(c(Pace_Shape_HFD$pace.Total,Pace_Shape_HFD$pace.B1,Pace_Shape_HFD$pace.B2,Pace_Shape_HFD$pace.B3,Pace_Shape_HFD$pace.B4))

range(c(Pace_Shape_HFD$shape.Total,Pace_Shape_HFD$shape.B1,Pace_Shape_HFD$shape.B2,Pace_Shape_HFD$shape.B3,Pace_Shape_HFD$shape.B4))

max(Pace_Shape_HFD$cohort)
min(Pace_Shape_HFD$cohort)

# Take care of the legends later
hcl_palettes("sequential (multi-hue)", n = 18, plot = TRUE)
cols <- diverging_hcl(n = 18, h = c(300, 128), c = c(30, 65), l = c(20, 95), power = c(1, 1.4), register = "Custom-Palette")
cols <- c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA",
          "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", 
          "#AA4455", "#DD7788")
size.pic <- 3
size.text <- 15
range.size <- c(1, 3)
size.point <- 2
# make preliminary figure
Fig.total.shape <- ggplot(data = Pace_Shape_HFD, aes(x = cohort,
                                                     y = shape.Total,
                                                     color=region,
                                                     #size = ccf,
                                                     shape = country)) +
  geom_point(alpha=I(1/1.5),show.legend = F,size = size.point)+
  #guides(color=FALSE)+
  scale_color_manual(name = 'Region',values = cols[c(1,4,7,11,15,18)])+
  scale_shape_manual(values = rep(c(15:18),5))+
  scale_x_continuous('Reproductive cohort',limits = c(1920,1962))+
  scale_size_continuous(name = 'Cumulative fertility',range = range.size)+
  scale_y_continuous(expression(Shape),limits = c(-.22,.3))+
  ggtitle('Shape, total fertility')+
  theme(text = element_text(size = size.text))
previous_theme <- theme_set(theme_bw())
Fig.total.shape

Fig.shape1 <- ggplot(data = Pace_Shape_HFD, aes(x = cohort,
                                                y = shape.B1,
                                                color=region,
                                                #size = ccf,
                                                shape = country)) +
  geom_point(alpha=I(1/1.5),show.legend = F,size = size.point)+
  #guides(color=FALSE)+
  scale_color_manual(name = 'Region',values = cols[c(1,4,7,11,15,18)])+
  scale_shape_manual(values = rep(c(15:18),5))+
  scale_x_continuous('Reproductive cohort',limits = c(1920,1962))+
  scale_size_continuous(name = 'Cumulative fertility',range = range.size)+
  scale_y_continuous(expression(Shape),limits = c(-.22,.3))+
  ggtitle('Shape, birth 1')+
  theme(text = element_text(size = size.text))
previous_theme <- theme_set(theme_bw())
Fig.shape1

Fig.shape2 <- ggplot(data = Pace_Shape_HFD, aes(x = cohort,
                                                y = shape.B2,
                                                color=region,
                                                #size = ccf,
                                                shape = country)) +
  geom_point(alpha=I(1/1.5),show.legend = F,size = size.point)+
  #guides(color=FALSE)+
  scale_color_manual(name = 'Region',values = cols[c(1,4,7,11,15,18)])+
  scale_shape_manual(values = rep(c(15:18),5))+
  scale_x_continuous('Reproductive cohort',limits = c(1920,1962))+
  scale_size_continuous(name = 'Cumulative fertility',range = range.size)+
  scale_y_continuous(expression(Shape),limits = c(-.22,.3))+
  ggtitle('Shape, birth 2')+
  theme(text = element_text(size = size.text))
previous_theme <- theme_set(theme_bw())
Fig.shape2

Fig.shape3 <- ggplot(data = Pace_Shape_HFD, aes(x = cohort,
                                                y = shape.B3,
                                                color=region,
                                                #size = ccf,
                                                shape = country)) +
  geom_point(alpha=I(1/1.5),show.legend = F,size = size.point)+
  #guides(color=FALSE)+
  scale_color_manual(name = 'Region',values = cols[c(1,4,7,11,15,18)])+
  scale_shape_manual(values = rep(c(15:18),5))+
  scale_x_continuous('Reproductive cohort',limits = c(1920,1962))+
  scale_size_continuous(name = 'Cumulative fertility',range = range.size)+
  scale_y_continuous(expression(Shape),limits = c(-.22,.3))+
  ggtitle('Shape, birth 3')+
  theme(text = element_text(size = size.text))
previous_theme <- theme_set(theme_bw())
Fig.shape3

Fig.shape4 <- ggplot(data = Pace_Shape_HFD, aes(x = cohort,
                                                y = shape.B4,
                                                color=region,
                                                #size = ccf,
                                                shape = country)) +
  geom_point(alpha=I(1/1.5),show.legend = F,size = size.point)+
  #guides(color=FALSE)+
  scale_color_manual(name = 'Region',values = cols[c(1,4,7,11,15,18)])+
  scale_shape_manual(values = rep(c(15:18),5))+
  scale_x_continuous('Reproductive cohort',limits = c(1920,1962))+
  scale_size_continuous(name = 'Cumulative fertility',range = range.size)+
  scale_y_continuous(expression(Shape),limits = c(-.22,.3))+
  ggtitle('Shape, birth 4')+
  theme(text = element_text(size = size.text))
previous_theme <- theme_set(theme_bw())
Fig.shape4


####


Fig.total.pace <- ggplot(data = Pace_Shape_HFD, aes(x = cohort,
                                                    y = age.pace.T ,
                                                    color=region,
                                                    #size = ccf,
                                                    shape = country)) +
  geom_point(alpha=I(1/1.5),show.legend = F,size = size.point)+
  #guides(color=FALSE)+
  scale_color_manual(name = 'Region',values = cols[c(1,4,7,11,15,18)])+
  scale_shape_manual(values = rep(c(15:18),5))+
  scale_x_continuous('Reproductive cohort',limits = c(1920,1962))+
  scale_size_continuous(name = 'Cumulative fertility',range = range.size)+
  scale_y_continuous('Reproductive pace',limits = c(20,37))+
  ggtitle('Pace, total fertility')+
  theme(text = element_text(size = size.text))
previous_theme <- theme_set(theme_bw())
Fig.total.pace


Fig.pace1 <- ggplot(data = Pace_Shape_HFD, aes(x = cohort,
                                               y = age.pace.1,
                                               color=region,
                                               #size = ccf,
                                               shape = country)) +
  geom_point(alpha=I(1/1.5),show.legend = F,size = size.point)+
  #guides(color=FALSE)+
  scale_color_manual(name = 'Region',values = cols[c(1,4,7,11,15,18)])+
  scale_shape_manual(values = rep(c(15:18),5))+
  scale_x_continuous('Reproductive cohort',limits = c(1920,1962))+
  scale_size_continuous(name = 'Cumulative fertility',range = range.size)+
  scale_y_continuous('Reproductive pace',limits = c(20,37))+
  ggtitle('Pace, birth 1')+
  theme(text = element_text(size = size.text))
previous_theme <- theme_set(theme_bw())
Fig.pace1


Fig.pace2 <- ggplot(data = Pace_Shape_HFD, aes(x = cohort,
                                               y = age.pace.2,
                                               color=region,
                                               #size = ccf,
                                               shape = country)) +
  geom_point(alpha=I(1/1.5),show.legend = F,size = size.point)+
  #guides(color=FALSE)+
  scale_color_manual(name = 'Region',values = cols[c(1,4,7,11,15,18)])+
  scale_shape_manual(values = rep(c(15:18),5))+
  scale_x_continuous('Reproductive cohort',limits = c(1920,1962))+
  scale_size_continuous(name = 'Cumulative fertility',range = range.size)+
  scale_y_continuous('Reproductive pace',limits = c(20,37))+
  ggtitle('Pace, birth 2')+
  theme(text = element_text(size = size.text))
previous_theme <- theme_set(theme_bw())
Fig.pace2


Fig.pace3 <- ggplot(data = Pace_Shape_HFD, aes(x = cohort,
                                               y = age.pace.3,
                                               color=region,
                                               #size = ccf,
                                               shape = country)) +
  geom_point(alpha=I(1/1.5),show.legend = F,size = size.point)+
  #guides(color=FALSE)+
  scale_color_manual(name = 'Region',values = cols[c(1,4,7,11,15,18)])+
  scale_shape_manual(values = rep(c(15:18),5))+
  scale_x_continuous('Reproductive cohort',limits = c(1920,1962))+
  scale_size_continuous(name = 'Cumulative fertility',range = range.size)+
  scale_y_continuous('Reproductive pace',limits = c(20,37))+
  ggtitle('Pace, birth 3')+
  theme(text = element_text(size = size.text))
previous_theme <- theme_set(theme_bw())
Fig.pace3




Fig.pace4 <- ggplot(data = Pace_Shape_HFD, aes(x = cohort,
                                               y = age.pace.4,
                                               color=region,
                                               #size = ccf,
                                               shape = country)) +
  geom_point(alpha=I(1/1.5),show.legend = F,size = size.point)+
  #guides(color=FALSE)+
  scale_color_manual(name = 'Region',values = cols[c(1,4,7,11,15,18)])+
  scale_shape_manual(values = rep(c(15:18),5))+
  scale_x_continuous('Reproductive cohort',limits = c(1920,1962))+
  scale_size_continuous(name = 'Cumulative fertility',range = range.size)+
  scale_y_continuous('Reproductive pace',limits = c(20,37))+
  ggtitle('Pace, birth 4')+
  theme(text = element_text(size = size.text))
previous_theme <- theme_set(theme_bw())
Fig.pace4




######################################   Pace shape
Fig.total.pace.shape <- ggplot(data = Pace_Shape_HFD, aes(x = age.pace.T,
                                                          y = shape.Total,
                                                          color=region,
                                                          #size = ccf,
                                                          shape = country)) +
  geom_point(alpha=I(1/1.5),show.legend = F,size = size.point)+
  #guides(color=FALSE)+
  scale_color_manual(name = 'Region',values = cols[c(1,4,7,11,15,18)])+
  scale_shape_manual(values = rep(c(15:18),5))+
  scale_x_continuous('Reproductive pace',limits = c(20,37))+
  scale_size_continuous(name = 'Cumulative fertility',range = range.size)+
  scale_y_continuous(expression(Shape),limits = c(-.22,.3))+
  ggtitle('Pace-shape, total fertility')+
  theme(text = element_text(size = size.text))
previous_theme <- theme_set(theme_bw())
Fig.total.pace.shape

Fig.pace.shape1 <- ggplot(data = Pace_Shape_HFD, aes(x = age.pace.1,
                                                     y = shape.B1,
                                                     color=region,
                                                     #size = ccf,
                                                     shape = country)) +
  geom_point(alpha=I(1/1.5),show.legend = F,size = size.point)+
  #guides(color=FALSE)+
  scale_color_manual(name = 'Region',values = cols[c(1,4,7,11,15,18)])+
  scale_shape_manual(values = rep(c(15:18),5))+
  scale_x_continuous('Reproductive pace',limits = c(20,37))+
  scale_size_continuous(name = 'Cumulative fertility',range = range.size)+
  scale_y_continuous(expression(Shape),limits = c(-.17,.3))+
  ggtitle('Pace-shape, birth 1')+
  theme(text = element_text(size = size.text))
previous_theme <- theme_set(theme_bw())
Fig.pace.shape1


Fig.pace.shape2 <- ggplot(data = Pace_Shape_HFD, aes(x = age.pace.2,
                                                     y = shape.B2,
                                                     color=region,
                                                     #size = ccf,
                                                     shape = country)) +
  geom_point(alpha=I(1/1.5),show.legend = F,size = size.point)+
  #guides(color=FALSE)+
  scale_color_manual(name = 'Region',values = cols[c(1,4,7,11,15,18)])+
  scale_shape_manual(values = rep(c(15:18),5))+
  scale_x_continuous('Reproductive pace',limits = c(20,37))+
  scale_size_continuous(name = 'Cumulative fertility',range = range.size)+
  scale_y_continuous(expression(Shape),limits = c(-.22,.3))+
  ggtitle('Pace-shape, birth 2')+
  theme(text = element_text(size = size.text))
previous_theme <- theme_set(theme_bw())
Fig.pace.shape2



Fig.pace.shape3 <- ggplot(data = Pace_Shape_HFD, aes(x = age.pace.3,
                                                     y = shape.B3,
                                                     color=region,
                                                     #size = ccf,
                                                     shape = country)) +
  geom_point(alpha=I(1/1.5),show.legend = F,size = size.point)+
  #guides(color=FALSE)+
  scale_color_manual(name = 'Region',values = cols[c(1,4,7,11,15,18)])+
  scale_shape_manual(values = rep(c(15:18),5))+
  scale_x_continuous('Reproductive pace',limits = c(20,37))+
  scale_size_continuous(name = 'Cumulative fertility',range = range.size)+
  scale_y_continuous(expression(Shape),limits = c(-.22,.3))+
  ggtitle('Pace-shape, birth 3')+
  theme(text = element_text(size = size.text))
previous_theme <- theme_set(theme_bw())
Fig.pace.shape3



Fig.pace.shape4 <- ggplot(data = Pace_Shape_HFD, aes(x = age.pace.4,
                                                     y = shape.B4,
                                                     color=region,
                                                     #size = ccf,
                                                     shape = country)) +
  geom_point(alpha=I(1/1.5),show.legend = F,size = size.point)+
  #guides(color=FALSE)+
  scale_color_manual(name = 'Region',values = cols[c(1,4,7,11,15,18)])+
  scale_shape_manual(values = rep(c(15:18),5))+
  scale_x_continuous('Reproductive pace',limits = c(20,37))+
  scale_size_continuous(name = 'Cumulative fertility',range = range.size)+
  scale_y_continuous(expression(Shape),limits = c(-.22,.3))+
  ggtitle('Pace-shape, birth 4')+
  theme(text = element_text(size = size.text))
previous_theme <- theme_set(theme_bw())
Fig.pace.shape4


lay.FigbOX <- rbind(c(1,2,3),
                    c(4,5,6),
                    c(7,8,9),
                    c(10,11,12),
                    c(13,14,15))

require(gridExtra)

pdf(file="Figures/Figure_MultipanelV3.pdf",width=12,height=18,pointsize=4)
grid.arrange(Fig.total.pace,Fig.total.shape,Fig.total.pace.shape,
             Fig.pace1,Fig.shape1,Fig.pace.shape1,
             Fig.pace2,Fig.shape2,Fig.pace.shape2,
             Fig.pace3,Fig.shape3,Fig.pace.shape3,
             Fig.pace4,Fig.shape4,Fig.pace.shape4,
             layout_matrix = lay.FigbOX)
dev.off()



Fig.legend <-  ggplot(data = Pace_Shape_HFD, aes(x = cohort,
                                                 y = shape.Total,
                                                 color=region,
                                                 #size = ccf,
                                                 shape = country)) +
  geom_point(alpha=I(1/1.5),show.legend = T,size = size.point)+
  #guides(color=FALSE)+
  scale_color_manual(name = 'Region',values = cols[c(1,4,7,11,15,18)])+
  scale_shape_manual(values = rep(c(15:18),5))+
  scale_x_continuous('Reproductive cohort',limits = c(1920,1962))+
  scale_size_continuous(name = 'Cumulative fertility',range = range.size)+
  scale_y_continuous(expression(Shape),limits = c(-.22,.3))+
  ggtitle('Shape, total fertility')+
  theme(text = element_text(size = size.text))
previous_theme <- theme_set(theme_bw())
Fig.legend

pdf(file="Figures/lGEND1 .pdf",width=12,height=18,pointsize=4)
grid.arrange(Fig.legend,Fig.total.shape,Fig.total.pace.shape,
             Fig.pace1,Fig.shape1,Fig.pace.shape1,
             Fig.pace2,Fig.shape2,Fig.pace.shape2,
             Fig.pace3,Fig.shape3,Fig.pace.shape3,
             Fig.pace4,Fig.shape4,Fig.pace.shape4,
             layout_matrix = lay.FigbOX)
dev.off()


pdf(file="Figures/legend 2 .pdf",width=12,height=18,pointsize=4)
Fig.legend
dev.off()


unique(HFD$country)
