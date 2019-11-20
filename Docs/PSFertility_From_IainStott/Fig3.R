### WORKING DIRECTORY ##########################################################

# setwd("path/to/folder")



### PACKAGES ###################################################################
library(png)
library(fields)



### FUNCTIONS ##################################################################
source("Code/functions.R")



### DATA #######################################################################


## ALL SPECIES

alldatfiles <- list.files("Data/Splines_Lifetables", pattern = ".csv")
allsp <- sapply(alldatfiles, function(s) {
     strsplit(strsplit(s, ".", fixed = TRUE)[[1]][1], "_", fixed = TRUE)[[1]][1]
})
names(allsp) <- NULL

allspl <- lapply(alldatfiles, function(f){
     read.csv(paste("Data/Splines_Lifetables/", f, sep = ""))
})
names(allspl) <- allsp

# SHAPES
allS <- lapply(allspl, shape_rep)

# PACES
allP <- sapply(allspl, pace_rep)

# A to Z therefore 26 lines to be sure
allPscale <- c("years", "years", "years", "years", "years",
               "years",
               "days", "years", "days", "years", "years", "years", "years",
               "days", "days",

               "years", "years",
               "years", "years",
               "years", "years", "centuries", "years",
               
               
               
               "years", "years", "years",
               "days", "years", "months",
               
               "years", "years",
               "years", "years", "years", "years", "years", "years", "days", "years", "months",
               "years",
               "years", "years", "years",
               
               
               "years",
               "years"
               
               
               
               )
names(allPscale) <- allsp
# Scale by day, month, year, century
allPscaleval <- rep(1, length(allsp))
allPscaleval[allPscale %in% "days"] <- 1/365
allPscaleval[allPscale %in% "months"] <- 1/12
allPscaleval[allPscale %in% "centuries"] <- 100
names(allPscaleval) <- allsp
allP <- allP * allPscaleval


## OUR SPECIES

datfiles <- "Data"

# rododendron = flat1
rho <- "Rhododendron maximum"
rhostep <- "years"
rhospl <- read.csv(paste(datfiles, "/Splines_Lifetables/", rho, "_spl.csv", sep = ""))

# chamois = flat2
rup <- "Rupicapra rupicapra"
rupstep <- "years"
rupspl <- read.csv(paste(datfiles, "/Splines_Lifetables/", rup, "_spl.csv", sep = ""))

# crocodile = up2
cro <- "Crocodylus johnsoni"
crostep <- "years"
crospl <- read.csv(paste(datfiles, "/Splines_Lifetables/", cro, "_spl.csv", sep = ""))

# st. Johns wort = up1
hyp <- "Hypericum cumulicola"
hypstep <- "years"
hypspl <- read.csv(paste(datfiles, "/Splines_Lifetables/", hyp, "_spl.csv", sep = ""))

# swan = hump2
cyg <- "Cygnus olor"
cygstep <- "years"
cygspl <- read.csv(paste(datfiles, "/Splines_Lifetables/", cyg, "_spl.csv", sep = ""))

# fruit fly = down2
cer <- "Ceratitis capitata"
cerstep <- "years"
cerspl <- read.csv(paste(datfiles, "/Splines_Lifetables/", cer, "_spl.csv", sep = ""))

# nematode = down1
cae <- "Caenorhabditis elegans"
caestep <- "years"
caespl <- read.csv(paste(datfiles, "/Splines_Lifetables/", cae, "_spl.csv", sep = ""))

# guppy = hump1
poe <- "Poecilia reticulata"
poestep <- "years"
poespl <- read.csv(paste(datfiles, "/Splines_Lifetables/", poe, "_spl.csv", sep = ""))


## PICTURES
rhoPic <- readPNG("Data/Critters/blk/Rhododendron_maximum_sil_blk.png", native = TRUE)
rupPic <- readPNG("Data/Critters/blk/Rupicapra_rupicapra_sil_blk.png", native = TRUE)
croPic <- readPNG("Data/Critters/blk/Crocodylus_johnsoni_sil_blk.png", native = TRUE)
hypPic <- readPNG("Data/Critters/blk/Hypericum_cumulicola_sil_blk.png", native = TRUE)
cygPic <- readPNG("Data/Critters/blk/Cygnus_olor_sil_blk.png", native = TRUE)
cerPic <- readPNG("Data/Critters/blk/Ceratitis_capitata_sil_blk.png", native = TRUE)
caePic <- readPNG("Data/Critters/blk/Caenorhabditis_elegans_sil_blk.png", native = TRUE)
poePic <- readPNG("Data/Critters/blk/Poecilia_reticulata_sil_blk.png", native = TRUE)


## Shape and Pace_______________________________________________________________

# Shape values
rhoS <- shape_rep(rhospl) # Rhododendron
rupS <- shape_rep(rupspl) # Chamois
croS <- shape_rep(crospl) # Freshwater crocodile
hypS <- shape_rep(hypspl) # St. John's Wort
cygS <- shape_rep(cygspl) # Mute swan
cerS <- shape_rep(cerspl) # Mediterranean fruit fly
caeS <- shape_rep(caespl) # Nematode worm
poeS <- shape_rep(poespl) # Guppy
Shapes <- c(rhoS, rupS, croS, hypS, cygS, cerS, caeS, poeS)

#Pace values
rhoP <- pace_rep(rhospl) # Rhododendron
rupP <- pace_rep(rupspl) # Chamois
croP <- pace_rep(crospl) # Freshwater crocodile
hypP <- pace_rep(hypspl) # St. John's Wort
cygP <- pace_rep(cygspl) # Mute swan
cerP <- pace_rep(cerspl) * (1/365) # Mediterranean fruit fly
caeP <- pace_rep(caespl) * (1/365) # Nematode worm
poeP <- pace_rep(poespl) * (1/12) # Guppy
Paces <- c(rhoP, rupP, croP, hypP, cygP, cerP, caeP, poeP)



### PLOT #######################################################################

## Setup _______________________________________________________________________

win.graph(width = 4, height = 4) # windows only
par(mar = c(2, 2.75, 0.75, 0.75))

xl <- log(c(0.001, 1000))
yl <- c(-0.5, 0.5)
plot(xl, yl, type = "n", bty = "n", 
         xaxt = "n", yaxt = "n",
         xlab = "", ylab = "")
axis(side = 1, at = log(c(1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3)), 
               labels = c("0.001", "0.01", "0.1", "1", "10", "100", "1000"), 
               mgp=c(0, 0.3, 0),
               cex.axis = 0.8)
mtext(side = 1, "Pace", cex = 0.8, line = 1)
axis(side = 2, at = c(-0.5, -0.25, 0, 0.25, 0.5), 
               labels = c("-0.5", "-0.25", "0", "0.25", "0.5"), 
               mgp=c(0, 0.6, 0), las = 2,
               cex.axis = 0.8)
mtext(side = 2, "Shape", cex = 0.8, line = 1.75)
points(log(allP), allS, pch = 16, cex = 0.9,
       col = rgb(127, 127, 127, 150,maxColorValue = 255))
points(log(Paces), Shapes, pch = 16, col = "black", cex = 1.1)
points(log(Paces)[Shapes < -0.05], 
       Shapes[Shapes < -0.05], 
       pch = 16, col = rgb(64, 160, 209, 255, maxColorValue = 255),
       cex = 1.1)
points(log(Paces)[Shapes > 0.05], 
       Shapes[Shapes > 0.05], 
       pch = 16, col = rgb(209, 64, 76, 255, maxColorValue = 255),
       cex = 1.1)

rasterImage(rhoPic, log(100), 0.1, log(1000), 0.25)
lines(c(log(rhoP + 1.5), log(100 + 50)), c(rhoS + 0.01, 0.1 + 0.02), lty = 3)
rasterImage(poePic, log(2), 0.33, log(20), 0.48)
lines(c(log(poeP + 0.2), log(2 + 3)), c(poeS + 0.02, 0.33 + 0.02), lty = 3)
rasterImage(caePic, log(0.02), 0.42, log(0.2), 0.57)
lines(c(log(caeP + 0.0015), log(0.02 + 0.01)), c(caeS + 0.01, 0.42 + 0.03), lty = 3)
rasterImage(cerPic, log(0.001), 0, log(0.01), 0.15)
lines(c(log(cerP - 0.02), log(0.01 - 0.002)), c(cerS, 0.15 - 0.06), lty = 3)
rasterImage(rupPic, log(0.002), -0.25, log(0.02), -0.1)
lines(c(log(rupP - 1.2), log(0.02 + 0.001)), c(rupS, -0.1 - 0.06), lty = 3)
rasterImage(cygPic, log(0.02), -0.45, log(0.2), -0.3)
lines(c(log(cygP - 2), log(0.2 - 0.01)), c(cygS - 0.01, -0.3 - 0.04), lty = 3)
rasterImage(hypPic, log(1), -0.53, log(10), -0.38)
lines(c(log(hypP), log(hypP)), c(hypS - 0.02, -0.38 - 0.03), lty = 3)
rasterImage(croPic, log(100), -0.4, log(1000), -0.25)
lines(c(log(croP + 2), log(100 + 150)), c(croS - 0.01, -0.25 - 0.04), lty = 3)

