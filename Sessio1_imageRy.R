#ImageRy package: how to

#DATA VISUALIZZATION
devtools::install_github("clauswilke/colorblindr")
devtools::install_github("ducciorocchini/cblindplot", force=T)
devtools::install_github("ducciorocchini/imageRy", force=T)

install.packages(c("patchwork","colorblindcheck","rasterdiv",
                   "ggridges","sdm"))
                 
library(ggplot2)
library(patchwork)
library(terra)
library(colorblindcheck)
library(colorblindr)
library(viridis)
library(rasterdiv)
library(ggridges)
library(tidyverse)
library(sdm)
library(raster)
library(cblindplot)


#list data
im.list()

b2<-im.import("sentinel.dolomites.b2.tif")
plot(b2,col=inferno(100))# color visualizzation change for colorblind

sent<-im.import("sentinel.dolomites") #all band toghether

#b2=blue
#b3=green
#b4=red
#b8=NIR

im.plotRGB(sent,r=3, g=2, b=1, 'natural color') #based on the order in which layer are stack
im.plotRGB(sent,r=4, g=3, b=2, 'false color')#enphasis on NIR 


#calculation of NDVI
 #First, dvi
#8bit image
#0-255
#dvi=nir-red
dvi_sent<-im.dvi(sent, 4,3 )
ndvi_sent<-im.ndvi(sent, 4,3)

#multitemporal visualization

mato2006<-im.import("matogrosso_ast_2006209_lrg.jpg")
mato1992<-im.import("matogrosso_l5_1992219_lrg.jpg")
ndvi1992<-im.ndvi(mato1992,1,2)
ndvi1992<-im.ndvi(mato2006,1,2)

#correlation
pairs(mato1992)
pairs(mato2006)

#import data
 forest <- rast("c:/Users/MENEGALDO ANDREA/OneDrive - Scientific Network South Tyrol (1)/amazon_deforestation_20000730_lrg.jpg")
 ext(forest) <- c(100, 150, 100, 150) 
 ext_crop <- ext(120, 140, 110, 130)  
 crop_forest <- crop(forest, ext_crop)

#classification
mato2006c<-im.classify(mato2006, num_cluster=2)
names(mato2006c)<-c('Forest', 'No forest')# da correggere

fre2006<-freq(mato2006c) #frequency of pixel (from which you can deduce proportion and percentage)

#copy dataset and ggplot line 

#how to represent change in time of ecosystemù
library(terra)
library(imageRy)

EN01<-im.import("EN_01.png")
EN13<-im.import("EN_13.png")

par(mfrow=c(1,2))# two plot in the same table
plot(EN01)
plot(EN13)

dev.off
plot(EN01)

diffEN<-EN01[[1]]-EN13[[1]]

#####-Greenland
gr<-im.import("greenland")
grt<-c(gr[[1]],gr[[4]])
diffgr<-gr[[1]]-gr[[4]]

#biomass over the alpine area
ndvi<- im.import("Sentinel2_NDVI_")
names(ndvi)<-c("02_Feb", "05_May", "08_Aug", "11_Nov")
  #ridgeline plots
im.ridgeline(ndvi,scale=1, palette="viridis")
im.ridgeline(ndvi,scale=2, palette="viridis")

#rgb scheme
im.plotRGB(ndvi,1,2,3)
#X11()
setwd("C:/Users/MENEGALDO ANDREA/OneDrive - Scientific Network South Tyrol (1)")
png("ndvi.png")

#spatial variability
library(imageRy)
library(terra)
libray(rasterdiv)
library(viridis)

im.list()
sent<-im.import("sentinel.png")
im.plotRGB(sent, 1,2,3)

nir<- sent[[1]]
sd3<-focal(nir,matrix(1/9,3,3),fun=sd) #creazione moving window
var3<-focal(nir,matrix(1/9,3,3),fun=var)

#Shannon entropy
ext<-c(0,50,0,50)
nir_crop<-crop(nir, ext)

#b=0.8 , g=0.2
#H=(0.8*log(0.8+))+(0.2*log(0.2))

shan3<-Shannon(nir_crop, window=3)
rao3<-paRao(nircrop, window=3, 
            alpha=2#to transform the original rao formula?
           )

#colorblindess correlation

# SIMULATION
library(imageRy)
library(terra)

im.list()

sentdol<-im.import("sentinel.dolomites")

ndvi<-im.ndvi(sentdol,4,3) #nir-red band

clgr<-colorRampPalette(c("black", "darkgrey","lightgrey"))(100)
plot(ndvi,col=clgr)

clbr<-colorRampPalette(c("blue", "white","red"))(100)
plot(ndvi,col=clbr)

clbg<-colorRampPalette(c("brown", "yellow","green"))(100)
plot(ndvi,col=clbg)

par(mfrow,c=(2,2))
plot(ndvi,col=clgr)
plot(ndvi,col=clbr)
plot(ndvi,col=clbg)
plot(ndvi)
dev.off()

##simulation of color vision deficiency(CVD)

palraw <- colorRampPalette(c("red", "orange", "red", "chartreuse", "cyan",
                             "blue"))(100)
palraw_grey <- colorRampPalette(c("dark orange", "orange", "grey", "dark grey",
                                  "light grey", "blue"))(100)

# Plot
plot(ndvi, col=palraw)
plot(ndvi, col=palraw_grey)


setwd("c:/Users/MENEGALDO ANDREA/OneDrive - Scientific Network South Tyrol (1)/")
vinicunca<-rast("vinicunca.jpg")
viniflip<-flip(vinicunca) #to flip image upside down

par(mfrow=c(1,2))
im.plotRGB(vinicunca, 1,2,3, title="Standard vision")
im.plotRGB(vinicunca, 2,2,3, title="Protanopia")
dev.off()

palraw_grey <- colorRampPalette(c("light blue", "darkgoldenrod", "dark grey",
"white", "light blue"))(100)
 plot(vinicunca[[1]], col=palraw_grey)


#### SPECIES DISTRIBUTION MODEL sdm package #####
file<-system.file("external/species.shp", package= "sdm")
species<-shapefile(file)

# looking at the occurrences (SQL query: select, from, where)
pres<-species[species$Occurrence == 1,]

# copy and then write 
plot(species[species$Occurrence == 1,],col='blue',pch=16)
points(species[species$Occurrence == 0,],col='red',pch=16)

#list the predictor
path <- system.file("external", package="sdm") 
lst <- list.files(path=path,pattern='asc$',full.names = T) #
lst

# stack
preds <- stack(lst)

# plot preds
plot(preds, col=viridis(100))

# plot predictors and occurrences
plot(preds$elevation, col=viridis(100), main="elevation")
points(species[species$Occurrence == 1,], pch=16, col="red")

plot(preds$temperature, col=viridis(100), main="temperature")
points(species[species$Occurrence == 1,], pch=16)

plot(preds$precipitation, col=viridis(100), main="precipitation")
points(species[species$Occurrence == 1,], pch=16)

plot(preds$vegetation, col=viridis(100), main="vegetation")
points(species[species$Occurrence == 1,], pch=16)


# model

# set the data for the sdm
datasdm <- sdmData(train=species, predictors=preds)

# model
m1 <- sdm(Occurrence ~ elevation + precipitation + temperature + vegetation, data=datasdm, methods = "glm")

# make the raster output layer
p1 <- predict(m1, newdata=preds) 

# plot the output
plot(p1, col=viridis(100))
points(species[species$Occurrence == 1,], pch=16)


