# set the working directories for our files
setwd('D:/Remote sensing/Remote sensing')
library(raster)

# set the extent
e<-extent(838332.7134,862990.3004,-51152.68162,-22688.83612)

#load the raster files
b1<- raster('Band 1.tif')
b2<- raster('Band 2.tif')
b3<- raster('Band 3.tif')
b4<- raster('Band 4.tif')
b5<- raster('Band 5.tif')
b6<- raster('Band 6.tif')
b7<- raster('Band 7.tif')

landsat<-stack(b1,b2,b3,b4,b5,b6,b7)
landsatcrop=crop(landsat,e)
plotRGB(landsat,main="TRUE COLOUR COMPOSITE",stretch='lin')

png(filename='landsat.png',width=480,height=480)
plotRGB(landsat,main="TRUE COLOUR COMPOSITE",stretch='lin')
dev.off()


#LANDSAT
#true color composite LANDSAT
png(filename="truecolcrop.png", width=480,height=480)
truecol<-stack(b4,b3,b2)
lantrue=crop(truecol,e)
plotRGB(lantrue,axes=TRUE,stretch='lin',main="TRUE COLOUR COMPOSITE")
dev.off()

#false colour composite LANDSAT
png(filename='falsecol.png',width=480,height=480)
falsecol<-stack(b6,b4,b3)
lanfalse=crop(falsecol,e)
plotRGB(lanfalse,axes=TRUE,stretch='lin',main="False colour composite")
dev.off()

ndwi<-(landsatcrop[[3]]-landsatcrop[[5]])/(landsatcrop[[3]]+landsatcrop[[5]])

#convert ndwi to a matrix that we are going to use in the classification
nr<-getValues(ndwi)
str(nr)

#It is important to set the seed generator because KMEANS initiates the centers in random locations
set.seed(99)
#create 10 clusters, 20 iterations,starting with 5 random sets using 'Lloyd' algorithm
kmncluster<-kmeans(na.omit(nr),centers=10, iter.max=500,nstart=5,algorithm='Lloyd')
str(kmncluster)

#use the ndwi object to set the cluster values to a new cluster
knr<-setValues(ndwi,kmncluster$cluster)
#you can also do it like this
knr<-raster(ndwi)
values(knr)<-kmncluster$cluster
knr
# Define a color vector for 10 clusters (learn more about setting the color later)
mycolor <- c("#fef65b","#ff0000", "#daa520","#0000ff","#0000ff","#00ff00","#cbbeb5",
             "#c3ff5b", "#ff7373", "#00ff00", "#808080")

#save the image as a png
png(filename="unsupervisedclassification.png",width=720,height=720)
plot(knr, main = 'Unsupervised classification',col = mycolor, axes=FALSE)
dev.off()

#read CSV file
data<-read.csv(file = 'Sample_Points_coords.csv')
plot(knr, main='Unsupervised classification',col=mycolor, axes=FALSE)
points(data$X,data$Y,pch=19)


writeraster(classified,file="")

